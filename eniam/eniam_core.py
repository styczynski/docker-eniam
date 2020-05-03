import subprocess
import sys
import importlib
DOCKER_IMAGE_NAME = "styczynski/eniam:0.1.1"

from base64 import b64encode
import time
import sys
from subprocess import PIPE, Popen
from threading  import Thread
import tempfile

try:
    from queue import Queue, Empty
except ImportError:
    from Queue import Queue, Empty  # python 2.x

ON_POSIX = 'posix' in sys.builtin_module_names

def enqueue_output(out, queue):
    for line in iter(out.readline, b''):
        queue.put(line)
    out.close()

def create_cmd(args_list):
    return " ".join(args_list)

def run_subsyntax(args=[], input=None, output=None, lexicon=None):

    lexicon_input = "empty" if lexicon is None else b64encode(lexicon.encode()).decode()
    cmd = create_cmd(["docker", "run", "-i", DOCKER_IMAGE_NAME, "/root/subsyntax.sh", lexicon_input] + args),
    return subprocess.Popen(
        cmd,
        stdin=input,
        stdout=output,
        stderr=output,
        shell=True,
        executable='/bin/bash',
        bufsize=1,
        close_fds=ON_POSIX,
    )


def run_lex(args=[], input=None, output=None):
    return subprocess.Popen(
        create_cmd(["docker", "run", "-i", DOCKER_IMAGE_NAME, "/root/print_lexicon"] + args),
        stdin=input,
        stdout=output,
        stderr=output,
        shell=True,
        executable='/bin/bash',
        bufsize=1,
        close_fds=ON_POSIX,
    )


def run_domparser(args=[], input=None, output=None, lexicon=None):
    lexicon_input = "empty" if lexicon is None else b64encode(lexicon.encode()).decode()
    return subprocess.Popen(
        create_cmd(["docker", "run", "-i", DOCKER_IMAGE_NAME, "/root/domparser.sh", lexicon_input] + args),
        stdin=input,
        stdout=output,
        stderr=output,
        shell=True,
        executable='/bin/bash',
        bufsize=1,
        close_fds=ON_POSIX,
    )


def csv_to_html_table(fname,headers=None,delimiter=","):
    with open(fname) as f:
        content = f.readlines()
    #reading file content into list
    rows = [x.strip() for x in content]
    table = "<table>"
    #creating HTML header row if header is provided
    if headers is not None:
        table+= "".join(["<th>"+cell+"</th>" for cell in headers.split(delimiter)])
    else:
        table+= "".join(["<th>"+cell+"</th>" for cell in rows[0].split(delimiter)])
        rows=rows[1:]
    #Converting csv to html row by row
    for row in rows:
        table+= "<tr>" + "".join(["<td>"+cell+"</td>" for cell in row.split(delimiter)]) + "</tr>" + "\n"
    table+="</table><br>"
    return table

class DomparserResult():
    def __init__(self, html_content, output_pdf):
        self.html_conent = html_content
        self.output_pdf = output_pdf
        self.output_pdf.seek(0)

    def show(self):
        display = importlib.import_module("IPython.core.display")
        display.display(display.HTML("\n".join(self.html_conent)))

    def html(self):
        return self.html_conent

    def chart(self):
        self.output_pdf.seek(0)
        WImage = importlib.import_module("wand.image").Image
        img = WImage(filename=self.output_pdf.name)
        return img

    def tables_chart(self):
        self.output_pdf.seek(0)
        tables = importlib.import_module("camelot").read_pdf(self.output_pdf.name)
        return tables

    def csv_chart_file(self):
        output_csv = tempfile.NamedTemporaryFile(suffix='.csv')
        tables = self.tables_chart()
        tables[0].to_csv(output_csv.name)
        output_csv.seek(0)
        return output_csv

    def csv_chart(self):
        f = self.csv_chart_file()
        output = f.read().decode()
        f.close()
        return output

    def html_chart(self):
        tables = self.tables_chart()
        content = tables[0].df.to_html()
        display = importlib.import_module("IPython.core.display")
        display.display(display.HTML(content))


class SubsyntaxResult():
    def __init__(self, html_content):
        self.html_conent = html_content

    def show(self):
        display = importlib.import_module("IPython.core.display")
        display.display(display.HTML("\n".join(self.html_conent)))

    def html(self):
        return self.html_conent


class Eniam():
    def __init__(self, lexicon_content=None):
        self.lexicon_content = lexicon_content

    def dom(self, input_text):
        p = run_domparser([], PIPE, PIPE, self.lexicon_content)
        q_out = Queue()
        q_err = Queue()

        print("push")
        p.stdin.write((input_text + "\r\n\r\n\r\n\r\n").encode())
        p.stdin.close()
        print("ok")

        t = Thread(target=enqueue_output, args=(p.stdout, q_out))
        t.daemon = True  # thread dies with the program
        t.start()

        t_err = Thread(target=enqueue_output, args=(p.stderr, q_err))
        t_err.daemon = True  # thread dies with the program
        t_err.start()

        start_time = None
        output = []
        collect_pdf = False
        collect_output = False
        collect_err = False
        output_err = []

        output_pdf = tempfile.NamedTemporaryFile(suffix='.pdf')

        pipe_start_time = time.time()
        while True:
            try:
                line = q_out.get_nowait()
            except Empty:
                if start_time is not None:
                    if (time.time() - start_time > 3):
                        break
                if (time.time() - pipe_start_time > 10) and (start_time is None):
                    raise Exception("Parser timeout")
                time.sleep(1)
                print("scan")
            else:
                if start_time is None:
                    start_time = time.time()
                line_content = ""
                if not collect_pdf:
                    line_content = line.decode()
                    if line_content.find("LCG lexicon error") > -1:
                        collect_err = True
                    print("=> "+line_content)
                if collect_err:
                    output_err.append(line_content)
                if line_content.startswith("<!DOCTYPE"):
                    collect_output = True
                if collect_output:
                    output.append(line_content)
                if collect_pdf:
                    output_pdf.write(line)
                if line_content.startswith("</html>"):
                    collect_output = False
                    collect_pdf = True
                    print("collect pdf mode")
            err_line = ""
            try:
                err_line = q_err.get_nowait().decode()
            except Empty:
                err_line = ""
            if len(err_line) > 0:
                print("err: " + err_line)
                collect_err = True
                output_err.append(err_line)
                break
        p.kill()
        time.sleep(1)
        t.join()
        t_err.join()

        if collect_err:
            raise Exception("\n".join(output_err))

        return DomparserResult(output, output_pdf)

    def syntax(self, input_text):
        p = run_subsyntax(["-h"], PIPE, PIPE, self.lexicon_content)
        q_out = Queue()

        p.stdin.write((input_text + "\r\n\r\n").encode())
        p.stdin.close()

        t = Thread(target=enqueue_output, args=(p.stdout, q_out))
        t.daemon = True  # thread dies with the program
        t.start()

        start_time = None
        output = []
        while True:
            try:
                line = q_out.get_nowait()
            except Empty:
                if start_time is not None:
                    if (time.time() - start_time > 3):
                        break
                time.sleep(1)
            else:
                if start_time is None:
                    start_time = time.time()
                output.append(line.decode())
        p.kill()
        time.sleep(1)
        t.join()
        return SubsyntaxResult(output)