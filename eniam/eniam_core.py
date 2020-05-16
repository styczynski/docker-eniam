import subprocess
import sys
import importlib
from bs4 import BeautifulSoup
DOCKER_IMAGE_NAME = "styczynski/eniam:1.0"

from .eniam_syntax import SUPPORTED_VALUES

from base64 import b64encode
import time
import sys
from subprocess import PIPE, Popen
from threading  import Thread
import tempfile


from multiprocessing import Pool
from functools import partial


def _pickle_method(method):
    func_name = method.im_func.__name__
    obj = method.im_self
    cls = method.im_class
    if func_name.startswith('__') and not func_name.endswith('__'): #deal with mangled names
        cls_name = cls.__name__.lstrip('_')
        func_name = '_' + cls_name + func_name
    return _unpickle_method, (func_name, obj, cls)


def _unpickle_method(func_name, obj, cls):
    for cls in cls.__mro__:
        try:
            func = cls.__dict__[func_name]
        except KeyError:
            pass
        else:
            break
    return func.__get__(obj, cls)

import copyreg
import types
copyreg.pickle(types.MethodType, _pickle_method, _unpickle_method)

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


def run_domparser(args=[], input=None, output=None, lexicon=None, valence=None, mwe=None, ne=None):
    lexicon_input = "empty" if lexicon is None else b64encode(lexicon.encode()).decode()
    valence_input = "empty" if valence is None else b64encode(valence.encode()).decode()
    mwe_input = "empty" if mwe is None else b64encode(mwe.encode()).decode()
    ne_input = "empty" if ne is None else b64encode(ne.encode()).decode()
    return subprocess.Popen(
        create_cmd(["docker", "run", "-i", DOCKER_IMAGE_NAME, "/root/domparser.sh", lexicon_input, valence_input, mwe_input, ne_input] + args),
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

class MultiprocessDomparserResult():
    def __init__(self, input, inst, opts):
        self.input = input
        self.opts = opts
        self.inst = inst

    def _call_dom(self, opts):
        start_time = time.time()
        out = self.inst._exec_dom_opt(opts).html()
        end_time = time.time()
        return [out, int((end_time - start_time)*100)/100]

    def save_html(self, file_name='output'):
        with open(f"{file_name}.html", 'w') as f:
            f.write(self.html())
        return self

    def show(self):
        display = importlib.import_module("IPython.core.display")
        display.display(display.HTML(self.html()))

    def html(self):
        pool = Pool()
        output_htmls = pool.map(self._call_dom, list(map(lambda sentence: [sentence, self.opts], self.input)))
        chunks = []

        index = -1
        for out in output_htmls:
            output_html = out[0]
            parsing_time = out[1]

            index = index+1
            soup = BeautifulSoup(output_html, features="html.parser")
            body = soup.body
            body_contents = str(body.find_all(recursive=False)[0])
            wrapped_result = f"""
            <section>
                <div>
                    <table>
                        <tr>
                            <th>Property</th>
                            <th>Value</th>
                        </tr>
                        <tr>
                            <td>Sentence no.</td>
                            <td>{index+1}</td>
                        </tr>
                        <tr>
                            <td>Input sentence</td>
                            <td>{self.input[index]}</td>
                        </tr>
                        <tr>
                            <td>Parsing time</td>
                            <td>{parsing_time} sec.</td>
                        </tr>
                    </table>
                </div>
                <br/>
                <div>
                    {body_contents}
                </div>
            </section>
            """
            chunks.append(wrapped_result)

        chunks_str = '\n'.join(chunks)
        return f"""
            <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
            <html>
                <head>
                    <META HTTP-EQUIV="CONTENT-TYPE" CONTENT="text/html; charset=utf8">
                    <TITLE>ENIAM: Kategorialny Parser Składniowo-Semantyczny</TITLE>
                    <META HTTP-EQUIV="Content-Language" CONTENT="pl">
                </head>
                <body>
                    <div>
                        ENIAM parsed {len(self.input)} sentences.
                    </div>
                    <div>
                        {chunks_str}
                    <div>
                </body>
        """


class DomparserResult():
    def __init__(self, html_content, output_pdfs, output_pngs, file_map):
        self.html_conent = html_content
        self.output_pdfs = output_pdfs
        self.output_pngs = output_pngs
        self.file_map = file_map
        for file in self.output_pdfs:
            file.seek(0)
        for file in self.output_pngs:
            file.seek(0)

    def save_html(self, file_name='output'):
        with open(f"{file_name}.html", 'w') as f:
            f.write(self.html())
        return self

    def _replace_all_links(self, html_code):
        for original_file_name in self.file_map:
            entry = self.file_map[original_file_name]
            path = original_file_name.split('/')
            path = path[len(path)-1].replace('\n', '').replace('\r', '')
            if entry[0] == 'png':
                html_code = html_code.replace(f'<A HREF="{path}">',
                                              self.output_chart_html(entry[1]) + "<a style='display:none;'>")
                html_code = html_code.replace(f'<IMG SRC="{path}">',
                                              self.output_chart_html(entry[1]))
            else:
                html_code = html_code.replace(f'<A HREF="{path}">', self.html_chart(entry[1]) + "<a style='display:none;'>")
        return html_code

    def show(self):
        display = importlib.import_module("IPython.core.display")
        display.display(display.HTML(self.html()))

    def html(self):
        return self._replace_all_links("\n".join(self.html_conent))

    def output_chart_html(self, index=0):
        self.output_pngs[index].seek(0)

        image_data = None
        with open(self.output_pngs[index].name, "rb") as img_file:
            image_data = b64encode(img_file.read()).decode()

        return f'<div><img src="data:image/png;base64,{image_data}"/></div>'

    def output_chart(self, index=0):
        display = importlib.import_module("IPython.core.display")
        display.display(display.HTML(self.output_chart_html(index)))

    def chart(self, index=0):
        self.output_pdfs[index].seek(0)
        WImage = importlib.import_module("wand.image").Image
        img = WImage(filename=self.output_pdfs[index].name)
        return img

    def tables_chart(self, index=0):
        self.output_pdfs[index].seek(0)
        tables = importlib.import_module("camelot").read_pdf(self.output_pdfs[index].name)
        return tables

    def csv_chart_file(self, index=0):
        output_csv = tempfile.NamedTemporaryFile(suffix='.csv')
        tables = self.tables_chart(index)
        tables[0].to_csv(output_csv.name)
        output_csv.seek(0)
        return output_csv

    def csv_chart(self, index=0):
        f = self.csv_chart_file(index)
        output = f.read().decode()
        f.close()
        return output

    def html_chart(self, index=0):
        tables = self.tables_chart(index)
        content = tables[0].df.to_html()
        return content


class SubsyntaxResult():
    def __init__(self, html_content):
        self.html_conent = html_content

    def show(self):
        display = importlib.import_module("IPython.core.display")
        display.display(display.HTML("\n".join(self.html_conent)))

    def html(self):
        return self.html_conent

    def save_html(self, file_name='output'):
        with open(f"{file_name}.html", 'w') as f:
            f.write(self.html())
        return self


class Eniam():

    def with_mwe(self, mwe):
        output = []
        for entity in mwe:
            output.append(f"{entity[0]}\t{entity[1]}\t{entity[2]}")
        self.mwe = "\n".join(output)
        return self

    def with_ne(self, ne):
        output = []
        for entity in ne:
            output.append(f"{entity[0]}\t{entity[1]}")
        self.ne = "\n".join(output)
        return self

    def __init__(self, lexicon_keywords=None, lexicon=None, valence_names=None, valence=None, mwe=None, ne=None):

        self.mwe = None
        self.ne = None

        lexicon_base_keywords = """
          infp np prepnp adjp ip cp ncp advp padvp
          adja prepadjp compar measure num aglt aux-fut
          aux-past aux-imp qub interj hyphen int
          rparen rparen2 rquot rquot2 rquot3 inclusion
          day-interval day-lex day-month-interval date-interval
          month-lex month-interval year-interval roman roman-interval
          hour-minute-interval hour-interval obj-id match-result
          url email day-month day year date hour hour-minute
          się nie by s <root> <conll_root> or or2 <colon> <speaker> <speaker-end> <squery> <sentence> <paragraph>
          <subst> <depr> <ppron12> <ppron3> <siebie> <prep> <num> <numcomp> <intnum>
          <realnum> <intnum-interval> <realnum-interval> <symbol> <ordnum>
          <date> <date-interval> <hour-minute> <hour> <hour-minute-interval>
          <hour-interval> <year> <year-interval> <day> <day-interval> <day-month>
          <day-month-interval> <month-interval> <roman> <roman-interval> <roman-ordnum>
          <match-result> <url> <email> <phone-number> <postal-code> <obj-id> <list-item> <fixed> <adj> <apron> <adjc> <adjp> <adja>
          <adv> <ger> <pact> <ppas> <fin> <bedzie> <praet> <winien> <impt>
          <imps> <pred> <aglt> <inf> <pcon> <pant> <qub> <comp> <compar> <conj> <interj>
          <sinterj> <burk> <interp> <part> <unk> <building-number> do w na location time link
        """

        lexicon_base = """
            pos=subst:  <subst>;
            pos=year:  <year>;
            pos=year-interval:  <year-interval>;
            pos=prep:  <prep>;
            pos=depr:  <depr>;
            pos=ppron12:  <ppron12>;
            pos=ppron3:  <ppron3>;
            pos=siebie:  <siebie>;
            pos=num:  <num>;
            pos=numcomp:  <numcomp>;
            pos=intnum:  <intnum>;
            pos=realnum:  <realnum>;
            pos=intnum-interval:  <intnum-interval>;
            pos=realnum-interval:  <realnum-interval>;
            pos=symbol:  <symbol>;
            pos=ordnum:  <ordnum>;
            pos=date:  <date>;
            pos=date-interval:  <date-interval>;
            pos=hour-minute:  <hour-minute>;
            pos=hour:  <hour>;
            pos=hour-minute-interval:  <hour-minute-interval>;
            pos=hour-interval:  <hour-interval>;
            pos=day:  <day>;
            pos=day-interval:  <day-interval>;
            pos=day-month:  <day-month>;
            pos=day-month-interval:  <day-month-interval>;
            pos=month-interval:  <month-interval>;
            pos=roman:  <roman>;
            pos=roman-interval:  <roman-interval>;
            pos=roman-ordnum:  <roman-ordnum>;
            pos=match-result:  <match-result>;
            pos=building-number:  <building-number>;
            pos=url:  <url>;
            pos=email:  <email>;
            pos=phone-number:  <phone-number>;
            pos=postal-code:  <postal-code>;
            pos=list-item:  <list-item>;
            pos=obj-id:  <obj-id>;
            pos=fixed:  <fixed>;
            pos=apron:  <apron>;
            pos=adj:  <adj>;
            pos=adjc:  <adjc>;
            pos=adjp:  <adjp>;
            pos=adja:  <adja>;
            pos=adv:  <adv>;
            pos=ger:  <ger>;
            pos=pact:  <pact>;
            pos=ppas:  <ppas>;
            pos=fin:  <fin>;
            pos=bedzie:  <bedzie>;
            pos=praet:  <praet>;
            pos=winien:  <winien>;
            pos=impt:  <impt>;
            pos=imps:  <imps>;
            pos=pred:  <pred>;
            pos=aglt:  <aglt>;
            pos=inf:  <inf>;
            pos=pcon:  <pcon>;
            pos=pant:  <pant>;
            pos=qub:  <qub>;
            pos=comp:  <comp>;
            pos=conj:  <conj>;
            pos=interj:  <interj>;
            pos=sinterj:  <sinterj>;
            pos=burk:  <burk>;
            pos=interp:  <interp>;
            pos=part:  <part>;
            pos=compar:  <compar>;
            pos=unk:  <unk>;
            pos=subst:  <subst>;
            pos=year:  <year>;
            pos=year-interval:  <year-interval>;
            pos=prep:  <prep>;
            pos=depr:  <depr>;
            pos=ppron12:  <ppron12>;
            pos=ppron3:  <ppron3>;
            pos=siebie:  <siebie>;
            pos=num:  <num>;
            pos=numcomp:  <numcomp>;
            pos=intnum:  <intnum>;
            pos=realnum:  <realnum>;
            pos=intnum-interval:  <intnum-interval>;
            pos=realnum-interval:  <realnum-interval>;
            pos=symbol:  <symbol>;
            pos=ordnum:  <ordnum>;
            pos=date:  <date>;
            pos=date-interval:  <date-interval>;
            pos=hour-minute:  <hour-minute>;
            pos=hour:  <hour>;
            pos=hour-minute-interval:  <hour-minute-interval>;
            pos=hour-interval:  <hour-interval>;
            pos=day:  <day>;
            pos=day-interval:  <day-interval>;
            pos=day-month:  <day-month>;
            pos=day-month-interval:  <day-month-interval>;
            pos=month-interval:  <month-interval>;
            pos=roman:  <roman>;
            pos=roman-interval:  <roman-interval>;
            pos=roman-ordnum:  <roman-ordnum>;
            pos=match-result:  <match-result>;
            pos=building-number:  <building-number>;
            pos=url:  <url>;
            pos=email:  <email>;
            pos=phone-number:  <phone-number>;
            pos=postal-code:  <postal-code>;
            pos=list-item:  <list-item>;
            pos=obj-id:  <obj-id>;
            pos=fixed:  <fixed>;
            pos=apron:  <apron>;
            pos=adj:  <adj>;
            pos=adjc:  <adjc>;
            pos=adjp:  <adjp>;
            pos=adja:  <adja>;
            pos=adv:  <adv>;
            pos=ger:  <ger>;
            pos=pact:  <pact>;
            pos=ppas:  <ppas>;
            pos=fin:  <fin>;
            pos=bedzie:  <bedzie>;
            pos=praet:  <praet>;
            pos=winien:  <winien>;
            pos=impt:  <impt>;
            pos=imps:  <imps>;
            pos=pred:  <pred>;
            pos=aglt:  <aglt>;
            pos=inf:  <inf>;
            pos=pcon:  <pcon>;
            pos=pant:  <pant>;
            pos=qub:  <qub>;
            pos=comp:  <comp>;
            pos=conj:  <conj>;
            pos=interj:  <interj>;
            pos=sinterj:  <sinterj>;
            pos=burk:  <burk>;
            pos=interp:  <interp>;
            pos=part:  <part>;
            pos=compar:  <compar>;
            pos=unk:  <unk>;
        """

        if lexicon is None:
            lexicon = dict()

        if lexicon_keywords is None:
            lexicon_keywords = []

        if valence_names is None:
            valence_names = []

        if valence is None:
            valence = dict()

        for key in lexicon:
            if hasattr(lexicon[key], 'rule'):
                lexicon[key] = lexicon[key].rule()

        lexicon_custom = []
        for key in lexicon:
            key_str = key
            if '|rule_id|' in key_str:
                key_str = key_str.split('|rule_id|')[1]
            if "__pos__" in key_str:
                for pos in SUPPORTED_VALUES:
                    key_str = ",".join([key_str.replace("__pos__", ""), f"pos={pos}"])
                    lexicon_custom.append(f"{key_str}: {lexicon[key]};")
            elif key_str == "__root__":
                lexicon_custom.append(f"lemma=</sentence>,pos=interp:     BRACKET {lexicon[key]};")
            else:
                lexicon_custom.append(f"{key_str}: {lexicon[key]};")

        lexicon_custom_keywords_str = " ".join(lexicon_keywords)
        lexicon_custom_str = "\n".join(lexicon_custom)
        self.lexicon_content = f"""
            @PHRASE_NAMES
                {lexicon_base_keywords}
                {lexicon_custom_keywords_str}
            
            @WEIGHTS
            
            @LEXICON
            
                lemma=<sentence>,pos=interp:      BRACKET <root>/s;
            
                {lexicon_custom_str}
                {lexicon_base}
        """

        valence_rules = []
        for key in valence:
            valence_rules.append(f"{key}:\t{valence[key]}: ;")

        valence_names_str = " ".join(valence_names)
        valence_rules_str = "\n".join(valence_rules)
        self.valence_content = f"""
        @PARAM_NAMES

          jak czy za do po o w na z u dla przeciwko celu żeby że bo jaki który to ile  aby
        
        @SELPREF_NAMES
        
        {valence_names_str}
        
        @ROLE_NAMES
        
        @LEXICON
        
        {valence_rules_str}
        """

    def lexicon_str(self):
        return "" if self.lexicon_content is None else self.lexicon_content

    def valence_str(self):
        return "" if self.valence_content is None else self.valence_content

    def mwe_str(self):
        return "" if self.mwe is None else self.mwe

    def ne_str(self):
        return "" if self.ne is None else self.ne

    def dom(self, input, silent=True):
        if isinstance(input, list):
            opts = {
                'silent': silent,
            }
            return MultiprocessDomparserResult(input, self, opts)
        else:
            return self._exec_dom(input, silent)

    def _exec_dom_opt(self, args):
        return self._exec_dom(args[0], **(args[1]))

    def _exec_dom(self, input_text, silent=True):
        p = run_domparser(["--no-sem", "--no-def-cat"], PIPE, PIPE, self.lexicon_content, self.valence_content, self.mwe, self.ne)
        q_out = Queue()
        q_err = Queue()

        p.stdin.write((input_text + "\r\n\r\n\r\n\r\n").encode())
        p.stdin.close()

        t = Thread(target=enqueue_output, args=(p.stdout, q_out))
        t.daemon = True  # thread dies with the program
        t.start()

        t_err = Thread(target=enqueue_output, args=(p.stderr, q_err))
        t_err.daemon = True  # thread dies with the program
        t_err.start()

        start_time = None
        output = []
        collect_pdf = False
        collect_png = False
        collect_output = False
        collect_err = False
        output_err = []

        pdf_files = []
        png_files = []

        file_map = dict()

        pipe_start_time = time.time()
        while True:
            try:
                line = q_out.get_nowait()
            except Empty:
                if (time.time() - pipe_start_time > 20) and (start_time is None):
                    raise Exception("Parser timeout")
                time.sleep(1)
                #print("scan")
            else:
                if start_time is None:
                    start_time = time.time()
                line_content = ""
                try:
                    line_content = line.decode()
                except:
                    line_content = ""
                if line_content.find("LCG lexicon error") > -1:
                    collect_err = True
                #print("=> "+line_content)
                if line_content.startswith("@done@"):
                    break
                if collect_err:
                    output_err.append(line_content)
                if line_content.startswith("<!DOCTYPE"):
                    collect_output = True
                if line_content.startswith("@end_png@"):
                    collect_png = False
                else:
                    if collect_png:
                        png_files[len(png_files)-1].write(line)
                    if line_content.startswith("@begin_png@:"):
                        new_file = tempfile.NamedTemporaryFile(suffix='.png')
                        file_map[line_content.replace("@begin_png@: ", "")] = ["png", len(png_files)]
                        collect_output = False
                        collect_png = True
                        png_files.append(new_file)
                        #print(f"collect png mode => [{line_content}]")
                if line_content.startswith("@end_pdf@"):
                    collect_pdf = False
                else:
                    if collect_pdf:
                        pdf_files[len(pdf_files)-1].write(line)
                    if line_content.startswith("@begin_pdf@:"):
                        new_file = tempfile.NamedTemporaryFile(suffix='.pdf')
                        file_map[line_content.replace("@begin_pdf@: ", "")] = ["pdf", len(pdf_files)]
                        collect_output = False
                        collect_pdf = True
                        pdf_files.append(new_file)
                        #print("collect pdf mode")
                if collect_output:
                    output.append(line_content)
            err_line = ""
            try:
                err_line = q_err.get_nowait().decode()
            except Empty:
                err_line = ""
            if len(err_line) > 0:
                if not silent:
                    print(err_line.replace("\n", "").replace("\r", ""))
                #collect_err = True
                #output_err.append(err_line)
                #break
        p.kill()
        time.sleep(1)
        t.join()
        t_err.join()

        if collect_err:
            raise Exception("\n".join(output_err))

        return DomparserResult(output, pdf_files, png_files, file_map)

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