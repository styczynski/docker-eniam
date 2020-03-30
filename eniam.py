#!/usr/local/bin/python
import subprocess
import sys

DOCKER_IMAGE_NAME="styczynski/eniam:0.1"

def main():
    if len(sys.argv)>1 and sys.argv[1] == "sub":
        subprocess.call(["docker", "run", "-it", DOCKER_IMAGE_NAME, "/root/subsyntax"]+sys.argv[2:])
    elif len(sys.argv)>1 and sys.argv[1] == "lex":
        subprocess.call(["docker", "run", "-it", DOCKER_IMAGE_NAME, "/root/print_lexicon"]+sys.argv[2:])
    elif len(sys.argv)>1 and sys.argv[1] == "dom":
        subprocess.call(["docker", "run", "-it", DOCKER_IMAGE_NAME, "/root/domparser"]+sys.argv[2:])
    else:
        print("Python cross-platform wrapper around ENIAM (http://eniam.nlp.ipipan.waw.pl/) (http://git.nlp.ipipan.waw.pl/wojciech.jaworski/ENIAM)")
        print("The author of the original code is Wojciech Jaworski.")
        print("")
        print("Usage:")
        print("   eniam.py [sub|lex|dom] <args>")
        print("      Use 'eniam.py sub <args>' to run subsyntax tool")
        print("      Use 'eniam.py lex <args>' to run lexer printer tool")
        print("      Use 'eniam.py dom <args>' to run dom parser tool")
        print("")
        print("   You can use 'eniam.py [sub|lex|dom] --help' to get more help information.")
        print("   Please browse official ENIAM documentation to learn more.")

if __name__ == "__main__":
    main()
