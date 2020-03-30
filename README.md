# Cross-platform ENIAM

This project provides only a wrapper for ENIAM ([see online](http://eniam.nlp.ipipan.waw.pl/) [source code](http://git.nlp.ipipan.waw.pl/wojciech.jaworski/ENIAM))
Author of original code is Wojciech Jaworski.

## Running inside docker

To run ENIAM docker please use the following commands:
```bash
    # Run subsyntax tool
    $ docker run -it styczynski/eniam:0.1 /root/subsyntax --help
    # Run lexicon printer
    $ docker run -it styczynski/eniam:0.1 /root/print_lexicon --help
    # Run DOM parser
    $ docker run -it styczynski/eniam:0.1 /root/domparser --help
```

## Running with Python wrapper

You can install Python wrapper to get a nice wrapper around the docker container (this requires Python +3 and Docker installed):
```bash
    $ pip install eniam
    $ eniam-cli sub --help
    $ eniam-cli lex --help
    $ eniam-cli dom --help
```

**Note:** Before installing you may wish to add python /bin/ directory to the PATH variable.
On MacOS for Python 3.7 that would be:
```bash
    $ export PATH="$PATH:/usr/local/Cellar/python/3.7.5/Frameworks/Python.framework/Versions/3.7/bin"
```
See [setting up Python PATH](https://www.tutorialspoint.com/python/python_environment.htm)