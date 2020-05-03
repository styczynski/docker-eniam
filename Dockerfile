FROM debian:stretch

LABEL maintainer="Piotr Styczyński (styczynski)" \
      description="A docker image to run ENIAM (http://eniam.nlp.ipipan.waw.pl/) (http://git.nlp.ipipan.waw.pl/wojciech.jaworski/ENIAM)" \
      repo="https://github.com/styczynski/docker-eniam"

# Update image
RUN apt-get update && apt-get upgrade && apt-get dist-upgrade && apt-get autoremove

# Latex packages
RUN apt-get install -y --no-install-recommends texlive-latex-recommended texlive-fonts-recommended && \
    apt-get install -y --no-install-recommends texlive-latex-extra texlive-fonts-extra texlive-lang-all && \
    rm -rf /var/lib/apt/lists/*

# Python
RUN apt-get update && apt-get upgrade && apt-get dist-upgrade && apt-get autoremove
RUN apt-get install -y python3 python3-pip

# Graphviz
RUN apt-get install -y graphviz

# Resources
COPY ./data /root/data
COPY ./domparser /root
COPY ./print_lexicon /root
COPY ./resources /root/resources
#COPY ./results /root/results
COPY ./subsyntax /root

# Shell wrappers
COPY ./domparser.sh /root
COPY ./subsyntax.sh /root

WORKDIR /root

# Default command
CMD ["bash"]