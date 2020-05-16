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

# Ocaml
RUN apt install -y ocaml-nox # If you don't want X11 support
RUN apt install -y ocaml

COPY ./fake_sudo /bin/sudo

COPY . /root
WORKDIR /root

# Build project
#RUN /root/compile.sh | grep -v 'cp ' | grep -v 'rm ' | grep -v 'mkdir ' | grep -v 'ocamlopt ' | grep -v 'ocamlc '
