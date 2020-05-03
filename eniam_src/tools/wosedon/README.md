Before installing WoSeDon the following libraries must be installed:

* CGAL installed using

        sudo apt-get install libcgal-dev

* cairomm installed using

        sudo apt-get install libcairomm-1.0-dev

* pycairo installed by cloning/downloading the repository at https://github.com/pygobject/pycairo then running

        sudo python setup.py install
  in its main directory

* sparsehash installed using

        sudo apt-get install libsparsehash-dev

* graphtool downloaded from https://graph-tool.skewed.de/ and installed by following the instructions in the included README.
  WARNING: the installation is very resource-intensive and takes a really long time

ALTERNATIVELY in recent Ubuntu distributions (xenial, yakkety, zesty) one can add the following lines to /etc/apt/sources.list (this requires root privileges)  
`deb http://downloads.skewed.de/apt/DISTRIBUTION DISTRIBUTION universe`  
`deb-src http://downloads.skewed.de/apt/DISTRIBUTION DISTRIBUTION universe`  
where `DISTRIBUTION` stands for the distribution name (xenial, yakkety, or zesty) and then run

    apt-key adv --keyserver pgp.skewed.de --recv-key 612DEFB798507F25
    apt-get install python-graph-tool

WoSeDon can be run interactively from the subdirectory wosedon (which should contain the cfg and resources directories among other things) using

    wosedon -i  
for example

    cat test.ccl | wosedon -i
The input needs to use the CCL format.

PLEASE NOTE: to run wosedon from a different directory it is necessary to modify the configuration file wosedon/cfg/wosedon.ini and replace the line  
`plwn_graph_file = resources/plwn_graph`  
with  
`plwn_graph_file = <absolute_path_to_resources_directory>/plwn_graph`  
Afterwards one can run wosedon anywhere by executing

    wosedon -c <path_to_configuration_file> -i

For an example usage of WoSeDon's server mode (in OCaml) see wosedon/wosedon_test.ml

Below are the contents of the original README file.

Poprawne działanie WoSeDona wymaga zainstalowania modułu PLWNGraphBuilder oraz wosedon.

I) Instalacja PLWNGraphBuilder'a, będąc w aktualnym katalogu, wydać polecenie:

    cd PLWNGraphBuilder; sudo python setup.py install; cd ..

II) Instalacja wosedona, będąc w aktualnym katalogu, wydać polecenie:

  cd wosedon; sudo python setup.py install; cd ..


II) Jeśli instalacja przebiegła pomyslne, to po wpisaniu w konsoli polecenia: wosedon
    Powinien ukazać się komunikat zbliżony do tego:

usage: wosedon [-h] [-c CONFIG] [-md MODEL_DIR] [-it] [-a ALPHA] [-V] -f CCL
               [-r RELCCL] [-o OUT_FILE] [-b] [-vd VISUALISATION_DIR]
wosedon: error: argument -f/--cclfile is required

