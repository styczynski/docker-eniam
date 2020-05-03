To build the project execute
ant build
in the main directory (mate-tools).
This requires ant which can be installed by executing
sudo apt-get install ant.

To parse an example sentence using the provided model execute
java -jar dist/anna-3.5.jar -model examples/160622_Polish_MateParser.mdl -test examples/test.csv
in the main directory (mate-tools).
This will create the file dp.conll containing the parser's output. It should be identical to examples/test.out.
