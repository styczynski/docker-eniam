To build the project execute
mkdir dist
mkdir include
ant build
in the main directory (mate-tools).
This requires ant which can be installed by executing
sudo apt-get install openjdk-8-jdk
sudo apt-get install ant.

To parse an example sentence using the provided model execute
java -jar dist/anna-3.5.jar -model examples/160622_Polish_MateParser.mdl -test < examples/test.csv
in the main directory (mate-tools).
The output should be identical to the contents of examples/test.out.
