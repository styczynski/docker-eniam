The instructions below apply to modern systems with the following caveats:

* Before running stack install it might be necessary to execute stack setup in order to install the correct version of ghc.

* Maca can be installed using the instructions on nlp.pwr.wroc.pl/redmine/projects/libpltagger/wiki/InstallOnUbuntu11 with the following exceptions:
    * The package libboost-all-dev can be installed in place of libboost1.42-all-dev as the latter might be unavailable.

    * It might also be necessary to install ncurses using

            sudo apt-get install libncurses5-dev
        
    * Instead of using adding Bartosz Zaborowski's repository one can download Morfeusz directly from  
      ppa.launchpad.net/bartosz-zaborowski/nlp/ubuntu/pool/main/m/morfeusz-sgjp/morfeusz-sgjp_0.81-1~precise_amd64.deb

    * Before installing SFST in lines 445 and 449 of maca/third_party/SFST-1.2/SFST/src/fst.C the identifier `it` must be replaced with `iter` (or any other non-conflicting identifier).

    * Additionally with newer versions of boost the following changes become necessary:
        - before installing Corpus 2
            + in line 25 of corpus2/learn_to_guess/main.cpp the type `ifstream` must be replaced with `std::ifstream`

        - before installing Toki
            + in lines 223-224 of toki/tests/compare.cpp the call  
              `boost::unit_test::make_test_case(boost::bind(test_one_item, ci), name)`  
              must be replaced with  
              `boost::unit_test::make_test_case(boost::bind(test_one_item, ci), name, __FILE__, __LINE__)`

            + in line 105 of toki/tests/srx.cpp `BOOST_MESSAGE` must be replaced with `BOOST_TEST_MESSAGE`

        - before installing Maca
            + in line 100 of maca/tests/compareconv.cpp `BOOST_MESSAGE` must be replaced with `BOOST_TEST_MESSAGE`

            + in lines 207-208 of maca/tests/compareconv.cpp the call  
              `boost::unit_test::make_test_case(boost::bind(test_one_item, ci), name)`  
              must be replaced with  
             `boost::unit_test::make_test_case(boost::bind(test_one_item, ci), name, __FILE__, __LINE__)`

After Concraft is successfully installed its executable can be found in ~/.local/bin  
For an example usage of Concraft's server-client mode (in OCaml) see concraft_test.ml

Below are the contents of the original README file.

Concraft-pl
===========

This package provides a morphosyntactic tagger for the Polish language.
The tool combines the following components into a pipeline:
* A morphosyntactic segmentation and analysis tool [Maca][maca],
* A morphosyntactic disambiguation library [Concraft][concraft],

<!---
* A simple, frequency-driven lemmatiser (TODO).  Until the lemmatiser component
  is implemented, the tagger may output multiple interpretations (all related
  to the same morphosyntactic tag, but with different lemmas) in some cases.
-->

As for now, the tagger doesn't provide any lemmatisation capabilities.
As a result, it may output multiple interpretations (all related
to the same morphosyntactic tag, but with different lemmas) for some known
words, while for the out-of-vocabulary words it just outputs orthographic
forms as lemmas.

See the [homepage][homepage] if you wish to download a pre-trained
model for the Polish language.


Installation
============

It is recommanded to install Concraft-pl using the
[Haskell Tool Stack][stack], which you will need to downoload and
install on your machine beforehand.  Then clone this repository into
a local directory and use `stack` to install the library by running:

    stack install

Unless you plan to use a custom preprocessing pipeline or run
[Maca][maca] on a different machine (see section
[Tagging analysed data](#tagging-analysed-data)), you will also need
the [Maca][maca] tool.  A detailed [installation guide][maca-install]
can be found on the [Maca][maca] homepage.


Data format
===========

The current version of Concraft-pl works on a simple `plain` text format supported by
the [Corpus2][corpus2] tools.  You will have to install these tools when you install
[Maca][maca] anyway, so you can use them to convert the output generated
by Concraft-pl to one of other formats supported by [Corpus2][corpus2].


Training
========

If you have the training material with disambiguation annotations (stored in the
`plain` text format) you can train the Concraft-pl model yourself.

    concraft-pl train train.plain -e eval.plain -o model.gz

Concraft-pl uses the [NKJP][nkjp] [morphosyntactic tagset definition](config/nkjp-tagset.cfg)
by default.  It will also reanalyse the input data before the actual training.  If you want
to change this behaviour, use the `--tagset` and `--noana` command-line options.

Consider using [runtime system options][ghc-rts].  You can speed up processing
by making use of multiple cores by using the `-N` option.  The `-s` option will
produce the runtime statistics, such as the time spent in the garbage collector.
If the program is spending too much time collecting garbage, you can try to
increase the allocation area size with the `-A` option.  If you have a big
dataset and it doesn't fit in the computer memory, use the `--disk` flag.
For example, to train the model using four threads and 256M allocation area
size, run:

    concraft-pl train train.plain -e eval.plain -o model.gz +RTS -N4 -A256M -s

Run `concraft-pl train --help` to learn more about the program arguments and
possible training options.

Finally, you may consider pruning the resultant model in order to reduce its size.
Features with values close to 0 (in log-domain) have little effect on the modeled
probability and, therefore, it should be safe to discard them.

    concraft-pl prune -t 0.05 input-model.gz pruned-model.gz


Tagging
=======

Once you have a Concraft-pl model you can use the following command tag `input.txt` file:

    concraft-pl tag model.gz < input.txt > output.plain

The input file is first divided into paragraphs (the tool interprets empty lines
as paragraph ending markers).  After that, [Maca][maca] is used to segment and analyse
each paragraph.  Finally, [Concraft][concraft] module is used to disambiguate each
sentence in the [Maca][maca] output.

With the `--marginals` option enabled, Concraft-pl will output marginal probabilities
corresponding to individual tags (determined on the basis of the disambiguation model)
instead of `disamb` markers.

Run `concraft-pl tag --help` to learn more about possible tagging options.


Server
======

Concraft-pl provides also a client/server mode.  It is handy when, for example,
you need to tag a large collection of small files.  Loading Concraft-pl model
from a disk takes considerable amount of time which makes the tagging method
described above very slow in such a setting.

To start the Concraft-pl server, run:

    concraft-pl server --inmodel model.gz

You can supply a custom port number using a `--port` option.  For example,
to run the server on the `10101` port, use the following command:

    concraft-pl server --inmodel model.gz --port 10101

To use the server in a multi-threaded environment, you need to specify the
`-N` [RTS][ghc-rts] option.  A set of options which usually yields good
server performance is presented in the following example:

    concraft-pl server --inmodel model.gz +RTS -N -A4M -qg1 -I0

Run `concraft-pl server --help` to learn more about possible server-mode options.

The client mode works just like the tagging mode.  The only difference is that,
instead of supplying your client with a model, you need to specify the port number
(in case you used a custom one when starting the server; otherwise, the default
port number will be used).

    concraft-pl client --port 10101 < input.txt > output.plain

Run `concraft-pl client --help` to learn more about possible client-mode options.


Tagging analysed data
=====================

In some situations you might want to feed Concraft-pl with a previously
analysed data.  Perhaps your Maca instance is installed on a different
machine, or maybe you want to use Concraft-pl with a custom
preprocessing pipeline.

If you want to use a preprocessing pipeline significantly different from
the standard one (Maca), you should first train your own Concraft model.
To train the model on analysed data use the `--noana` training flag.

Use the same `--noana` flag when you want to tag analysed data.
Input format should be the same as the output format.
This option is currently not supported in the client/server mode.

*Remember to use the same preprocessing pipeline (segmentation + analysis) for both
training and disambiguation.  Inconsistencies between training material and input
data may severely harm the quality of disambiguation.*


[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
[homepage]: http://zil.ipipan.waw.pl/Concraft "Homepage"
[concraft]: https://github.com/kawu/concraft "Concraft"
[hackage-repo]: http://hackage.haskell.org/package/concraft-pl "Concraft-pl Hackage repository"
[maca]: http://nlp.pwr.wroc.pl/redmine/projects/libpltagger/wiki "Maca"
[maca-install]: http://nlp.pwr.wroc.pl/redmine/projects/libpltagger/wiki#Download-and-install-MACA "Maca installation guide"
[corpus2]: http://nlp.pwr.wroc.pl/redmine/projects/corpus2/wiki "Corpus2"
[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[ghc-rts]: http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html "GHC runtime system options"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"
[nkjp]: http://nkjp.pl/index.php?page=0&lang=1 "NKJP"
