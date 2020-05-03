Maca can be installed using the instructions on nlp.pwr.wroc.pl/redmine/projects/libpltagger/wiki/InstallOnUbuntu11 with the following exceptions:

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

For an example usage of Maca's interactive mode (in OCaml) see maca_test.ml

Below are the contents of the original README file.

DEPENDENCIES

Maca requires several development packages in order to be built, primarily:
 * Corpus2 and libpwrutils (both from the corpus2 repository)
 * Toki
Other dependencies (some of them indirect via the two libraries above) are:
 * ICU (libicu-dev)
 * Boost libraries, 1.41 or 1.42 (libboost1.42-all-dev); not tested on newer versions
 * Loki (libloki-dev)
 * LibXML++ (libxml++2.6-dev)
 * bison and flex

Plugins require other libraries that are optional:
 * The SFST plugin requires the SFST library 1.2 (see below)
 * The Morfeusz plugin requires the Morfeusz library and header
 * The Guesser plugin requires the Guesser package from TaKIPI / Corpus

SFST note: The SFST package included in Ubuntu and Debian repositories (libsfst-dev) is apparently missing required header files. The sources downloadable from ims.uni-stuttgart.de in turn need some Makefile modifications to build and install the library and headers. There are two possibilities to overcome this:
1. Install the version with modified Makefile contained in third_party directory of this package.
2. Install the Debian package libsfst1-1.2-0-dev, then download the 1.2 sources and copy the missing header files into the proper system header dir (presumably /usr/include/sfst-1.0/sfst/). Make sure those files are given read permissions.


USAGE

See the maca-analyse and maca-convert binaries.


NOTES

Spurious "array subscript is above array bounds" warnings originating in bits/stl_algo seem to be an artifact of gcc4.4's overly-eager correctness checking in conjunction with boost::algorithm::split and boost::is_any_of. Apparently, they are harmless.

