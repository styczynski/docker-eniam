/*
    Copyright (C) 2010 Tomasz Śniatowski, Adam Radziszewski
    Part of the libmaca project

    This program is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option)
any later version.

    This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE. 

    See the LICENSE.MACA, LICENSE.SFST, LICENSE.GUESSER, COPYING.LESSER and COPYING files for more details.
*/

#include <libcorpus2/io/writer.h>
#include <libmaca/io/text.h>
#include <libmaca/io/premorph.h>
#include <libmaca/morph/dispatchanalyser.h>
#include <libmaca/util/settings.h>
#include <libcorpus2/util/settings.h>
#include <libmaca/util/sentenceanalyser.h>
#include <libcorpus2/util/tokentimer.h>
#include <libcorpus2/tagsetmanager.h>

#include <libcorpus2/util/ioformat-options.h>

// generated by CMake
#include <libmaca/version.h>

#include <libtoki/sentencesplitter.h>
#include <libtoki/tokenizer/layertokenizer.h>
#include <boost/foreach.hpp>
#include <libtoki/util/settings.h>

#include <boost/algorithm/string.hpp>
#include <boost/program_options.hpp>
#include <boost/make_shared.hpp>

#include <fstream>
#include <omp.h>

void reanalyse_token(Corpus2::Token* token, boost::shared_ptr<Maca::MorphAnalyser> a, const Corpus2::Tag& ign_tag)
{
	bool has_ign = false;
	std::set<Corpus2::Lexeme> nonign;
	std::set<Corpus2::Tag> nonign_tags;
	BOOST_FOREACH(const Corpus2::Lexeme& lex, token->lexemes()) {
		if (lex.tag() == ign_tag) {
			has_ign = true;
		} else if (lex.is_disamb()){
			nonign.insert(lex);
			nonign_tags.insert(lex.tag());
		}
	}
	if (has_ign) {
		boost::shared_ptr<Toki::Token> toki(new Toki::Token(token->orth(), "t", token->wa()));
		std::vector<Corpus2::Token*> newtoks = a->process(*toki);
		if (newtoks.size() == 1) {
			Corpus2::Token* newtok = newtoks[0];
			token->lexemes().clear();
			bool had_disamb = false;
			BOOST_FOREACH(const Corpus2::Lexeme& lex, newtok->lexemes()) {
				if (nonign_tags.find(lex.tag()) == nonign_tags.end()) {
					token->add_lexeme(lex);
				} else {
					had_disamb = true;
				}
			}
			if (had_disamb) {
				BOOST_FOREACH(const Corpus2::Lexeme& lex, nonign) {
					token->add_lexeme(lex);
				}
			} else {
				std::cerr << "Disamb tag not in analysis: ";
				std::cerr << token->orth_utf8() << " ";
				BOOST_FOREACH(const Corpus2::Lexeme& lex, nonign) {
					std::cerr << a->tagset().tag_to_string(lex.tag()) << " ";
				}
				std::cerr << "\n";
			}
		} else {
			std::cerr << "ERROR: Newtoks size is " << newtoks.size()
				<< " for input :" << token->orth_utf8() << "\n";
			token->lexemes().clear();
			token->add_lexeme(Corpus2::Lexeme(UnicodeString::fromUTF8("None"), ign_tag));
		}
	}
}

int main(int argc, char** argv)
{
	std::string config;
	std::vector<std::string> plugins;
	std::string config_path;
	bool quiet = false, progress = false;
	using boost::program_options::value;
	std::string input_filename, output_filename;

	boost::program_options::options_description desc("Allowed options");
	Corpus2::add_input_options(desc);
	Corpus2::add_output_options(desc);
	desc.add_options()
			("config,c", value(&config),
			 "Morphological analyser configuration file")
			("config-path,C", value(&config_path)->default_value(""),
			 "Override config search path")
			("input-file,I", value(&input_filename)->default_value("-"),
			 "Input filename (- for stdin)")
			("output-file,O", value(&output_filename)->default_value("-"),
			 "Output filename (- for stdout)")
			("plugin,P", value(&plugins),
			 "Additional plugins to load")
			("progress,p", value(&progress)->zero_tokens(),
			 "Show progress info")
			("quiet,q", value(&quiet)->zero_tokens(),
			 "Suppress startup info when loading a tagset")
			("help,h", "Show help")
			("version", "print version string")
			;

	boost::program_options::variables_map vm;
	try {
		boost::program_options::store(
			boost::program_options::command_line_parser(argc, argv)
			.options(desc).run(), vm);
	} catch (boost::program_options::error& e) {
		std::cerr << e.what() << "\n";
		return 2;
	}
	boost::program_options::notify(vm);

	if (!config_path.empty()) {
		Maca::Path::Instance().set_search_path(config_path);
	}
	BOOST_FOREACH(const std::string& s, plugins) {
		Maca::MorphAnalyser::load_plugin(s, false);
	}

	if (vm.count("help")) {
		std::cout << "Available configurations: ";
		std::cout << Maca::SentenceAnalyser::available_configurations() << "\n";
		return 1;
	}
	if (vm.count("version")) {
		std::cout << "maca-reanalyse (MACA) " << LIBMACA_VERSION << "\n";
		return 0;
	}

	Toki::Path::Instance().set_verbose(!quiet);
	Maca::Path::Instance().set_verbose(!quiet);
	Corpus2::Path::Instance().set_verbose(!quiet);

	if (!config.empty()) {
		try {
			boost::shared_ptr<Maca::MorphAnalyser> a;
			a = Maca::DispatchAnalyser::create_from_named_config(config);

			boost::shared_ptr<Corpus2::TokenReader> reader;
			reader = Corpus2::create_reader(vm, a->tagset(), input_filename);
			boost::shared_ptr<Corpus2::TokenWriter> writer;
			writer = Corpus2::create_writer(vm, a->tagset(), output_filename);

			Corpus2::TokenTimer& timer = Corpus2::global_timer();
			timer.register_signal_handler();

			Corpus2::Tag ign_tag = a->tagset().make_ign_tag();

			while (boost::shared_ptr<Corpus2::Chunk> chunk = reader->get_next_chunk()) {
				BOOST_FOREACH(Corpus2::Sentence::Ptr sentence, chunk->sentences()) {
					BOOST_FOREACH(Corpus2::Token* token, sentence->tokens()) {
						reanalyse_token(token, a, ign_tag);
					}
				}
				writer->write_chunk(*chunk);
			}
			if (progress) {
				timer.stats();
			}
		} catch (Maca::MacaError& e) {
			std::cerr << "Maca Error: " << e.info() << "\n";
			return 4;
		} catch (Toki::TokiError& e) {
			std::cerr << "Tokenizer Error: " << e.info() << "\n";
			return 6;
		} catch (Corpus2::Corpus2Error& e) {
			std::cerr << "Corpus2 Error: " << e.info() << "\n";
			return 8;
		}
	} else {
		std::cerr << "Usage: maca-reanalyse -c CONFIG [OPTIONS]\n";
		std::cerr << "See maca-reanalyse --help\n";
		return 1;
	}
	return 0;
}
