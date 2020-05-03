/* Prolog interface to the morphological analyzer Morfeusz 2 (“Morfeusz Reloaded”) 
   Version for SWI Prolog release 5.6 and up (Unicode relases)

   Author: Marcin Woliński 
   This file is in the public domain. 

   To compile use this incantation:
   
   plld -v -shared -o morfeusz-swi.so morfeusz2-glueswi.cc -lmorfeusz2

   If your Prolog interpreter is not called 'pl' add to the options
   -pl <interpreter>.  

   On Ubuntu 14.04 the line becomes:
   
   swipl-ld -v -shared -o morfeusz2-swi.so morfeusz2-glueswi.cc -lmorfeusz2

   Here is how I compile the library for Windows using Linux version of MinGW:
   i586-mingw32msvc-gcc -D_REENTRANT -D__SWI_PROLOG__ -I./ -I/usr/lib/swi-prolog/include -o morfeusz-swi.dll -shared morfeusz2-glueswi.cc  -L./ -lpl -lmorfeusz2

   SWI Prolog 6.x names its library libswipl.dll:
   i586-mingw32msvc-gcc -D_REENTRANT -D__SWI_PROLOG__ -I./ -I/usr/lib/swi-prolog/include -o morfeusz-swi.dll -shared morfeusz2-glueswi.cc  -L./ -lswipl -lmorfeusz2

   SWI Prolog 6.x on Win64 with 64-bit MinGW:
   x86_64-w64-mingw32-gcc -D_REENTRANT -D__SWI_PROLOG__ -I./ -I/usr/lib/swi-prolog/include -o morfeusz-swi.dll -shared morfeusz2-glueswi.cc  -L./ -lswipl -lmorfeusz2

*/
#include <SWI-Prolog.h>

// Needed as long as Morfeusz is compiled with the old GCC ABI for std::string:
#define _GLIBCXX_USE_CXX11_ABI 0

#include <string.h>
#include "morfeusz2.h"

using namespace morfeusz;

static functor_t F_interp;
static functor_t F_colon;
static Morfeusz *m_instance;

//extern "C" 
static foreign_t pl_morfeusz_analyse(term_t st, term_t at) { 
  char *tekst;
  
  if ( PL_get_chars(st, &tekst,
		    CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|
		    BUF_DISCARDABLE|REP_UTF8) ) { 
    ResultsIterator *r=m_instance->analyse(tekst);

    term_t l = PL_copy_term_ref(at);
    term_t elem = PL_new_term_ref();

    while(r->hasNext()) {
      MorphInterpretation i=r->next(); 

      term_t t_lemma = PL_new_term_ref();
      size_t colon_pos = i.lemma.find_last_of(':');
      if (colon_pos == std::string::npos ||
	  colon_pos < 1 ||
	  colon_pos == i.lemma.length()-1 ) {
	// lemat nie zawiera dwukropka lub zawiera go na którymś końcu:
	if (!PL_unify_term(t_lemma,
			   PL_UTF8_CHARS, i.lemma.c_str()
			   )) PL_fail;
      } else {
	if (!PL_unify_term(t_lemma,
			   PL_FUNCTOR, F_colon,
			   PL_UTF8_CHARS, i.lemma.substr(0,colon_pos).c_str(),
			   PL_UTF8_CHARS, i.lemma.substr(colon_pos+1).c_str()
			   )) PL_fail;
      }
	
      term_t interp = PL_new_term_ref();
      if ( !PL_unify_list(l, elem, l) ||
	   //	   !PL_chars_to_term(i.lemma.c_str(), t_lemma) ||
	   !PL_chars_to_term(i.getTag(*m_instance).c_str(), interp) ||
	   // i.getLabelsAsString(*morfeusz)
	   !PL_unify_term(elem,
			  PL_FUNCTOR, F_interp,
			  PL_INT, i.startNode,
			  PL_INT, i.endNode,
			  PL_UTF8_CHARS, i.orth.c_str(),
			  //PL_UTF8_CHARS, i.lemma.c_str(),
			  PL_TERM, t_lemma,
			  PL_TERM, interp/* , */
			  /* PL_UTF8_CHARS, i.getName(*m_instance).c_str() */
			  )
	   ) PL_fail;
    }
    return PL_unify_nil(l);
  }
  PL_fail;
}

extern "C" {
  install_t install() { 
    m_instance=Morfeusz::createInstance();
    m_instance->setCharset(UTF8);
    F_interp = PL_new_functor(PL_new_atom("i"), 5);
    F_colon = PL_new_functor(PL_new_atom(":"), 2);
    PL_register_foreign("morfeusz_analyse", 2, (pl_function_t)pl_morfeusz_analyse, 0);
  }
}

/* Local Variables: */
/* coding: utf-8 */
/* mode: c */
/* End: */
