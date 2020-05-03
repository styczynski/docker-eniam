/*******************************************************************/
/*                                                                 */
/*  FILE     fst-generate.C                                        */
/*  MODULE   fst-generate                                          */
/*  PROGRAM  SFST                                                  */
/*  AUTHOR   Helmut Schmid, IMS, University of Stuttgart           */
/*                                                                 */
/*******************************************************************/

#include "fst.h"

using std::cerr;

bool Upper=false;
bool Lower=false;


/*******************************************************************/
/*                                                                 */
/*  usage                                                          */
/*                                                                 */
/*******************************************************************/

void usage()

{
  fprintf(stderr,"Usage: fst-generate [Options] file\n");
  fprintf(stderr,"\nOptions:\n");
  fprintf(stderr,"\t-s: print the surface characters only\n");
  fprintf(stderr,"\t-a: print the analysis characters only\n");
  fprintf(stderr,"\t-h: print usage information\n\n");
}


/*******************************************************************/
/*                                                                 */
/*  get_flags                                                      */
/*                                                                 */
/*******************************************************************/

void get_flags( int *argc, char **argv )

{
  for( int i=1; i<*argc; i++ ) {
    if (strcmp(argv[i],"-s") == 0) {
      Upper = true;
      argv[i] = NULL;
    }
    else if (strcmp(argv[i],"-a") == 0) {
      Lower = true;
      argv[i] = NULL;
    }
    else if (strcmp(argv[i],"-h") == 0 ||
	     strcmp(argv[i],"-help") == 0 ||
	     strcmp(argv[i],"-?") == 0)
      {
	usage();
	exit(0);
      }
  }
  // remove flags from the argument list
  int k;
  for( int i=k=1; i<*argc; i++)
    if (argv[i] != NULL)
      argv[k++] = argv[i];
  *argc = k;
}


/*******************************************************************/
/*                                                                 */
/*  main                                                           */
/*                                                                 */
/*******************************************************************/

int main( int argc, char **argv )

{
  FILE *file;

  get_flags(&argc, argv);

  if (argc == 1)
    file = stdin;
  else if ((file = fopen(argv[1],"rb")) == NULL) {
    fprintf(stderr,"\nError: Cannot open transducer file %s\n\n", argv[1]);
    exit(1);
  }

  try {
    Transducer *a2, a(file);
    fclose(file);
    if (Upper)
      a2 = &a.upper_level().minimise();
    else if (Lower)
      a2 = &a.lower_level().minimise();
    else
      a2 = &a;
    a2->generate(stdout);
  }
  catch (const char *p) {
    cerr << p << "\n";
    return 1;
  }

  return 0;
}
