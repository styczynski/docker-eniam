/*******************************************************************/
/*                                                                 */
/*  FILE     generate.C                                            */
/*  MODULE   generate                                              */
/*  PROGRAM  SFST                                                  */
/*  AUTHOR   Helmut Schmid, IMS, University of Stuttgart           */
/*                                                                 */
/*  PURPOSE  prints automata                                       */
/*                                                                 */
/*******************************************************************/

#include "fst.h"

using std::vector;

const int BUFFER_SIZE = 10000;


/*******************************************************************/
/*                                                                 */
/*  Transducer::generate1                                          */
/*                                                                 */
/*******************************************************************/

bool Transducer::generate1( Node *node, Node2Int &visitations, char *buffer,
			   int pos, char *buffer2, int pos2, FILE *file  )
{
  bool result = false;

  if (pos > BUFFER_SIZE - 100 || pos2 > BUFFER_SIZE - 100)
    return result;
  if (node->is_final()) {
    buffer[pos] = '\n';
    buffer[pos+1] = 0;
    if (buffer2) {
      buffer2[pos2] = '\t';
      buffer2[pos2+1] = 0;
      fputs(buffer2, file);
    } 
    fputs(buffer, file);
    result = true;
  }

  visitations[node]++;
  // sort arcs by number of visitations
  vector<Arc*> arc;
  for( ArcsIter p(node->arcs()); p; p++ ) {
    Arc *a=p;
    Node *n=a->target_node();
    size_t i;
    for( i=0; i<arc.size(); i++ )
      if (visitations[n] < visitations[arc[i]->target_node()])
	break;
    arc.push_back(NULL);
    for( size_t k=arc.size()-1; k>i; k-- )
      arc[k] = arc[k-1];
    arc[i] = a;
  }
  for( size_t i=0; i<arc.size(); i++ ) {
    int p = pos;
    int p2 = pos2;
    Label l = arc[i]->label();
    if (buffer2) {
      Character lc=l.lower_char();
      Character uc=l.upper_char();
      if (lc != Label::epsilon)
	alphabet.write_char(lc, buffer, &p );
      if (uc != Label::epsilon)
	alphabet.write_char(uc, buffer2, &p2 );
    } else
      alphabet.write_label(l, buffer, &p );
    result |= generate1( arc[i]->target_node(), visitations, 
			 buffer, p, buffer2, p2, file);
  }
  return result;
}


/*******************************************************************/
/*                                                                 */
/*  Transducer::generate                                           */
/*                                                                 */
/*******************************************************************/

bool Transducer::generate( FILE *file, bool separate  )

{
  bool result;
  char buffer[BUFFER_SIZE];
  Node2Int visitations;

  if (separate) {
    char buffer2[BUFFER_SIZE];
    result = generate1( root_node(), visitations, buffer, 0, buffer2, 0,file);
  }
  else
    result = generate1( root_node(), visitations, buffer, 0, NULL, 0, file );

  return result;
}
