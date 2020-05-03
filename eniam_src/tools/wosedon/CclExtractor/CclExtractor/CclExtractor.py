import argparse
import graph_tool
import xml.etree.ElementTree as ET


IN_SENTENCE = 'in_sentence'
IN_PARAGRAPH = 'in_paragraph'
IN_DOCUMENT = 'in_document'


def make_parser():
    desc = 'Extracts graph of synsets from ccl files.'
    parser = argparse.ArgumentParser(description = desc)
    parser.add_argument('-i', help='list of files to process', required=True, metavar='INPUT', nargs='+')
    parser.add_argument('-o', help='output file whither the graph will be stored', default='ccl_graph.xml', metavar='OUTPUT')
    parser.add_argument('-b', help='if set, there will be at most one (best of all) relation between any two nodes', action='store_true', default=False)
    parser.add_argument('-d', help='if set, edges "in_document" will be generated (slow and worthless)', action='store_true', default=False)
    parser.add_argument('-p', help='if set, edges "in_paragraph" will be generated', action='store_true', default=False)
    parser.add_argument('-s', help='if set, edges "in_sentence" will be generated', action='store_true', default=False)
    return parser


def comp_rel(r1, r2):
    """
    Compare two relation types. Return 1 if r2 is better, 0 if they are equal and -1 if r2 is worse.
    """
    if r2 == r1:
        return 0
    if r2 == IN_SENTENCE:
        return 1
    if r1 == IN_SENTENCE:
        return -1
    if r2 == IN_PARAGRAPH:
        return 1
    return -1


def add_edge(graph, s, t, rel, rel_type_prop, only_best):
    if only_best:
        e = graph.edge(s, t)
        if e:
            if comp_rel(rel_type_prop[e], rel) == 1:
                rel_type_prop[e] = rel
            return
  
    e = graph.add_edge(s, t)
    rel_type_prop[e] = rel
  
  

def main(argv = None):
    parser = make_parser()
    args = parser.parse_args(argv)
    
    graph = graph_tool.Graph()
    
    synset_id = graph.new_vertex_property("int")
    graph.vertex_properties['synset_id'] = synset_id
    
    rel_type = graph.new_edge_property("string")
    graph.edge_properties['rel_type'] = rel_type
    
    syn_to_node = {}
    
    counter = 0
    for path in args.i:
        counter += 1
        print "Processing", path, "(" + str(counter) + "/" + str(len(args.i)) + " = " + str(round(counter * 100.0 / len(args.i), 3)) + "%)"
        in_document = set()
        root = ET.parse(path).getroot()
        for chunk in root:
            in_paragraph = set()
            for sentence in chunk:
                in_sentence = set()
                for token in sentence:
                    for prop in token.findall('prop'):
                        if prop.get('key') == "wsd:synset":
                            si = int(prop.text)
                            in_sentence.add(si)
                            in_paragraph.add(si)
                            in_document.add(si)
                            if si not in syn_to_node:
                                node = graph.add_vertex()
                                syn_to_node[si] = node
                                synset_id[node] = si
                if args.s:
                    for syns1 in in_sentence:
                        for syns2 in in_sentence:
                            if syns1 != syns2:
                                add_edge(graph, syn_to_node[syns1], syn_to_node[syns2], IN_SENTENCE, rel_type, args.b)
            
            if args.p:
                for syns1 in in_paragraph:
                    for syns2 in in_paragraph:
                        if syns1 != syns2:
                            add_edge(graph, syn_to_node[syns1], syn_to_node[syns2], IN_PARAGRAPH, rel_type, args.b)
        if args.d:
            for syns1 in in_document:
                for syns2 in in_document:
                    if syns1 != syns2:
                        add_edge(graph, syn_to_node[syns1], syn_to_node[syns2], IN_DOCUMENT, rel_type, args.b)
    
    graph.save(args.o)
                            
                        


if __name__ == '__main__':
        main()
