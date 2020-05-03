import graph_tool as gt
import sys
from random import randint
from collections import deque



def distance(start, end):
    visited = set()
    queue = deque()
    
    visited.add(start)
    queue.append((start, 0))
    
    while queue:
        node, d = queue.popleft()
        if node == end:
            return d
        
        for n in node.out_neighbours():
            if n not in visited:
                visited.add(n)
                queue.append((n, d+1))
    
    return None





print "Loading graph..."
sys.stdout.flush()

graph = gt.load_graph(sys.argv[1])

print "Done.\n"
sys.stdout.flush()

if len(sys.argv) > 2 and sys.argv[2] == 'undirected':
    graph.set_directed(False)
if len(sys.argv) > 2 and sys.argv[2] == 'directed':
    graph.set_directed(True)

s = 0  # sum
n = 0  # number if trials
m = 0  # missing, i.e. how many times there is no way

while True:
    n1 = graph.vertex(randint(0, graph.num_vertices()-1))
    n2 = graph.vertex(randint(0, graph.num_vertices()-1))
    
    d = distance(n1, n2)
    
    n += 1
    
    if d == None:
        m += 1
    else:
        s += d
    
    print 'trial: ' + str(n) + ', average lenght: ' + str(s / float(n-m)) + ', unconnected: ' + str(m * 100.0 / n) + '%'
    sys.stdout.flush()
    
    
    
