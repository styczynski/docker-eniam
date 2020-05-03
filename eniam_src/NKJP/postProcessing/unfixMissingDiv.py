import re
import sys
from typing import Match

def unfix(m:Match) -> str:
	n0 = int(m.group(2))
	n1 = n0 + 1 if n0 > 2 else n0
	return m.group(1) + str(n1) + m.group(3)

if __name__ == '__main__':
	with open(sys.argv[1]) as i:
		for l in i:
			s1 = re.sub(r'^(\s*)(\d+)(\.\d+-ab;\n)', unfix, l)
			if not int(sys.argv[2]): print(s1, end='')
			else: print(re.sub(r'^()(\d+)(;\n)', unfix, s1), end='')
