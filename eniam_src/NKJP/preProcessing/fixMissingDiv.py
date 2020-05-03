import re
import sys
from typing import Match

def fix(m: Match) -> str:
	n0 = int(m.group(1))
	n1 = n0 - 1 if n0 > 2 else n0
	return str(n1) + (m.group(2) or '') + m.group(3)

if __name__ == '__main__':
	with open(sys.argv[1]) as i:
		for l in i:
			print(re.sub(r'(\d+)(\.\d+)?(-div|-ab)', fix, l), end='')
