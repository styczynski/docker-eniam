import re
import sys
from typing import Match

def fix(m: Match) -> str:
	return m.group(1) + re.sub(r'>', r'&gt;', m.group(2)) + m.group(3)

if __name__ == '__main__':
	with open(sys.argv[1]) as i:
		for l in i: print(re.sub(r'(<.+?>)(.+?)(</.+?>)', fix, l), end='')
