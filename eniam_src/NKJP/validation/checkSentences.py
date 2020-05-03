from enum import Enum
import sys

class State(Enum):
	DEF = 0
	NEWPAR = 1
	NEWSEN = 2
	SKIP = 3

def indent(l: str) -> int:
	i = 0
	while l[i] == '\t': i += 2
	return i / 2

if __name__ == '__main__':
	with open(sys.argv[1]) as i:
		state = State.DEF
		ab = ""
		sen = ""
		senBeg = 0
		senEnd = 0
		for l in i:
			if len(l) == 1:
				if state != State.SKIP:
					sen = sen.replace(chr(160), ' ')
					ab = ab.replace(chr(160), ' ')
					if sen != ab[senBeg:senEnd]:
						print("Mismatch in " + sys.argv[1] + ":\n" + sen + "\n" + ab[senBeg:senEnd])
						print([i for i in range(len(sen)) if sen[i] != ab[senBeg:senEnd][i]])
					sen = ""
					state = State.SKIP
			elif indent(l) == 0:
				if state == State.NEWPAR:
					ab = l
				state = State.DEF
			elif indent(l) == 1:
				state = State.NEWPAR
			elif indent(l) == 2:
				state = State.NEWSEN
			elif indent(l) == 3:
				split = l.split('; ')
				beg = int(split[1])
				if state == State.NEWSEN:
					senBeg = beg
					senEnd = beg
				length = int(split[2])
				sen += ' ' * (beg - senEnd)
				sen += split[3]
				senEnd = beg + length
				state = State.DEF
