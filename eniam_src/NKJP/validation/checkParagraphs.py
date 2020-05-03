from enum import Enum
import sys

class State(Enum):
	DEF = 0
	NEWPAR = 1
	ENDSEN = 2
	ENDPAR = 3

def indent(l: str) -> int:
	i = 0
	while l[i] == '\t': i += 2
	return i / 2

if __name__ == '__main__':
	with open(sys.argv[1]) as i:
		state = State.DEF
		ab = ""
		tokenAb = ""
		abEnd = 0
		for l in i:
			if len(l) == 1:
				if state == State.ENDSEN:
					tokenAb = tokenAb.replace(chr(160), ' ')
					ab = ab.replace(chr(160), ' ')
					if ab.rstrip(' ') != tokenAb:
						print("Mismatch in " + sys.argv[1] + ":\n" + tokenAb + "\n" + ab)
						print([i for i in range(len(tokenAb)) if tokenAb[i] != ab[i]])
					tokenAb = ""
					abEnd = 0
					state = State.ENDPAR
				elif state != State.ENDPAR:
					state = State.ENDSEN
			elif indent(l) == 0:
				if state == State.NEWPAR:
					ab = l[:-2]
				state = State.DEF
			elif indent(l) == 1:
				state = State.NEWPAR
			elif indent(l) == 3:
				split = l.split('; ')
				beg = int(split[1])
				length = int(split[2])
				tokenAb += ' ' * (beg - abEnd)
				tokenAb += split[3]
				abEnd = beg + length
				state = State.DEF
