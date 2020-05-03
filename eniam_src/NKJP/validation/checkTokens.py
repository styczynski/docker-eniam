import sys

def indent(l: str) -> int:
	i = 0
	while l[i] == '\t': i += 2
	return i / 2

if __name__ == '__main__':
	with open(sys.argv[1]) as i:
		ab = None
		readNext = False
		prevBeg = -1
		for l in i:
			if readNext:
				ab = l
				readNext = False
			elif indent(l) == 1:
				readNext = True
				prevBeg = -1
			elif indent(l) == 3:
				split = l.split('; ')
				beg = int(split[1])
				end = beg + int(split[2])
				if ab[beg:end] != split[3]: print("Token error at\n" + l)
				if beg <= prevBeg: print("Monotonicity error at\n" + l)
				prevBeg = beg
