import sys

def indent(l: str) -> int:
	i = 0
	while l[i] == '\t': i += 2
	return i / 2

if __name__ == '__main__':
	with open(sys.argv[1]) as i:
		ab = None
		readNext = False
		for l in i:
			outL = l
			if readNext:
				ab = l
				readNext = False
			elif indent(l) == 1:
				readNext = True
			elif indent(l) == 3:
				split = l.split('; ')
				beg = int(split[1])
				end = beg + int(split[2])
				if ab[beg:end] != split[3]:
					split[3] = ab[beg:end]
					outL = '; '.join(split)
			print(outL, end='')
