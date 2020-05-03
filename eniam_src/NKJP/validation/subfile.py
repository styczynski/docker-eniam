import sys

def subfile(f0, f1):
	i = iter(f1)
	#for l in f0:
	#	if l not in i:
	#		print(l)
	#		return False
	#return True
	return all(l in i for l in f0)

if __name__ == '__main__':
	with open(sys.argv[1]) as f0, open(sys.argv[2]) as f1:
		print(subfile(f0, f1))
