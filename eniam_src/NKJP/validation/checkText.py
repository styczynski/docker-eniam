import re
import sys
from typing import Match

def prepare(m: Match) -> str:
	dic = {"&amp;":"&", "&gt;":">", "&lt;":"<"}
	return re.sub("|".join(dic.keys()), lambda subm: dic[subm.group(0)], m.group(1)) + '\n'

if __name__ == '__main__':
	with open(sys.argv[1] + '/text.xml', 'r') as i, open('extract_text.txt', 'w') as o:
		f = re.finditer(r'<ab.+?>(.+?)</ab>', i.read())
		for ab in f: o.write(prepare(ab))
	with open(sys.argv[1] + '/fold_text.txt', 'r') as i, open('extract_fold_text.txt', 'w') as o:
		f = re.finditer(r'-ab;\n(.*);', i.read())
		for ab in f: o.write(prepare(ab))
	with open(sys.argv[1] + '/fold_segm.txt', 'r') as i, open('extract_fold_segm.txt', 'w') as o:
		f = re.finditer(r'-ab;\n(.*);', i.read())
		for ab in f: o.write(prepare(ab))
	with open(sys.argv[1] + '/fold_morph.txt', 'r') as i, open('extract_fold_morph.txt', 'w') as o:
		f = re.finditer(r'-ab;\n(.*);', i.read())
		for ab in f: o.write(prepare(ab))
	with open(sys.argv[1] + '/fold_sense.txt', 'r') as i, open('extract_fold_sense.txt', 'w') as o:
		f = re.finditer(r'-ab;\n(.*);', i.read())
		for ab in f: o.write(prepare(ab))
