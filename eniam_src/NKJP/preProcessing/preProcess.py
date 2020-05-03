import re
import sys
from typing import Dict, Match, Tuple

def posDict(ann_segm: str) -> Dict[str, Tuple[str, str, bool]]:
	srch = re.finditer(r'<seg corresp="text.xml#string-range\(txt_(.+?-ab),(\d+?),\d+?\)".*?xml:id="(.+?)"/>', ann_segm)
	return dict((m.group(3), (m.group(1), m.group(2), 'nkjp:rejected="true"' in m.group(0))) for m in srch)

def tamperFun(m: Match, pD: Dict[str, Tuple[str, str, bool]]) -> str:
	(ab, pos, rej) = pD[m.group(2)]
	rejStr = ' nkjp:rejected="true"' if rej else ''
	return m.group(1) + m.group(2) + m.group(3) + ' pos="' + pos + '" ab="' + ab + '"' + rejStr + '>'

def tamper(source: str, pD: Dict[str, Tuple[str, str, bool]]) -> str:
	return re.sub(r'(<seg corresp="ann_segmentation.xml#)(.+?)(" xml:id=".+?")>', lambda m: tamperFun(m, pD), source, flags=re.DOTALL)

if __name__ == '__main__':
	if len(sys.argv) < 3:
		print("Too few arguments.")
		exit()
	with open(sys.argv[1], 'r') as i, open(sys.argv[2], 'r') as ann_segm:
		pD = posDict(ann_segm.read())
		print(tamper(i.read(), pD))
