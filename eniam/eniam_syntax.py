SUPPORTED_VALUES = {
    "lemma": None,
    "cat": None,
    "pos": [
        "subst", "depr", "ppron12", "ppron3",
        "siebie", "prep", "num", "intnum",
        "realnum", "intnum-interval",
        "realnum-interval", "symbol", "ordnum",
        "date", "date-interval", "hour-minute", "hour",
        "hour-minute-interval", "hour-interval",
        "year", "year-interval", "day", "day-interval",
        "day-month", "day-month-interval",
        "month-interval", "roman", "roman-interval",
        "roman-ordnum", "match-result",
        "url", "email", "obj-id", "adj", "adjc", "adjp",
        "adja", "adv", "ger", "pact", "ppas", "fin",
        "bedzie", "praet", "winien", "impt", "imps",
        "pred", "aglt", "inf", "pcon", "pant", "qub",
        "comp", "conj", "interj", "sinterj", "burk", "interp",
        "unk", "apron", "part",
    ],
    "number": [ "sg", "pl" ],
    "case": [
        "nom", "gen", "dat", "acc", "inst", "loc", "postp", "pred", "voc"
    ],
    "gender": [
        "m1", "m2", "m3", "f", "n1", "n2", "p1", "p2", "p3"
    ],
    "person": [
        "pri", "sec", "ter"
    ],
    "grad": [
        "pos", "com", "sup"
    ],
    "praep": [
        "praep", "npraep", "praep-npraep",
    ],
    "acm": [ "congr", "rec" ],
    "ctype": [ "int", "rel", "sub", "coord" ],
    "mode": [ "abl", "adl", "locat", "perl", "dur", "temp", "mod" ],
    "aspect": [ "perf", "imperf" ],
    "negation": [ "neg", "aff" ],
    "mood": [ "indicative", "imperative", "conditional" ],
    "tense": [ "past", "pres", "fut" ],
    "nsyn": [ "proper", "pronoun", "common" ],
    "nsem": [ "count", "time", "mass", "measure" ],
};

rule_id = 0

def getstrval(v):
    if v is None:
        return None
    if hasattr(v, 'rule'):
        return v.rule()
    return str(v)

def ne(name, type):
    return [name, type]

def mwe(name, lemma, signature):
    return [name, lemma, signature]

def valence_rule(lemma="", pos=""):
    return f"lemma={lemma},pos2={getstrval(pos)},node=concept|sit"


def root_rule():
    return "__root__"


def rule(
    lemma=None,
    pos=None,
    case=None,
    number=None,
    gender=None,
    person=None,
    grad=None,
    praep=None,
    acm=None,
    ctype=None,
    mode=None,
    aspect=None,
    negation=None,
    mood=None,
    tense=None,
    nsyn=None,
    nsem=None
):
    if pos is None:
        raise Exception("Pos parameter is required for a rule definition.")

    for key in SUPPORTED_VALUES:
        if key in locals():
            val = getstrval(locals()[key])
            if val is not None:
                if SUPPORTED_VALUES[key] is not None:
                    if val not in SUPPORTED_VALUES[key]:
                        raise Exception(f"Invalid value '{val}' was used for '{key}' parameter. Allowed values are: {', '.join(SUPPORTED_VALUES[key])}")
    output = []
    for key in SUPPORTED_VALUES:
        if key in locals():
            val = getstrval(locals()[key])
            if val is not None:
                output.append(f"{key}={val}")
    global rule_id
    rule_id = rule_id + 1
    return f"rule_id={rule_id}|rule_id|" + ",".join(output)

class gram():
    def __init__(self, value):
        self.value = value

    def rule(self):
        return self.value

    def __add__(self, other):
        return gram(f"{self.value}+{getstrval(other)}")

    def __mul__(self, other):
        return gram(f"{self.value}*{getstrval(other)}")

    def __gt__(self, other):
        return gram(f"({self.value})/({getstrval(other)})")

    def __floordiv__(self, other):
        return gram(f"({self.value})/({getstrval(other)})")

    def __lt__(self, other):
        return gram(f"({self.value})\\({getstrval(other)})")

    def __or__(self, other):
        if not self.value.startswith("|"):
            return gram(f"|({self.value}), |({getstrval(other)})")
        else:
            return gram(f"{self.value}, |({getstrval(other)})")

    def __mod__(self, other):
        return gram(f"{self.value}\\?({getstrval(other)})")

    def __getitem__(self, key):
        rule = getstrval(key)
        case_content = ""
        if not rule.startswith("|"):
            case_content = f"|{rule}"
        else:
            case_content = rule
        case_content = "{" + case_content + "}"
        return gram(f"{self.value}{case_content}")


def quant(gr, **kwargs):
    quant_conds = []
    for key in kwargs:
        val_str = ""
        val = kwargs[key]
        if isinstance(val, list):
            val_str = "&".join(map(lambda x: getstrval(x), val))
        else:
            val_str = getstrval(val)
        quant_conds.append(f"{key}={val_str}")
    return "QUANT[" + ",".join(quant_conds) + "] " + getstrval(gr)

def optional(gr):
    return gram('?'+getstrval(gr))

infp = gram('infp')
np = gram('np')
prepnp = gram('prepnp')
adjp = gram('adjp')
ip = gram('ip')
cp = gram('cp')
ncp = gram('ncp')
advp = gram('advp')
padvp = gram('padvp')
adja = gram('adja')
prepadjp = gram('prepadjp')
compar = gram('compar')
measure = gram('measure')
num = gram('num')
aglt = gram('aglt')
aux_fut = gram('aux-fut')
aux_past = gram('aux-past')
aux_imp = gram('aux-imp')
qub = gram('qub')
interj = gram('interj')
hyphen = gram('hyphen')
int = gram('int')
rparen = gram('rparen')
rparen2 = gram('rparen2')
rquot = gram('rquot')
rquot2 = gram('rquot2')
rquot3 = gram('rquot3')
inclusion = gram('inclusion')
day_interval = gram('day-interval')
day_lex = gram('day-lex')
day_month_interval = gram('day-month-interval')
date_interval = gram('date-interval')
month_lex = gram('month-lex')
month_interval = gram('month-interval')
year_interval = gram('year-interval')
roman = gram('roman')
roman_interval = gram('roman-interval')
hour_minute_interval = gram('hour-minute-interval')
hour_interval = gram('hour-interval')
obj_id = gram('obj-id')
match_result = gram('match-result')
url = gram('url')
email = gram('email')
day_month = gram('day-month')
day = gram('day')
year = gram('year')
date = gram('date')
hour = gram('hour')
hour_minute = gram('hour-minute')
sie = gram('się')
nie = gram('nie')
by = gram('by')
s = gram('s')
or1 = gram('or')
or2 = gram('or2')
do = gram('do')
w = gram('w')
na = gram('na')
location = gram('location')
time = gram('time')
link = gram('link')
one = gram('1')

_conll = gram('<conll>')
_colon = gram('<colon>')
_speaker = gram('<speaker>')
_speaker_end = gram('<speaker-end>')
_squery = gram('<squery>')
_sentence = gram('<sentence>')
_paragraph = gram('<paragraph>')
_subst = gram('<subst>')
_depr = gram('<depr>')
_ppron12 = gram('<ppron12>')
_ppron3 = gram('<ppron3>')
_siebie = gram('<siebie>')
_prep = gram('<prep>')
_numcomp = gram('<numcomp>')
_intnum = gram('<intnum>')
_realnum = gram('<realnum>')
_intnum_interval = gram('<intnum-interval>')
_realnum_interval = gram('<realnum-interval>')
_symbol = gram('<symbol>')
_ordnum = gram('<ordnum>')
_roman_ordnum = gram('<roman-ordnum>')
_phone_number = gram('<phone-number>')
_postal_code = gram('<postal-code>')
_list_item = gram('<list-item>')
_fixed = gram('<fixed>')
_adj = gram('<adj>')
_apron = gram('<apron>')
_adjc = gram('<adjc>')
_adv = gram('<adv>')
_ger = gram('<ger>')
_pact = gram('<pact>')
_ppas = gram('<ppas>')
_fin = gram('<fin>')
_bedzie = gram('<bedzie>')
_praet = gram('<praet>')
_winien = gram('<winien>')
_impt = gram('<impt>')
_imps = gram('<imps>')
_pred = gram('<pred>')
_inf = gram('<inf>')
_pcon = gram('<pcon>')
_pant = gram('<pant>')
_comp = gram('<comp>')
_conj = gram('<conj>')
_sinterj = gram('<sinterj>')
_burk = gram('<burk>')
_interp = gram('<interp>')
_part = gram('<part>')
_unk = gram('<unk>')
_building_number = gram('<building-number>')

_root = gram('<root>')
_num = gram('<num>')
_date = gram('<date>')
_date_interval = gram('<date-interval>')
_hour_minute = gram('<hour-minute>')
_hour = gram('<hour>')
_hour_minute_interval = gram('<hour-minute-interval>')
_hour_interval = gram('<hour-interval>')
_year = gram('<year>')
_year_interval = gram('<year-interval>')
_day = gram('<day>')
_day_interval = gram('<day-interval>')
_day_month = gram('<day-month>')
_day_month_interval = gram('<day-month-interval>')
_month_interval = gram('<month-interval>')
_roman = gram('<roman>')
_roman_interval = gram('<roman-interval>')
_match_result = gram('<match-result>')
_url = gram('<url>')
_email = gram('<email>')
_obj_id = gram('<obj-id>')
_adjp = gram('<adjp>')
_adja = gram('<adja>')
_aglt = gram('<aglt>')
_qub = gram('<qub>')
_compar = gram('<compar>')
_interj = gram('<interj>')


colon = gram('colon')
speaker = gram('speaker')
speaker_end = gram('end')
squery = gram('squery')
sentence = gram('sentence')
paragraph = gram('paragraph')
subst = gram('subst')
depr = gram('depr')
ppron12 = gram('ppron12')
ppron3 = gram('ppron3')
siebie = gram('siebie')
prep = gram('prep')
numcomp = gram('numcomp')
intnum = gram('intnum')
realnum = gram('realnum')
intnum_interval = gram('interval')
realnum_interval = gram('interval')
symbol = gram('symbol')
ordnum = gram('ordnum')
roman_ordnum = gram('ordnum')
phone_number = gram('number')
postal_code = gram('code')
list_item = gram('item')
fixed = gram('fixed')
adj = gram('adj')
apron = gram('apron')
adjc = gram('adjc')
adv = gram('adv')
ger = gram('ger')
pact = gram('pact')
ppas = gram('ppas')
fin = gram('fin')
bedzie = gram('bedzie')
praet = gram('praet')
winien = gram('winien')
impt = gram('impt')
imps = gram('imps')
pred = gram('pred')
inf = gram('inf')
pcon = gram('pcon')
pant = gram('pant')
comp = gram('comp')
conj = gram('conj')
sinterj = gram('sinterj')
burk = gram('burk')
interp = gram('interp')
part = gram('part')
unk = gram('unk')
building_number = gram('number')


lemma = gram('lemma')
cat = gram('cat')
pos = gram('pos')
number = gram('number')
case = gram('case')
gender = gram('gender')
person = gram('person')
grad = gram('grad')
praep = gram('praep')
ctype = gram('ctype')
mode = gram('mode')
aspect = gram('aspect')
negation = gram('negation')
mood = gram('mood')
tense = gram('tense')
nsyn = gram('nsyn')
nsem = gram('nsem')
T = gram('T')
voc = gram('voc')
nom = gram('nom')
gen = gram('gen')
dat = gram('dat')
acc = gram('acc')
inst = gram('inst')
loc = gram('loc')
postp = gram('postp')
n = gram('n')
sg = gram('sg')


pri = gram('pri')
sec = gram('sec')
ter = gram('ter')

vint = gram('int')

abl = gram('abl')
adl = gram('adl')
locat = gram('locat')
perl = gram('perl')
dur = gram('dur')
temp = gram('temp')
mod = gram('mod')

perf = gram('perf')
imperf = gram('imperf')

neg = gram('neg')
aff = gram('aff')

indicative = gram('indicative')
imperative = gram('imperative')
conditional = gram('conditional')

past = gram('past')
pres = gram('pres')
fut = gram('fut')

proper = gram('proper')
pronoun = gram('pronoun')
common = gram('common')

count = gram('count')
mass = gram('mass')

tt = gram