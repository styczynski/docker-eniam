# -*- encoding: utf-8 -*-
__author__ = 'nika'

import random, time
random.seed(time.time())

class Node(object):
    def __init__(self, _args):
        # nid="11" from="3" to="5" subtrees="1" chosen="false"
        self.nid = _args["nid"]
        self.fro = int(_args["from"])
        self.to = int(_args["to"])
        self.subtress = int(_args["subtrees"])
        if "chosen" in _args and _args["chosen"] == "false":
            self.chosen = False
        else:
            self.chosen = True
        self.terminal = False
        self.nonterminal = False
        self.arguments = {}
        self.category = u'terminal'
        self.children = []
        self.parents = {}

    def __unicode__(self):
        return "NID: "+unicode(self.nid)+" "+unicode(self.arguments)

    def getFromToCat(self):
        return unicode(self.getFrom()) + '@' + unicode(self.getTo()) + '@' +unicode(self.getCategory())

    def getArgDict(self, prefix):
        d = {}
        d[prefix+'@'+unicode(self.category)] = 1
        #print self.nid, self.category
        for key in self.arguments.keys():
            if key != "rekcja" and key != "poz":# and not(key == 'ink' and self.category != 'zdanie'):
                d[prefix+'@'+key+'@'+self.arguments[key]] = 1
        return d

    def make_exp_data(self, leaves,dom):
        d = {}
        for ii in range(len(leaves)):
            try:
                d['1gram@base@'+ leaves[ii]["base"]]  = 1
            except KeyError:
                pass
            for jj in leaves[ii]["tag"].split(':'):
                d['1gram@tag@'+jj] = 1
            #if ii < len(leaves)-1:
            #    #d['2gram@tag@' + leaves[ii]["tag"].split(':')[0] + '@' +leaves[ii+1]["tag"].split(':')[0]] = 1
            #    try:
            #        d['2gram@base@' + dom[leaves[ii]["base"]] + '@' + dom[leaves[ii+1]["base"]]] = 1
            #    except KeyError:
            #        pass
            
            #if ii < len(leaves)-2:
            #    d['3gram@tag@' + leaves[ii]["tag"].split(':')[0] + '@' +leaves[ii+1]["tag"].split(':')[0] + '@' + leaves[ii+2]["tag"].split(':')[0]] = 1
        return d

    def isTerminal(self):
        return self.terminal

    def isChosen(self):
        return self.chosen

    def addChildren(self, child):
        self.children.append(child)

    def addArgument(self, arg, value):
        self.arguments[arg] = value

    def setTerminal(self):
        self.terminal = True

    def setNonterminal(self):
        self.nonterminal = True

    def setCategory(self, _cat):
        self.category = _cat

    def getRandomChildren(self):
        return self.children[random.randint(0,len(self.children)-1)]

    def getAllChildren(self):
        #print self.children
        return self.children

    def getChildrenForActPcfg(self):
        ch = [(self.children['centre'], u'true')]
        ch += map(lambda x: (x,u'false'), self.children['productions'])
        return ch

    def getRawChildren(self):
        ch = [(self.children['centre'], u'true')]
        ch += map(lambda x: (x,u'false'), self.children['productions'])
        return ch

    def getCategory(self):
        cat = unicode(self.category)
        if self.category == "fw":
            cat += '@' + unicode(self.arguments['tfw'])
        if "przypadek" in self.arguments.keys():
            pass#cat += '@' + unicode(self.arguments["przypadek"])
        if "rodzaj" in self.arguments.keys():
            pass#cat += '@' + unicode(self.arguments["rodzaj"])
        if "liczba" in self.arguments.keys():
            pass#cat += '@' + unicode(self.arguments["liczba"])
        if "osoba" in self.arguments.keys():
            pass#cat += '@' + unicode(self.arguments["osoba"])
        return cat

    def getRodzaj(self):
        if 'rodzaj' in self.arguments.keys():
            return self.arguments['rodzaj']
        else:
            return None

    def getLiczba(self):
        if 'liczba' in self.arguments.keys():
            return self.arguments['liczba']
        else:
            return None

    def getOsoba(self):
        if 'osoba' in self.arguments.keys():
            return self.arguments['osoba']
        else:
            return None


    def getExtCategory(self):
        #if self.category == "fw":
        #    return unicode(self.category) + '@' + unicode(self.arguments['tfw']) + '@' + unicode(self.getRodzaj()) + '@' + unicode(self.getLiczba()) + '@' +unicode(self.getOsoba())
        return unicode(self.getCategory())# + '@' + unicode(self.getRodzaj()) + '@' + unicode(self.getLiczba()) + '@' +unicode(self.getOsoba())

    def getNode(self):
        return self.getCategory(), self.children

    def getArguments(self):
        return self.arguments

    def getID(self):
        return self.nid

    def getFrom(self):
        return self.fro

    def getTo(self):
        return self.to

    def equals(self, node):
        equal = True
        for arg in self.arguments.keys():
            if arg in node.arguments.keys():
                if not self.arguments[arg] == node.arguments[arg]:
                    equal = False
                    break
            else:
                equal = False
                break
        return equal and self.equals_from_to_cat(node)

    def equals_from_to_cat(self, node):
        return self.getFrom() == node.getFrom() and self.getTo() == node.getTo() and self.getCategory() == node.getCategory()

    def equals_from_to(self, node):
        return self.getFrom() == node.getFrom() and self.getTo() == node.getTo()

    def overlaps(self, node):
        #print self.getFrom(), self.getTo(), node.getFrom(), node.getTo(), self.getFrom() <= node.getTo() and self.getTo() >= node.getFrom()
        return self.getFrom() <= node.getTo() and self.getTo() >= node.getFrom()





