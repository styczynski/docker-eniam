#! /usr/bin/python
# *-* coding: utf-8 *-*

import codecs 
import requests
import json
import time
import os

from subprocess import Popen, PIPE



class Concraft(object):
    def __init__(self, model_name=None, model_path=None, concraft_path='./', port=3000, core=5):
        self.path = os.path.join(model_path, model_name)
        self.concraftpath = os.path.join(concraft_path, 'concraft-dag2-pl')
        self.port = port
	self.core = core
        self.concraftserver = Popen([self.concraftpath, 'server', '--port=%d' % port, '-i', self.path, '+RTS', '-N%d' % core],
                              stdin=PIPE, stdout=PIPE, stderr=PIPE)
        print(u"Concraft model " + model_name + u" loading…")
        loaded = False
        while not loaded:
            try:
                request_data = {'dag':''}
                r = requests.post('http://localhost:%d/parse' % port, data=json.dumps(request_data))
                loaded = True
                #print(u"loaded!")
            except requests.ConnectionError as e:
                #print(u"loading…")
                time.sleep(2)
   
    def tag(self, dag):
        json_string = ''
        for item in dag:
            num1, num2, (forma, lemat, tag, posp, kwal) = item
            line_string = '\t'.join((str(num1), str(num2), forma, lemat, tag, ','.join(posp), ','.join(kwal), '0.000', '', '' + '\n'))
            json_string = json_string + line_string
        analyse_list = []
        if json_string != '':
            request_data = {'dag':json_string + '\n'}
            r = requests.post('http://localhost:%d/parse' % self.port, data=json.dumps(request_data))                          
            for line in r.json()['dag'].split('\n'):
                if line != '':
                    num1, num2, forma, lemat, tag, posp, kwal, prob, interp, eos, disamb = line.strip('\n').split('\t')
                    eos = 'eos' if eos else None
                    disamb = 'disamb' if disamb else None
                    posp = posp.split(',') if posp else []
                    kwal = kwal.split(',') if kwal else []
                    interp = ''
                    analyse_list.append((int(num1), int(num2), (forma, lemat, tag, posp, kwal), prob, eos, disamb))
                else:
                    continue
        return analyse_list
   
    def done(self):
        self.concraftserver.terminate()

