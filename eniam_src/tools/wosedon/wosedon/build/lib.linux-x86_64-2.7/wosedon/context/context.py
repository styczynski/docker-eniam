#
# <one line to give the library's name and an idea of what it does.>
# Copyright (C) 2014  <copyright holder> <email>
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
#

import abc
import corpus2

class Context(object):
  def __init__(self, tagset, ccl_doc, ccl_rel, str_name):
    self._ccl_doc = ccl_doc
    self._ccl_rel = ccl_rel
    self._str_name = str_name
    self._tagset = tagset

  def __str__(self):
    return self._str_name
  
  def ccl_document(self):
    return self._ccl_doc
  
  def ccl_relations(self):
    return self._ccl_rel

  @abc.abstractmethod
  def reset(self):
    raise NotImplementedError("Method \"reset\" in Context is virtual!")

  @abc.abstractmethod
  def next(self):
    '''
    Implementation of @next metho should to return WSDContext
    containing tokens with ordering same as in the text
    '''
    raise NotImplementedError("Method \"next\" in Context is virtual!")

class WSDContext(object):
  '''
  Kontekst dla metody ujednoznaczniajacej tekst.
  W sklad kontekstu wchodzi lista tokentow oraz mozliwe 
  operacje na kontekscie.
  '''
  def __init__(self, tagset, tokens):
    self._tokens = tokens
    self._tagset = tagset
    self._full_pos_mask = corpus2.get_attribute_mask(tagset, '') 
  
  def tagset(self):
    return self._tagset
  
  def tokens(self):
    return self._tokens

  def get_token_lemma_str(self, token):
    return str(token.get_preferred_lexeme(self._tagset).lemma())

  def get_token_coarse_pos(self, token):
    tag = token.get_preferred_lexeme(self._tagset).tag()
    pos_mask = tag.get_masked(self._full_pos_mask)
    return self.convert_to_coarse_pos(
      self._tagset.tag_to_symbol_string(pos_mask))

  def convert_to_coarse_pos(self, pl_pos):
    noun_pl_pos = ['subst', 'depr', 'ger', 'brev']
    adj_pl_pos = ['adj', 'adja', 'adjp', 'adjc']
    verb_pl_pos = ['fin', 'bedzie', 'praet', 'impt', \
                   'inf', 'pcon', 'pant', 'imps', \
                   'winien', 'pred', 'pact', 'ppas', 'pred']
    pos_str = None
    if pl_pos in noun_pl_pos:
      pos_str = 'n'
    elif pl_pos in adj_pl_pos:
      pos_str = 'a'
    elif pl_pos in verb_pl_pos:
      pos_str = 'v'
    return pos_str

  def convert_num_pos_to_coarse_str(self, pos_int):
    pos_str = None
    if pos_int == 1:
      #verb
      pos_str = 'v'
    elif pos_int == 2:
      #subst
      pos_str = 'n'
    elif pos_int == 3:
      #adv
      pos_str = 'r'
    elif pos_int == 4:
      #adj
      pos_str = 'a'
    elif pos_int == 5:
      #PWN verb
      pos_str = 'v-PWN'
    elif pos_int == 6:
      #PWN subst
      pos_str = 'n-PWN'
    elif pos_int == 7:
      #PWN adv
      pos_str = 'r-PWN'
    elif pos_int == 8:
      #PWN adj
      pos_str = 'a-PWN'
    return pos_str
