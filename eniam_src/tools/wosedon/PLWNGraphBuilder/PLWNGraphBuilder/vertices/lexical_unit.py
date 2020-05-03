class LexicalUnit:
  def __init__(self, lu_id, lemma, pos, domain, variant, lexicon = None):
      self.lu_id = lu_id
      self.lemma = lemma
      self.pos = pos
      self.domain = domain
      self.variant = variant
      self.lexicon = lexicon
