#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

"""
Return the element of the list l closest in value to v.  I
n the case of a tie, the first such element is returned.  
If l is empty, None is returned.
"""
def closest_to(l,v):
  if l == None: return None
  else:
    mini = (abs(l[0]-v),0)
    for i,e in enumerate(l):
      if abs(e-v) < mini[0]: mini = (abs(e-v),i)
  return l[mini[1]]

"""Return a dictionary pairing corresponding keys to values."""
def make_dict(keys,values):
  dic = {}
  for i,k in enumerate(keys):
    dic[k] = values[i]
  return dic
   
# file IO functions

"""
Open the file fn and return a dictionary mapping words to the number
of times they occur in the file.  A word is defined as a sequence of
alphanumeric characters and _.  All spaces and punctuation are ignored.
Words are returned in lower case
"""
def word_count(fn):
    news = open(fn,'r')
    split = re.compile('[\W]')
    word_cnt = {}
    for line in news:
      words = split.split(line.lower())
      for w in words:
        if w in word_cnt:
          word_cnt[w] += 1
        else: word_cnt[w] = 1
    del(word_cnt['']) 
    return word_cnt









