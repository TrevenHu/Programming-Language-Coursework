
from misc import *
import crypt
import re

"""
Load the words from the file filename that match the regular
expression regexp.  Returns a list of matching words in the order
they are in the file.
"""
def load_words(filename,regexp):
    file = open(filename,'r')
    pattern = re.compile(regexp)
    result = []
    for line in file:
      word = line.strip() #remove the blank space
      if pattern.match(word):
        result.append(word)
    return result

"""
transform_reverse(str) 
return a list with the original string and the reversal of the original string.
"""
def transform_reverse(str):
    s = [str]
    s.append(str [::-1])
    return s

"""
transform_capitalize(str)
return a list of all the possible ways to capitalize the input string.
"""
def transform_capitalize(str):
    res = [str.lower()]
    for e in res:
      for c in e:
        for p in range(0,len(e)):
          s = e
          s = s[:p] + s[p].upper() + s[p+1:]
          if (s not in res): res.append(s)
    return res

"""
transform_digits(str)
return return a list of all possible ways to replace letters 
with similar looking digits according to the following mappings
"""
def transform_digits(str):
    res = [str]
    for e in res:
      for c in e:
        for p in range(0,len(e)):
          s = e
          ss = s[p].lower()
          m = s[p]
          if ss == 'o':
            m = '0' 
          elif ss == 'i' or ss == 'l':
            m = '1'
          elif ss == 'z':
            m = '2'
          elif ss == 'e': 
            m = '3'
          elif ss == 'a': 
            m = '4'
          elif ss == 's': 
            m = '5'
          elif ss == 'b': 
            six = s[:p] + '6' + s[p+1:]
            if six not in res: m = '6'
            else:
              eight = s[:p] + '8' + s[p+1:]
              if eight not in res: m = '8'
          elif ss == 't':
            m = '7'
          elif ss == 'g' or  ss == 'q': 
            m = '9'
          s = s[:p] + m + s[p+1:]
          if (s not in res): res.append(s)
    return res
"""
crypt(...)
    crypt(word, salt) -> string
    word will usually be a user's password. salt is a 2-character string
    which will be used to select one of 4096 variations of DES. The characters
    in salt must be either ".", "/", or an alphanumeric character. Returns
    the hashed password as a string, which will be composed of characters from
    the same alphabet as the salt.
"""
"""Check to see if the plaintext plain encrypts to the encrypted text enc"""
def check_pass(plain,enc):
    return crypt.crypt(plain,enc[:2]) == enc

"""
Load the password file filename and returns a list of
dictionaries with fields "account", "password", "UID", "GID",
"GECOS", "directory", and "shell", each mapping to the
corresponding field of the file.
"""
def load_passwd(filename):
    res = []
    file = open(filename,'r')
    for line in file:
      keys = ['account','password','UID','GID','GECOS','directory','shell']
      values = re.split('[:]',line)
      res.append(make_dict(keys,values))
    return res

"""Crack as many passwords in file fn_pass as possible using words in the file words"""
def crack_pass_file(pass_filename,words_filename,out_filename):   
    # loading files
    record = load_passwd(pass_filename)
    output = open(out_filename,'w')
    words = load_words(words_filename, r"^.{6,8}$")
    

# transform_reverse & untransformed
    for wd in words:
      for trans in transform_reverse(wd):
        for re in record:
          pw = re['password']
          account = re['account']
          if check_pass(trans, pw):
            output.write(account + '=' + trans + '\n')
            output.flush()
            record.remove(re)
            if len(record) < 1: return

    # digits
    if len(record) > 0: 
      for wd in words:
        for trans in transform_digits(wd):
          for re in record:
            pw = re['password']
            account = re['account']
            if check_pass(trans, pw):
              output.write(account + '=' + trans + '\n')
              output.flush()
              record.remove(re)
              if len(record) < 1: return

    # transform_capitalize & reverse
    if len(record) > 0: 
      for wd in words:
        for reverse in transform_reverse(wd):
          for trans in transform_capitalize(reverse):
            for re in record:
              pw = re['password']
              account = re['account']
              if check_pass(trans, pw):
                output.write(account + '=' + trans + '\n')
                output.flush()
                record.remove(re)
                if len(record) < 1: return

      # reverse & digits
      if len(record) > 0: 
        for wd in words:
          reverse = transform_reverse(wd)[1]
          for trans in transform_digits(reverse):
            for re in record:
              pw = re['password']
              account = re['account']
              if check_pass(trans, pw):
                output.write(account + '=' + trans + '\n')
                output.flush()
                record.remove(re)
                if len(record) < 1: return

      # digits & capitalize
      if len(record) > 0: 
        for wd in words:
          for digits in transform_digits(wd):
            for trans in transform_capitalize(digits):
              for re in record:
                pw = re['password']
                account = re['account']
                if check_pass(trans, pw):
                  output.write(account + '=' + trans + '\n')
                  output.flush()
                  record.remove(re)
                  if len(record) < 1: return


      if len(record) > 0: 
        for wd in words:
          for digits in transform_digits(wd):
            for caps in transform_capitalize(digits):
              for trans in transform_reverse(caps):
                for re in record:
                  pw = re['password']
                  account = re['account']
                  if check_pass(trans, pw):
                    output.write(account + '=' + trans + '\n')
                    output.flush()
                    record.remove(re)
                    if len(record) < 1: return
       
      output.close()
      return 



