import itertools
import gensim.downloader as api
from gensim.models import KeyedVectors

# model = api.load('glove-wiki-gigaword-300')
model = KeyedVectors.load_word2vec_format('glovewiki300.bin', binary=True)

def assocs(word, topn=10):
  return [w for (w, _) in model.most_similar(word, topn=topn) if '_' not in w]

def clues(words, a=10, n=5):
  ass = [assocs(w, a) for w in words]
  tups = itertools.product(*ass)
  gs = [(eval_group(t), t) for t in tups]
  cs = sorted(gs, key=lambda t: -t[0])[:n]
  for c in cs:
    print(c)

def eval_group(words):
  sum = 0.0
  for i in range(len(words)):
    for j in range(i + 1, len(words)):
      sum += model.similarity(words[i], words[j])
  return round(sum * 2 / len(words) / (len(words) - 1), 3)

def get_groups(words, max):
  gs = []
  for n in range(2, max + 1):
    gs.extend(itertools.combinations(words, n))
  return gs

def cheat(*words):
  groups = get_groups(words, 4)
  evald = map(lambda g: (eval_group(g), g), groups)
  best = sorted(evald, key=lambda t: -t[0])
  for g in best[:5]:
    print(g)
    for w in g[1]:
      print(assocs(w, 5))
    print()
  
def guess(clue, words):
  pairs = [(round(model.similarity(w, clue), 3), w) for w in words]
  best = sorted(pairs, key=lambda t: -t[0])
  for w in best[:8]:
    print(w)

def wordnet_common():
  pass