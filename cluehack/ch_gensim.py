import itertools
import gensim.downloader as api

model = api.load('word2vec-google-news-300')

def assocs(word):
  return [w for (w, _) in model.most_similar(word, topn=20) if '_' not in w][:10]

def clues(words, n):
  ass = [assocs(w) for w in words]
  tups = itertools.product(*ass)
  cs = [(eval_group(t), t) for t in tups]
  return sorted(cs, key=lambda t: -t[0])[:n]

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

def cheat(words):
  groups = get_groups(words, 4)
  evald = map(lambda g: (eval_group(g), g), groups)
  best = sorted(evald, key=lambda t: -t[0])
  print(words)
  for g in best[:20]:
    print(g)
    for w in g[1]:
      print(assocs(w))
    print()