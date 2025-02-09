import gensim.downloader as api

model = api.load('word2vec-google-news-300')

def assocs(word):
  one = model.most_similar(word, topn=20)
  two = {t[0] for w in one for t in model.most_similar(w[0], topn=10)}
  return two

def clues(word1, word2, n):
  a1 = assocs(word1)
  a2 = assocs(word2)
  cs = [(model.similarity(a10, a20), a10, a20) for a10 in a1 for a20 in a2]
  return sorted(cs, key=lambda t: -t[0])[:n]

print(clues('wolf', 'dog', 20))