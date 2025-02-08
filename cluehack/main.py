from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity
import nltk
from nltk.corpus import wordnet as wn

nltk.download('wordnet', quiet=True)

def adj(word):
  ss = wn.synsets(word)
  words = set()
  for s in ss:
    for path in s.hypernym_paths():
      for h in path:
        words |= {lemma.name() for lemma in h.lemmas()}
        words |= {lemma.name() for mero in h.part_meronyms() for lemma in mero.lemmas()}
        words |= {lemma.name() for holo in h.part_holonyms() for lemma in holo.lemmas()}
        words |= {anto.name() for lemma in h.lemmas() for anto in lemma.antonyms()}
        words |= {lemma.name() for sub in h.substance_meronyms() for lemma in sub.lemmas()}
        words |= {lemma.name() for mem in h.member_meronyms() for lemma in mem.lemmas()}
  return words

print(adj('doctor') & adj('ambulance'))

# ss1 = wn.synsets('car')
# ss2 = wn.synsets('crow')
# ss3 = wn.synsets('bus')

# print(ss1[0].wup_similarity(ss2[0]))
# print(ss1[0].wup_similarity(ss3[0]))
# print(ss1[0].lowest_common_hypernyms(ss3[0]))
# print(ss1[0].lowest_common_hypernyms(ss2[0]))

# def get_wup_similarity_path(word1, word2):
#     synsets1 = wn.synsets(word1)
#     synsets2 = wn.synsets(word2)
    
#     best_score = 0
#     best_pair = None
#     best_lcs = None
    
#     for syn1 in synsets1:
#         for syn2 in synsets2:
#             score = syn1.wup_similarity(syn2)
#             if score and score > best_score:
#                 best_score = score
#                 best_pair = (syn1, syn2)
#                 best_lcs = syn1.lowest_common_hypernyms(syn2)
    
#     if not best_pair or not best_lcs:
#       print(f'No similarity path found between {word1} and {word2}!')
#       return -1

#     syn1, syn2 = best_pair
#     lcs = best_lcs[0] 
#     print(syn1.hypernym_paths()[0])
#     print(syn2.hypernym_paths()[0])
#     print(lcs)


# print(get_wup_similarity_path("dog", "wolf"))
# print(get_wup_similarity_path("car", "bicycle"))

# model = SentenceTransformer('paraphrase-MiniLM-L6-v2')

# def cmp(e1, e2):
#   return float(cosine_similarity([e1], [e2])[0][0])

# words = ['fire', 'sand', 'web']
# embeds = [model.encode(w) for w in words]

# def triples():
#   for i in range(len(embeds)):
#     for j in range(i + 1, len(embeds)):
#       yield (cmp(embeds[i], embeds[j]), words[i], words[j])

# results = sorted(list(triples()), key=lambda t: -t[0])

# print(words)
# for r in results:
#   print(r)
