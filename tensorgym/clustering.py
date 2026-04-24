from datasets import load_dataset
from sentence_transformers import SentenceTransformer
from umap.umap_ import UMAP
from hdbscan import HDBSCAN
from bertopic import BERTopic

dataset = load_dataset("maartengr/arxiv_nlp")["train"]

abstracts = dataset["Abstracts"]
titles = dataset["Titles"]

# https://huggingface.co/spaces/mteb/leaderboard
# embedding_model = SentenceTransformer("microsoft/harrier-oss-v1-270m")
embedding_model = SentenceTransformer("thenlper/gte-small")
embeddings = embedding_model.encode(abstracts, show_progress_bar=True, convert_to_numpy=True)

umap_model = UMAP(
    n_components=5,
    min_dist=0.0,
    metric="cosine",
)
reduced_embeddings = umap_model.fit_transform(embeddings)

hdbscan_model = HDBSCAN(
    min_cluster_size=50,
    metric="euclidean",
    cluster_selection_method="eom",
).fit(reduced_embeddings)
clusters = hdbscan_model.labels_

topic_model = BERTopic(
    embedding_model=embedding_model,
    umap_model=umap_model,
    hdbscan_model=hdbscan_model,
    verbose=True,
).fit(abstracts, embeddings)

print(topic_model.get_topic_info())

fig = topic_model.visualize_documents(
    titles,
    reduced_embeddings=reduced_embeddings,
    width=1200,
    hide_annotations=True,
)
fig.show()

# there exist other visualisations
# reranking + llm to summarize
