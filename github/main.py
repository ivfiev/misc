import urllib.request
import sys
import json
from datetime import datetime
import matplotlib.pyplot as plt

user = sys.argv[1]
token = sys.argv[2]
path = sys.argv[3]

with open(path, 'r') as file:
  data = json.load(file)

github_repos_req = urllib.request.Request(f'https://api.github.com/users/{user}/repos')
github_repos_req.add_header('Authorization', f'token {token}')

response = urllib.request.urlopen(github_repos_req)
contents = json.load(response)
names = list(map(lambda x: x['name'], contents))

today = datetime.today().strftime('%Y-%m-%d')
data[today] = 0
points = []

for name in names:
  github_views_req = urllib.request.Request(f'https://api.github.com/repos/{user}/{name}/traffic/views')
  github_views_req.add_header('Authorization', f'token {token}')
  response = urllib.request.urlopen(github_views_req)
  contents = json.load(response)
  for obj in contents['views']:
    new_obj = {'date': obj['timestamp'].split('T')[0], **obj}
    points.append(new_obj)

for point in points:
  data[point['date']] = 0

for point in points:
  data[point['date']] += point['count']

data = {key: data[key] for key in sorted(data)}

with open(path, 'w') as file:
  json.dump(data, file)

xs = data.keys()
ys = data.values()

fig, ax = plt.subplots(figsize=(12, 8))

for (x, y) in zip(xs, ys):
  ax.annotate(f'{y}', (x, y), textcoords='offset points', xytext=(0, 10), ha='center')

ax.plot(xs, ys, marker='o')

plt.xticks(rotation=90)
plt.show()
