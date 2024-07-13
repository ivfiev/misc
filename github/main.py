import urllib.request
import sys
import json
from datetime import datetime

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

with open(path, 'w') as file:
  json.dump(data, file)


