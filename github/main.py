import urllib.request
import sys
import json

user = sys.argv[1]
token = sys.argv[2]

github_repos_req = urllib.request.Request(f'https://api.github.com/users/{user}/repos')
github_repos_req.add_header('Authorization', f'token {token}')

response = urllib.request.urlopen(github_repos_req)

contents = json.load(response)

names = list(map(lambda x: x['name'], contents))

print(names)
