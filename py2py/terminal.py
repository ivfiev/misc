import json
import os


def run():
    while True:
        model = json.loads(input())
        os.system('clear')
        for node, edges in model['graph'].items():
            print(node, ' -> ', ','.join(edges))
        print(f'\nDead: [{','.join(model['dead'])}]')


if __name__ == '__main__':
    run()
