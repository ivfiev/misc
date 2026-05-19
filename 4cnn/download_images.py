import api
import util

board = ""
top = 100

catalog = api.catalog4(board)

if catalog is None:
    print("failed to fetch the catlog")
else:
    threads = util.parallel(lambda t: api.thread4(board, t.id), catalog[:top])
    pics = [p.image_url for _, t in threads for p in t.posts if p.image_url is not None]
    print(pics)
    print(len(pics))
    for ok, x in util.parallel(lambda p: util.download(p, f"./images/{board}/{p.split("/")[-1]}"), pics):
        print(ok)
