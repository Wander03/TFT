from riotApi import *
import time

p = "4pnGHS5xYfTogigUBzqp-jtPuCKs02UNXI82obZiA6Qdxw6hP5hvQOhHQF-qio4F3NIWS2SUQXnc5Q"

# count = 0
# f = True
# while(f == True):
#     print(update_match_history("Wanderr", 0, 2000))
#     time.sleep(1)
#     count += 1
#     f = False
# print(count)

# a = get_match_info(["NA1_4759160721"])
# print(a["traits"][0])

# print(get_challenger())

a = get_match_ids(puuid=p, start = 0, count = 300)
print(len(a))
