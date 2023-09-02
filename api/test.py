from riotApi import *
import time

# count = 0
# f = True
# while(f == True):
#     print(update_match_history("Wanderr", 0, 2000))
#     time.sleep(1)
#     count += 1
#     f = False
# print(count)

a = get_match_info("Wanderr", ["NA1_4759160721"])
print(a.columns)


