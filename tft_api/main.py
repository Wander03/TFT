import os  # used to clear terminal


class APIKey:
    def __init__(self):
        # Riot API key
        self.key = "RGAPI-ee15b75d-5681-4ec3-887d-805df96f6626"

# def screen_clear():
#     # for mac and linux
#     if os.name == 'posix':
#         _ = os.system('clear')
#     # for windows
#     else:
#         _ = os.system('cls')
#
#
# def main():
#     screen_clear()
#     region = None
#     while region not in ("americas", "asia", "europe"):
#         region = input("Please provide region: americas, asia, or europe\n")
#     summoner = input("Please provide summoner name: e.g. Wander#HENRO\n")
#     a = TftSummoner(region, summoner)
#     print(a)
#
#
# if __name__ == "__main__":
#     main()
