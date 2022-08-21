from account import Account
import requests
import json


class TftSummoner(Account):
    def __init__(self, region: str, summoner_name: str) -> None:
        super().__init__(region, summoner_name)

    def get_matches(self, n):
        response = requests.get(
            "https://americas.api.riotgames.com/tft/match/v1/matches/by-puuid/%s/ids?count=%d&api_key=%s" % (
                self.puuid, n, self.key))
        return json.loads(response.text)
