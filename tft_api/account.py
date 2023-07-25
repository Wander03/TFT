from riotApi import APIKey
from typing import Optional
import pandas as pd
import numpy as np
import requests
import json
from pandas.io.json import json_normalize
import time


class Account(APIKey):
    def __init__(self, region: str, summoner_name: str) -> None:
        super().__init__()
        self.region = region
        self.summoner_name = summoner_name
        self.puuid = self._get_puuid()

    def __repr__(self) -> str:
        return ("region: " + self.region
                + "\nsummoner_name: " + self.summoner_name
                + "\npuuid: " + self.puuid
                )

    def _get_puuid(self) -> Optional[str]:
        try:
            name, tag = self.summoner_name.split(sep="#")

            response = requests.get(
                "https://%s.api.riotgames.com/riot/account/v1/accounts/by-riot-id/%s/%s?api_key=%s" % (
                    self.region, name, tag, self.key))
            player = json.loads(response.text)

            return pd.json_normalize(player)["puuid"][0]
        except:
            print("Player not found.")
