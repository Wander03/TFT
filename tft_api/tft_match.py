from main import APIKey
import pandas as pd
import requests
import json


class Match(APIKey):
    def __init__(self, match_id: str) -> None:
        super().__init__()
        self.match_id = match_id

    def get_match(self):
        response = requests.get("https://americas.api.riotgames.com/tft/match/v1/matches/%s?api_key=%s" % (
            self.match_id, self.key))
        match = json.loads(response.text)
        df_all_players_ = pd.json_normalize(pd.json_normalize(match)["info.participants"][0])
        return df_all_players_
