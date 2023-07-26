import os
from dotenv import load_dotenv
import requests
import json
import pandas as pd


load_dotenv()
api_key = os.environ.get("RIOT_KEY")


def get_puuid(name: str):
    """
    Input: summonerName
    Output: puuid
    """
    try:
        response = requests.get(
            f"https://na1.api.riotgames.com/lol/summoner/v4/summoners/by-name/{name}?api_key={api_key}"
            )
        
        player = json.loads(response.text)

        return(pd.json_normalize(player)["puuid"][0])
    except:
        print("Player not found.")

def get_match_ids(name: str, num: int):
    """
    Input: summonerName, amount of past matches to collect
    Output: List of past num match ids
    """
    puuid = get_puuid(name)

    response = requests.get(
                f"https://americas.api.riotgames.com/tft/match/v1/matches/by-puuid/{puuid}/ids?count={num}&api_key={api_key}"
                )

    return(response.json())

def get_challenger():
    """
    Output: Data Frame of Challenger players ordered from most to least lp
    """
    response = requests.get(
                f"https://na1.api.riotgames.com/tft/league/v1/challenger?api_key={api_key}"
                )

    response = response.json()["entries"]

    entries = pd.json_normalize(response).sort_values("leaguePoints", ascending=False)

    return(entries)

def get_match_history(name: str, num: int):
    """
    Input: summonerName, amount of past n matches
    Output: Data Frame of name's data from past num matches
    """
    match_ids = get_match_ids(name, num)
    puuid = get_puuid(name)

    matches = pd.DataFrame()
    for id in match_ids:
        response = requests.get(f"https://americas.api.riotgames.com/tft/match/v1/matches/{id}?api_key={api_key}")
        match = response.json()
        df = pd.json_normalize(match["info"]["participants"])
        matches = pd.concat([matches, df[df["puuid"] == puuid]], ignore_index=True)

    return(matches)
    
