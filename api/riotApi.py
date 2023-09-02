import os
from dotenv import load_dotenv
import requests
import json
import time
import pandas as pd


load_dotenv()
api_key = os.environ.get("RIOT_KEY")


def get_puuid(name: str):
    """
    Input: summonerName (for NA)
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


def update_match_history(name: str, start:int = 0, count: int = 20):
    """
    Input: summonerName, amount of past matches to collect
    Output: Data Frame of past num match ids, match datetime, match length, tft set number, game type (ranked, double up, etc.), game version (patch)
    """
    puuid = get_puuid(name)

    response = requests.get(
                f"https://americas.api.riotgames.com/tft/match/v1/matches/by-puuid/{puuid}/ids?start={start}&count={count}&api_key={api_key}"
                )
    
    if(response.status_code != 200):
        print("Getting Match Ids")
        print(response.status_code)
        exit()

    ids = response.json()
    matches = pd.DataFrame()

    for id in ids:
        response = requests.get(f"https://americas.api.riotgames.com/tft/match/v1/matches/{id}?api_key={api_key}")

        if(response.status_code != 200):
            print("Getting Matches")
            print(response.status_code)
            exit()

        match = response.json()
        df = pd.json_normalize(match["info"])
        df["match_id"] = id
        matches = pd.concat([matches, df], ignore_index=True)

    return(matches[["match_id", "game_datetime", "game_length", "tft_set_number", "tft_set_core_name", "tft_game_type", "game_version"]])


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


def get_match_info(name: str, match_ids: list):
    """
    Input: summonerName, list of match ids
    Output: Data Frame of name's data from match ids in provided list
    """
    puuid = get_puuid(name)

    matches = pd.DataFrame()
    count = 0

    for id in match_ids:
        response = requests.get(f"https://americas.api.riotgames.com/tft/match/v1/matches/{id}?api_key={api_key}")
        match = response.json()
        df = pd.json_normalize(match["info"]["participants"])
        df["match_id"] = id
        matches = pd.concat([matches, df[df["puuid"] == puuid]], ignore_index=True)
        count += 1
        if(count % 99 == 0):
            print(f"Completed: {count} ({count - 99}, {count})")
            time.sleep(121)

    return(matches[["match_id", "placement", "level", "last_round", "gold_left", "players_eliminated", "total_damage_to_players", "traits", "units", "augments"]])


def common_traits(traits: dict):
    """
    Input: Dictionary of traits
    Output: List of dominant trait(s) in Dictionary
    """
    pass


# def get_stats(name: str, num: int = 20):
#     """
#     Input: summonerName, amount of past n matches
#     Output: statistics about past n matches
#     """
#     df_hist = get_match_history(name, int)

    
#     df_stats = pd.DataFrame(
#         avg_placement = df_hist["placement"].mean(),
#         avg_level = df_hist["level"].mean(),
#         most_played_traits = 
#         )
