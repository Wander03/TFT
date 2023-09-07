import os
from dotenv import load_dotenv
import requests
import json
import time
import pandas as pd


load_dotenv()
api_key = os.environ.get("RIOT_KEY")


def get_summoner(name: str = None, puuid: str = None, r_puuid: bool = True, r_name: bool = False):
    """
    Input: summonerName and/or puuid (for NA), what you want returned from the function
    Output: summonderName and/or puuid (or nothing if you really want)
    """
    try:
        if(name):
            response = requests.get(
                f"https://na1.api.riotgames.com/tft/summoner/v1/summoners/by-name/{name}?api_key={api_key}"
                )
            
        else:
            response = requests.get(
                f"https://na1.api.riotgames.com/tft/summoner/v1/summoners/by-puuid/{puuid}?api_key={api_key}"
                )
        
        player = json.loads(response.text)

        if(r_name and r_puuid):
            return(pd.json_normalize(player)["name"][0], pd.json_normalize(player)["puuid"][0])
        elif(r_puuid):
            return(pd.json_normalize(player)["puuid"][0])
        elif(r_name):
            return(pd.json_normalize(player)["name"][0])
    except:
        print("Player not found")

def get_match_ids(name: str = None, puuid: str = None, start: int = 0, count: int = 20):
    """
    Input: summonerName, puuid (optional, reduced API calls), amount of past matches to collect (max 200)
    Output: List of match ids
    """
    if(count > 200):
        print("API can only return 200 match ids at a time")
        return

    if(puuid is None):
        puuid = get_summoner(name)

    response = requests.get(
                f"https://americas.api.riotgames.com/tft/match/v1/matches/by-puuid/{puuid}/ids?start={start}&count={count}&api_key={api_key}"
                )
    
    if(response.status_code != 200):
        print("Error Getting Match Ids")
        print(response.status_code)
        return

    return(response.json())

def get_match_history(match_ids: list):
    """
    Input: list of match ids
    Output: Data Frame of past num match ids, match datetime, match length, tft set number, game type (ranked, double up, etc.), game version (patch)
    """
    matches = pd.DataFrame()
    count = 0

    for id in match_ids:
        response = requests.get(f"https://americas.api.riotgames.com/tft/match/v1/matches/{id}?api_key={api_key}")

        if(response.status_code != 200):
            print(f"Error Getting Matches\nCollected: {count}")
            print(response.status_code)
            return(matches[["match_id", "game_datetime", "game_length", "tft_set_number", "tft_set_core_name", "tft_game_type", "game_version"]])

        match = response.json()
        df = pd.json_normalize(match["info"])
        df["match_id"] = id
        matches = pd.concat([matches, df], ignore_index=True)
        count += 1

        if(count % 99 == 0):
            print(f"Completed: {count} ({round(count / len(match_ids), 2) * 100}%)")
            time.sleep(121)

    return(matches[["match_id", "game_datetime", "game_length", "tft_set_number", "tft_set_core_name", "tft_game_type", "game_version"]])

def get_match_info(match_ids: list, name: str = None, puuid: str = None):
    """
    Input: ist of match ids, can filter by summonerName/puuid
    Output: Data Frame of name's data from match ids in provided list
    """
    if(puuid is None and name is not None):
        puuid = get_summoner(name)

    matches = pd.DataFrame()
    count = 0

    for id in match_ids:
        response = requests.get(f"https://americas.api.riotgames.com/tft/match/v1/matches/{id}?api_key={api_key}")
        match = response.json()
        df = pd.json_normalize(match["info"]["participants"])
        df["match_id"] = id

        if(name is not None and puuid is not None):
            matches = pd.concat([matches, df[df["puuid"] == puuid]], ignore_index=True)
        else:
            matches = pd.concat([matches, df], ignore_index=True)
                                
        count += 1
        if(count % 99 == 0):
            print(f"Completed: {count} ({round(count / len(match_ids), 2) * 100}%)")
            time.sleep(121)

    matches['augment_1'] = matches['augments'].apply(lambda x: x[0] if len(x) > 0 else None)
    matches['augment_2'] = matches['augments'].apply(lambda x: x[1] if len(x) > 1 else None)
    matches['augment_3'] = matches['augments'].apply(lambda x: x[2] if len(x) > 2 else None)

    matches['traits'] = matches['traits'].apply(lambda x: x[0] if len(x) > 0 else None)
    matches['units'] = matches['units'].apply(lambda x: x[0] if len(x) > 0 else None)

    return(matches[["match_id", "placement", "level", "last_round", "gold_left", "players_eliminated", 
                    "total_damage_to_players", "augment_1", "augment_2", "augment_3", "traits", "units"]])

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


# def common_traits(traits: dict):
#     """
#     Input: Dictionary of traits
#     Output: List of dominant trait(s) in Dictionary
#     """
#     pass


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
