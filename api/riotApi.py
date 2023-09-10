import os
from dotenv import load_dotenv
import requests
from ratelimit import limits, sleep_and_retry
import json
import pandas as pd
import time
from alive_progress import alive_bar


load_dotenv()
api_key = os.environ.get("RIOT_KEY")

@sleep_and_retry
@limits(calls=100, period=130)
def call_api(url):
    response = requests.get(url)

    if response.status_code != 200:
        print('API response: {}'.format(response.status_code))

    return response

def get_summoner(name: str = None, puuid: str = None, s_id: str = None, all: bool = False):
    """
    Input: summonerName and/or puuid (for NA), what you want returned from the function
    Output: summonerName and puuid and summonerId or just puuid
    """
    if(name):
        response = call_api(
            f"https://na1.api.riotgames.com/tft/summoner/v1/summoners/by-name/{name}?api_key={api_key}"
            )
        
    elif(puuid):
        response = call_api(
            f"https://na1.api.riotgames.com/tft/summoner/v1/summoners/by-puuid/{puuid}?api_key={api_key}"
            )
    elif(s_id):
        response = call_api(
            f"https://na1.api.riotgames.com/tft/summoner/v1/summoners/{s_id}?api_key={api_key}"
            )
    else:
        print("Please add an identifier")
    
    player = json.loads(response.text)

    if(all):
        return(pd.json_normalize(player)["name"][0], pd.json_normalize(player)["puuid"][0], pd.json_normalize(player)["id"][0])
    else:
        return(pd.json_normalize(player)["puuid"][0])

def get_rank_info(summonder_id: str):
    """
    Input: summonerId
    Output: dataframe of players ranked data
    """
    response = call_api(f"https://na1.api.riotgames.com/tft/league/v1/entries/by-summoner/{summonder_id}?api_key={api_key}")

    rank_info = response.json()
    df_rank_info = pd.json_normalize(rank_info)

    # if unranked (1: no ranking in anything; 2: ranking in hyperroll but not ranked)
    if(len(df_rank_info) == 0):
        return(pd.DataFrame({"tier": ["UNRANKED"], "rank": ["I"], "leaguePoints": [0], "wins": [0], "losses": [0]}))
    if((df_rank_info[df_rank_info["queueType"] == "RANKED_TFT"]).empty):
        return(pd.DataFrame({"tier": ["UNRANKED"], "rank": ["I"], "leaguePoints": [0], "wins": [0], "losses": [0]}))

    df_rank_info = df_rank_info[df_rank_info["queueType"] == "RANKED_TFT"]
    df_rank_info.reset_index(drop=True, inplace=True)

    return(df_rank_info[["tier", "rank", "leaguePoints", "wins", "losses"]])

def get_queue_from_id(id):
    queue_dict = {
        1090: "NORMAL",
        1100: "RANKED",
        1110: "TUTORIAL",
        1160: "DOUBLE UP",
        1170: "FORTUNE'S FAVOR",
        1180: "SOUL BRAWL",
    }

    return(queue_dict[id] if id in queue_dict else "UNKNOWN")
    
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

    response = call_api(f"https://americas.api.riotgames.com/tft/match/v1/matches/by-puuid/{puuid}/ids?start={start}&count={count}&api_key={api_key}")

    return(response.json())

def get_match_history(match_ids: list):
    """
    Input: list of match ids
    Output: Data Frame of past num match ids, match datetime, match length, tft set number, game type (ranked, double up, etc.), game version (patch)
    """
    matches = pd.DataFrame()
    count = 0

    for id in match_ids:
        response = call_api(f"https://americas.api.riotgames.com/tft/match/v1/matches/{id}?api_key={api_key}")

        match = response.json()
        df = pd.json_normalize(match["info"])
        df["match_id"] = id
        df["tft_game_type"] = get_queue_from_id(df["queue_id"][0])
        matches = pd.concat([matches, df], ignore_index=True)
        count += 1

        print(f"Getting History: {count} ({round(count / len(match_ids), 2) * 100}%)")

    return(matches[["match_id", "game_datetime", "game_length", "tft_set_number", "tft_set_core_name", "tft_game_type", "game_version"]])

def get_match_info(match_ids: list):
    """
    Input: list of match ids
    Output: Data Frame of data from match ids in provided list
    """
    matches = pd.DataFrame()
    count = 0

    for id in match_ids:
        response = call_api(f"https://americas.api.riotgames.com/tft/match/v1/matches/{id}?api_key={api_key}")
        match = response.json()
        df = pd.json_normalize(match["info"]["participants"])
        df["match_id"] = id
        matches = pd.concat([matches, df], ignore_index=True)
                                
        count += 1
        print(f"Getting Info: {count} ({round(count / len(match_ids), 2) * 100}%)")


    matches['augment_1'] = matches['augments'].apply(lambda x: x[0] if len(x) > 0 else None)
    matches['augment_2'] = matches['augments'].apply(lambda x: x[1] if len(x) > 1 else None)
    matches['augment_3'] = matches['augments'].apply(lambda x: x[2] if len(x) > 2 else None)

    matches['traits'] = matches['traits'].apply(lambda x: x[0] if len(x) > 0 else None)
    matches['units'] = matches['units'].apply(lambda x: x[0] if len(x) > 0 else None)

    return(matches[["match_id", "puuid", "placement", "level", "last_round", "gold_left", "players_eliminated", 
                    "total_damage_to_players", "augment_1", "augment_2", "augment_3", "traits", "units"]])

def get_match_history_info(match_ids: list, name: str = None, puuid: str = None):
    """
    Does the function of both get_match_history and get_match_info in 1, cutting api calls in half!

    Requires either a name or puuid!
    """
    if(puuid is None and name is not None):
        puuid = get_summoner(name)
    
    df_match_history = pd.DataFrame()
    df_match_info = pd.DataFrame()

    print("Getting History/Info")
    with alive_bar(len(match_ids)) as bar:
        for id in match_ids:
            response = call_api(f"https://americas.api.riotgames.com/tft/match/v1/matches/{id}?api_key={api_key}")

            match = response.json()
            df_hist = pd.json_normalize(match["info"])
            df_info = pd.json_normalize(match["info"]["participants"])
            df_hist["match_id"] = id
            df_hist["tft_game_type"] = get_queue_from_id(df_hist["queue_id"][0])
            df_info["match_id"] = id

            df_match_history = pd.concat([df_match_history, df_hist], ignore_index=True)
            df_match_info = pd.concat([df_match_info, df_info], ignore_index=True)

            bar()

    df_match_info['augment_1'] = df_match_info['augments'].apply(lambda x: x[0] if len(x) > 0 else None)
    df_match_info['augment_2'] = df_match_info['augments'].apply(lambda x: x[1] if len(x) > 1 else None)
    df_match_info['augment_3'] = df_match_info['augments'].apply(lambda x: x[2] if len(x) > 2 else None)

    df_match_info['traits'] = df_match_info['traits'].apply(lambda x: x[0] if len(x) > 0 else None)
    df_match_info['units'] = df_match_info['units'].apply(lambda x: x[0] if len(x) > 0 else None)

    return(df_match_history[["match_id", "game_datetime", "game_length", "tft_set_number", "tft_set_core_name", "tft_game_type", "game_version"]], 
           df_match_info[["match_id", "puuid", "placement", "level", "last_round", "gold_left", "players_eliminated", 
                    "total_damage_to_players", "augment_1", "augment_2", "augment_3", "traits", "units"]])


def get_challenger():
    """
    Output: Data Frame of Challenger players
    """
    response = call_api(
                f"https://na1.api.riotgames.com/tft/league/v1/challenger?api_key={api_key}"
                )

    response = response.json()["entries"]

    entries = pd.json_normalize(response)

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
