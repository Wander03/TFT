from api import riotApi
from sql import database as db
from datetime import datetime
import time
from alive_progress import alive_bar
import pandas as pd


# to not update recently updated players (saves api calls)
# BOT is the puuid of the fake players in the tutorial
updated_players = {"BOT"}

def add_update_player(name: str = None, puuid: str = None, s_id: str = None):
    """
    Checks if a player is in SQL, if not add them (and update ranked data while we are at it)!

    TRY NOT TO CHECK VIA NAME, THESE CAN CHANGE
    """
    with db.engine.begin() as conn:
        # get all info (even if all is provided in case of name change)
        name, puuid, summoner_id = riotApi.get_summoner(name, puuid, s_id, all = True)
        check_player = conn.execute(db.sa.select(db.players).where(db.players.c.puuid == puuid)).fetchone()

        if(check_player is None):
            conn.execute(db.players.insert().values({"puuid": puuid, "name": name, "id": summoner_id}))

            df_rank_info = riotApi.get_rank_info(summoner_id)
            conn.execute(db.rank_info.
                        insert().
                        values({"puuid": puuid, 
                                "tier": df_rank_info["tier"][0],
                                "rank": df_rank_info["rank"][0],
                                "lp": int(df_rank_info["leaguePoints"][0]),
                                "top_fours": int(df_rank_info["wins"][0]),
                                "bot_fours": int(df_rank_info["losses"][0]),
                                "last_updated": datetime.utcnow()}))
        else:
            conn.execute(db.players.update().where(db.players.c.puuid == puuid).values({"puuid": puuid, "name": name, "id": summoner_id}))

            df_rank_info = riotApi.get_rank_info(summoner_id)
            conn.execute(db.rank_info.
                         update().
                         where(db.rank_info.c.puuid == puuid).
                         values({"puuid": puuid, 
                                "tier": df_rank_info["tier"][0],
                                "rank": df_rank_info["rank"][0],
                                "lp": int(df_rank_info["leaguePoints"][0]),
                                "top_fours": int(df_rank_info["wins"][0]),
                                "bot_fours": int(df_rank_info["losses"][0]),
                                "last_updated": datetime.utcnow()}))

    return(name, puuid, summoner_id)

def get_n_history_info(name: str = None, puuid: str = None, s_id: str = None, n: int = 1000):
    """
    Input: summonerName, puuid (optional), amount of matches to get (full = 1,000)

    Adds/updates player data in SQL.
    
    Gets as much match history from a given summoner as the RiotApi will provide. 
    Inserts match history and match info into SQL.

    Will skip already added matches.

    Output: None
    """
    if(n > 1000):
        print("The maximum amout of matches stored is 1,000")
        return

    if(n < 200):
        get = n
    else:
        get = 200

    with db.engine.begin() as conn:
        if(puuid):
            name, puuid, summoner_id = add_update_player(puuid = puuid)
        elif(name):
            name, puuid, summoner_id = add_update_player(name)
        elif(s_id):
            name, puuid, summoner_id = add_update_player(s_id = s_id)
        else:
            print("Please add identifier")

        # get past 1,000 matches (or last 2 yrs)
        id_lst = []
        flag = False
        while(len(id_lst) < n or flag):
            new_ids = riotApi.get_match_ids(name, puuid, len(id_lst), get)

            if(len(new_ids) == 0):
                flag = True
            else:
                id_lst.extend(new_ids)

        # check for already existing match_ids
        existing_ids = conn.execute(db.sa.select(db.match_history.c.match_id))
        existing_ids = list(row[0] for row in existing_ids)
        ids_to_insert = [id for id in id_lst if id not in existing_ids]

        # exit if no new data
        if(len(ids_to_insert) == 0):
            return

        # add data
        df_hist, df_info = riotApi.get_match_history_info(ids_to_insert, puuid = puuid)

        # check if all players are in players table
        print(f"Adding/Updating Players")
        with alive_bar(len(df_info)) as bar:
            for row in df_info.iterrows():
                if(row[1]["puuid"] not in updated_players):
                    add_update_player(puuid = row[1]["puuid"])
                    updated_players.add(row[1]["puuid"])
                bar()

        print(f"Adding History")
        with alive_bar(len(df_hist)) as bar:
            for val in df_hist.iterrows():
                conn.execute(db.match_history.insert().values(val[1]))
                bar()

        print(f"Adding Info")
        with alive_bar(len(df_info)) as bar:
            for val in df_info.iterrows():
                if(val[1]["puuid"] != "BOT"):
                    conn.execute(db.match_info.insert().values(val[1]))
                bar()

def crawl_chally(n: int = 1):
    """
    Gets n matchs for each current Challenger ranked players
    """
    df_chally = riotApi.get_challenger()

    for player in df_chally.iterrows():
        print("-----------------------------------------------------------------------------------")
        print(f"Player {player[0] + 1}/{len(df_chally)}: {player[1]['summonerName']}")
        get_n_history_info(s_id = player[1]["summonerId"], n = n)
    print("-----------------------------------------------------------------------------------")

def update_player(name: str = None, puuid: str = None, s_id: str = None, n: int = 1000):
    print("===================================================================================")
    start = datetime.now()
    get_n_history_info(name, puuid, s_id, n)
    print("===================================================================================")
    print(f"Began: {start}\nEnded: {datetime.now()}")
    print("===================================================================================")

def get_chally(n: int = 1):
    print("===================================================================================")
    start = datetime.now()
    crawl_chally(n)
    print("===================================================================================")
    print(f"Began: {start}\nEnded: {datetime.now()}")
    print("===================================================================================")   

if __name__ == "__main__":
    # get_chally(20)
    add_update_player("Helop")
    

# TODO: crawling to get match data | get n matchs from specificed players history (getting full 1,000 takes a while)