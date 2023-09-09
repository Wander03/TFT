from api import riotApi
from sql import database as db
from datetime import datetime
import time
import pandas as pd


def add_update_player(name: str = None, puuid: str = None):
    """
    Checks if a player is in SQL, if not add them (and update ranked data while we are at it)!
    """
    with db.engine.begin() as conn:
        # get/add puuid
        if(puuid is None):
            check_player = conn.execute(db.sa.select(db.players).where(db.players.c.name == name)).fetchone()
        else:
            check_player = conn.execute(db.sa.select(db.players).where(db.players.c.puuid == puuid)).fetchone()

        flag = False
        if(check_player is None):
            if(name is not None):
                name, puuid, summoner_id = riotApi.get_summoner(name, all = True)
            else:
                name, puuid, summoner_id = riotApi.get_summoner(puuid = puuid, all = True)
            conn.execute(db.players.insert().values({"puuid": puuid, "name": name, "id": summoner_id}))
            flag = True
        else:
            puuid = check_player[0]
            summoner_id = check_player[2]

        # add/update rank info
        df_rank_info = riotApi.get_rank_info(summoner_id)
        if(flag):
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

# def crawl_chally():
#     """
#     Gets match history 
#     """
#     df_chally = riotApi.get_challenger()

#     with db.engine.begin() as conn:
#         for player in df_chally.iterrows():
#             name = player[1]["summonerName"]
#             check_player = conn.execute(db.sa.select(db.players).where(db.players.c.name == name)).fetchone()
#             print("|", name, "|")
#             return
#             flag = False
#             if(check_player is None):
#                 name, puuid, summoner_id = riotApi.get_summoner(name, all = True)
#                 conn.execute(db.players.insert().values({"puuid": puuid, "name": name, "id": summoner_id}))
#                 flag = True
#             else:
#                 puuid = check_player[0]
#                 summoner_id = check_player[2]

#             # add/update rank info
#             df_rank_info = player[1]

#             if(flag):
#                 conn.execute(db.rank_info.
#                             insert().
#                             values({"puuid": puuid, 
#                                     "tier": "CHALLENGER",
#                                     "rank": df_rank_info["rank"],
#                                     "lp": df_rank_info["leaguePoints"],
#                                     "top_fours": df_rank_info["wins"],
#                                     "bot_fours": df_rank_info["losses"],
#                                     "last_updated": datetime.utcnow()}))
#             else:
#                 conn.execute(db.rank_info.
#                             update().
#                             where(db.rank_info.c.puuid == puuid).
#                             values({"puuid": puuid, 
#                                     "tier": "CHALLENGER",
#                                     "rank": df_rank_info["rank"],
#                                     "lp": df_rank_info["leaguePoints"],
#                                     "top_fours": df_rank_info["wins"],
#                                     "bot_fours": df_rank_info["losses"],
#                                     "last_updated": datetime.utcnow()}))
                
#             print(f"Adding Chally: {player[0]} ({round(player[0] / len(df_chally), 2) * 100}%)")


def get_full_history_info(name: str = None, puuid: str = None):
    """
    Input: summonerName

    Adds/updates player data in SQL.
    
    Gets as much match history from a given summoner as the RiotApi will provide. 
    Inserts match history and match info into SQL.

    Will skip already added matches.

    Output: None
    """
    with db.engine.begin() as conn:
        name, puuid, summoner_id = add_update_player(name)

        # get past 1,000 matches (or last 2 yrs)
        id_lst = []
        flag = False
        while(len(id_lst) < 1000 or flag):
            new_ids = riotApi.get_match_ids(name, puuid, len(id_lst), 200)

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
            pass

        # add data
        df_hist, df_info = riotApi.get_match_history_info(ids_to_insert, puuid = puuid)

        # check if all players are in players table
        for row in df_info.iterrows():
            add_update_player(puuid = row[1]["puuid"])
            print(f"Adding/Updating Players: {row[0]} ({round(row[0] / len(df_info), 2) * 100}%)")

        for val in df_hist.iterrows():
            conn.execute(db.match_history.insert().values(val[1]))
            print(f"Adding History: {val[0]} ({round(val[0] / len(df_hist), 2) * 100}%)")

        for val in df_info.iterrows():
            conn.execute(db.match_info.insert().values(val[1]))
            print(f"Adding Info: {val[0]} ({round(val[0] / len(df_info), 2) * 100}%)")
        
# def get_full_match_info(name: str):
#     with db.engine.begin() as conn:
#         # get matche ids
#         match_ids = conn.execute(db.sa.select(db.match_history.c.match_id))
#         match_ids = set(row[0] for row in match_ids)

#         # check for already existing match_ids
#         existing_ids = conn.execute(db.sa.select(db.my_match_info.c.match_id))
#         existing_ids = set(row[0] for row in existing_ids)

#         new_ids = match_ids - existing_ids

#         # get info
#         df_info = riotApi.get_match_info(name, new_ids)

#         # add new info
#         for val in df_info.iterrows():
#             conn.execute(db.my_match_info.insert().values(val[1]))

if __name__ == "__main__":
    print("===================================================================================")
    start = datetime.now()
    get_full_history_info("Wanderr")
    print("===================================================================================")
    print(f"Began: {start}\nEnded: {datetime.now()}")
    print("===================================================================================")
    

# TODO: crawling to get match data | get n matchs from specificed players history (getting full 1,000 takes a while)