from api import riotApi
from sql import database as db
import time
import pandas as pd


def save_new_matches(start:int = 0, count:int = 10):
    with db.engine.begin() as conn:
        # get matches
        df_hist = riotApi.update_match_history("Wanderr", start, count)
        dict_hist = df_hist.to_dict(orient="records")

        # check for already existing match_ids
        existing_ids = conn.execute(db.sa.select(db.my_match_history.c.match_id))
        existing_ids = set(row[0] for row in existing_ids)

        data_to_insert = [data for data in dict_hist if data['match_id'] not in existing_ids]

        # add new matches
        for val in data_to_insert:
            conn.execute(db.my_match_history.insert().values(val))

def save_new_match_info():
    with db.engine.begin() as conn:
        # # get matche ids
        # match_ids = conn.execute(db.sa.select(db.my_match_history.c.match_id))
        # match_ids = set(row[0] for row in match_ids)

        # # check for already existing match_ids
        # existing_ids = conn.execute(db.sa.select(db.my_match_info.c.match_id))
        # existing_ids = set(row[0] for row in existing_ids)

        # new_ids = match_ids - existing_ids

        # get info
        df_info = riotApi.get_match_info("Wanderr", ["NA1_4759160721"])
        with pd.option_context('display.max_rows', None, 'display.max_columns', None):
            print(df_info["traits"][0])
        # add new info
        # df_info.to_sql("my_match_info", conn, if_exists = "append", index = False)
            
#conn.execute(db.my_match_info.insert().values(val))
def get_full_history():
    start = 0
    count = 99
    debugging = 1

    while(start < 1000):
        save_new_matches(start, count)
        print(f"Complete: {debugging} ({start}, {start + count})")
        debugging += 1
        start += count
        time.sleep(121)

if __name__ == "__main__":
    # updates "my_match_history"
    # get_full_history()

    # updates "my_match_info"
    # RUN get_full_history() first!!!
    save_new_match_info()
    


# TODO: figure out augments and stuff :)