from api import riotApi
from sql import database as db
import time
import pandas as pd


# pd.set_option("display.max_columns", None)


def save_new_matches(num = 10):
    with db.engine.begin() as conn:
        # get matches
        df_hist = riotApi.update_match_history("Wanderr", num)
        dict_hist = df_hist.to_dict(orient="records")

        # check for already existing match_ids
        existing_ids = conn.execute(db.sa.select(db.match_history.c.match_id))
        existing_ids = set(row[0] for row in existing_ids)

        data_to_insert = [data for data in dict_hist if data['match_id'] not in existing_ids]

        # add new matches
        for val in data_to_insert:
            conn.execute(db.match_history.insert().values(val))

if __name__ == "__main__":
    save_new_matches(100)
    time.sleep(1)


# TODO: get matches and get data from matches (new table)