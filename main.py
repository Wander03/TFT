from api import riotApi
from sql import database as db
import pandas as pd


# pd.set_option("display.max_columns", None)


def main():
    with db.engine.begin() as conn:
        df_hist = riotApi.update_match_history("Wanderr", 6)
        dict_hist = df_hist.to_dict(orient="records")

        for val in dict_hist:
            conn.execute(db.match_history.insert().values(val))

if __name__ == "__main__":
    main()


# TODO: modify update match history to add all new matches and not crash if a match is already added