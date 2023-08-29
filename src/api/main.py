import riotApi
import database as db
import pandas as pd


# pd.set_option("display.max_columns", None)


def main():
    with db.engine.begin() as conn:
        df_hist = riotApi.update_match_history("Wanderr", 5)
        dict_hist = df_hist.to_dict(orient="records")
        print(dict_hist)
        # match_values = []

        # for val in match_values:
        #     conn.execute(db.match_history.insert().values())

if __name__ == "__main__":
    main()