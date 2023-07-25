import os
from dotenv import load_dotenv


def get_API_key():
    load_dotenv()

    # Riot API key
    return os.environ.get("RIOT_KEY")
