import os
import sqlalchemy as sa
from dotenv import load_dotenv


def database_connection_url():
    load_dotenv()
    escapedPassword = os.environ.get("DB_PASSWORD")
    sqldialect = os.environ.get("DB_DIALECT")
    username = os.environ.get("DB_USER")
    database = os.environ.get("DB_NAME")
    host = os.environ.get("DB_HOST")
    port = os.environ.get("DB_PORT")
    return f"{sqldialect}://{username}:{escapedPassword}@{host}:{port}/{database}"


# Create a new DB engine based on our connection string
engine = sa.create_engine(database_connection_url())

# Create a metadata object for each table
metadata_obj = sa.MetaData()

# Load tables
my_match_history = sa.Table("my_match_history", metadata_obj, autoload_with=engine)
my_match_info = sa.Table("my_match_info", metadata_obj, autoload_with=engine)
