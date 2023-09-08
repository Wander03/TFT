import os
import sqlalchemy as sa
from dotenv import load_dotenv
import retry


def database_connection_url():
    load_dotenv()
    escapedPassword = os.environ.get("DB_PASSWORD")
    sqldialect = os.environ.get("DB_DIALECT")
    username = os.environ.get("DB_USER")
    database = os.environ.get("DB_NAME")
    host = os.environ.get("DB_HOST")
    port = os.environ.get("DB_PORT")
    return f"{sqldialect}://{username}:{escapedPassword}@{host}:{port}/{database}"


# Retry code if server connection fails (does this even work?)
# pool_pre_ping to fix timeout issue
@retry.retry(exceptions=sa.exc.SQLAlchemyError, tries=10, delay=5)
def retry_database_connection():
    # Attempt to create the database engine
    return sa.create_engine(database_connection_url(), pool_pre_ping=True)

# Create a new DB engine based on our connection string
engine = retry_database_connection()

# Create a metadata object for each table
metadata_obj = sa.MetaData()

# Load tables
match_history = sa.Table("match_history", metadata_obj, autoload_with=engine)
match_info = sa.Table("match_info", metadata_obj, autoload_with=engine)
players = sa.Table("players", metadata_obj, autoload_with=engine)
rank_info = sa.Table("rank_info", metadata_obj, autoload_with=engine)
