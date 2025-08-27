DEFAULT_DIFFICULTY = 3
DEFAULT_PORT = 5000
SYNC_INTERVAL = 10 

from datetime import datetime

TIMESTAMP_FORMAT = "%Y-%m-%dT%H:%M:%SZ"
GENESIS_TIMESTAMP = datetime.strptime(datetime.now().strftime(TIMESTAMP_FORMAT), TIMESTAMP_FORMAT)

with open("./.env", "w") as file:
    file.write(f"DEFAULT_DIFFICULTY={DEFAULT_DIFFICULTY}\n")
    file.write(f"DEFAULT_PORT={DEFAULT_PORT}\n")
    file.write(f"SYNC_INTERVAL={SYNC_INTERVAL}\n")
    file.write(f"TIMESTAMP_FORMAT={TIMESTAMP_FORMAT}\n")
    file.write(f"GENESIS_TIMESTAMP={GENESIS_TIMESTAMP.isoformat()}\n")