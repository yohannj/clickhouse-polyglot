import requests

class Client(object):

    def __init__(self, port: int) -> None:
        self.url = f"http://localhost:{port}"

    def execute(self, query: str) -> object:
        return self.execute_full_response(query)['data']
    
    def execute_full_response(self, query: str) -> object:
        r = requests.get(f"{self.url}/?query={query} FORMAT JSONCompact")
        if r.status_code == 200:
            return r.json()
        else:
            raise Exception(f"ClickHouse returned a non 200 response: {r}")
