import requests

class Client(object):

    def __init__(self, port: int) -> None:
        self.url = f"http://localhost:{port}"

    def execute(self, query: str) -> object:
        return self.execute_full_response(query)['data']
    
    def execute_full_response(self, query: str) -> object:
        return requests.get(f"{self.url}/?query={query} FORMAT JSONCompact").json()
