from src.client import Client

if __name__ == '__main__':
    client = Client(8123)

    client.execute("SELECT name FROM system.functions")
