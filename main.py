from src.client import Client

if __name__ == '__main__':
    client = Client(8123)

    function_names = sorted([row[0] for row in client.execute("SELECT name, is_aggregate FROM system.functions")])

    function0 = []
    for function_name in function_names:
        try:
            client.execute(f"SELECT {function_name}()")
            function0.append(function_name)
        except:
            pass

    print(function0)
