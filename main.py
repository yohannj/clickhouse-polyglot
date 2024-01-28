from src.ch_types import *
from src.client import Client

if __name__ == '__main__':
    client = Client(8123)

    function_names = sorted([row[0] for row in client.execute("SELECT name, is_aggregate FROM system.functions")])

    # function0 = []
    # for function_name in function_names:
    #     try:
    #         client.execute(f"SELECT {function_name}()")
    #         function0.append(function_name)
    #     except:
    #         pass

    function1 = []
    for function_name in function_names:
        valid_type = []

        for type1 in CHTypes:
            arg1 = f"{type1.value['fuzzing_values'][0]}::{type1.value['name']}"
            try:
                client.execute(f"SELECT {function_name}({arg1})")
                function1.append(function_name)
            except:
                pass

    print(function1)
