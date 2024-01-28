from src.ch_types import *
from src.client import Client

from multiprocessing.pool import Pool

def fuzz_function0(function_name):
    try:
        client.execute(f"SELECT {function_name}()")
        return True
    except:
        return False

def fuzz_function1(function_name):
    valid_types = []

    for type1 in CHTypes:
        arg1 = f"{type1.value['fuzzing_values'][0]}::{type1.value['name']}"
        try:
            client.execute(f"SELECT {function_name}({arg1})")
            valid_types.append(type1)
        except:
            pass

    return valid_types

if __name__ == '__main__':
    client = Client(8123)

    function_names = sorted([row[0] for row in client.execute("SELECT name, is_aggregate FROM system.functions")])

    with Pool(4) as p:
        res = zip(
            function_names,
            p.map(fuzz_function0, function_names)
        )
        function0 = [function_name for function_name, is_function0 in res if is_function0]
        print(function0)

        res = zip(
            function_names,
            p.map(fuzz_function1, function_names)
        )
        function1 = [function_name for function_name, input_types_list in res if input_types_list]
        print(function1)
