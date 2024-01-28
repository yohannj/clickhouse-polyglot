from enum import Enum

class CHTypes(Enum):
    Int8 = {"name": "Int8", "fuzzing_values": [-128, 127]}
    Int16 = {"name": "Int16", "fuzzing_values": [-32768, 32767]}
    Int32 = {"name": "Int32", "fuzzing_values": [-2147483648, 2147483647]}
    Int64 = {"name": "Int64", "fuzzing_values": [-9223372036854775808, 9223372036854775807]}
    Int128 = {"name": "Int128", "fuzzing_values": [-170141183460469231731687303715884105728, 170141183460469231731687303715884105727]}
    Int256 = {"name": "Int256", "fuzzing_values": [-57896044618658097711785492504343953926634992332820282019728792003956564819968, 57896044618658097711785492504343953926634992332820282019728792003956564819967]}
    UInt8 = {"name": "UInt8", "fuzzing_values": [0, 1, 255]}
    UInt16 = {"name": "UInt16", "fuzzing_values": [0, 65535]}
    UInt32 = {"name": "UInt32", "fuzzing_values": [0, 4294967295]}
    UInt64 = {"name": "UInt64", "fuzzing_values": [0, 18446744073709551615]}
    UInt128 = {"name": "UInt128", "fuzzing_values": [0, 340282366920938463463374607431768211455]}
    UInt256 = {"name": "UInt256", "fuzzing_values": [0, 115792089237316195423570985008687907853269984665640564039457584007913129639935]}
