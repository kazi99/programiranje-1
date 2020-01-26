# 3. naloga

# a)

def simetricen(niz):
    if len(niz) in [0,1]:
        return True
    elif niz[0] != niz[-1]:
        return False
    else:
        return simetricen(niz[1:-1])

# b)

zap = "00101011"

"""
    zap = 00101011
    1 + min (
    if sim 0 -> st_del(0101011)
    if sim 00 -> st_del(101011)
    if sim 001 -> st_del(01011)
     ...
    if sim 00101011
    )
"""

from functools import lru_cache

@lru_cache(maxsize=None)
def stevilo_delov(zapestnica):
    if simetricen(zapestnica):
        return 1
    else:
        moznosti = []
        for i in range(1, len(zapestnica)):
            if simetricen(zapestnica[:i]):
                moznosti.append(stevilo_delov(zapestnica[i:]))
        return 1 + min(moznosti)

# c)

@lru_cache(maxsize=None)
def razdeli_sim(zapestnica):
    if simetricen(zapestnica):
        return [zapestnica]
    else:
        mozne_delitve = []
        for i in range(1, len(zapestnica)):
            delitev = []
            if simetricen(zapestnica[:i]):
                delitev.append(zapestnica[:i])
                delitev += razdeli_sim(zapestnica[i:])
                dolzine = list(map(len, mozne_delitve))
                if mozne_delitve == [] or (len(delitev) <= min(dolzine)):
                    mozne_delitve.clear()
                    mozne_delitve.append(delitev)
        return mozne_delitve[0]
        
razdeli_sim("0100")

import random
def test_zap_gen(n):
    zapestnica = []
    for i in range(n):
        zapestnica.append("{}".format(random.randint(0,1)))
    return "".join(zapestnica)

# d)

def zap_v_sez(zap):
    return [int(c) for c in zap]

def vsotno_simetricen(zapestnica):
    zap = zap_v_sez(zapestnica)
    k = len(zap) // 2
    levi = zap[:k]
    desni = zap[k:]
    return sum(levi) == sum(desni)

vsotno_simetricen("1101010")

# e)

@lru_cache(maxsize=None)
def razdeli(zapestnica, f): 
    if f(zapestnica):
        return [zapestnica]
    else:
        mozne_delitve = []
        for i in range(1, len(zapestnica)):
            delitev = []
            if f(zapestnica[:i]):
                delitev.append(zapestnica[:i])
                delitev += razdeli(zapestnica[i:], f)
                dolzine = list(map(len, mozne_delitve))
                if mozne_delitve == [] or (len(delitev) <= min(dolzine)):
                    mozne_delitve.clear()
                    mozne_delitve.append(delitev)
        return mozne_delitve[0]

print(razdeli("00101011", simetricen))
print(razdeli("00101011", vsotno_simetricen))

print(razdeli("11010100", simetricen))
print(razdeli("11010100", vsotno_simetricen))