mocvirje1 = [2,4,1,2,1,3,1,1,5]
mocvirje2 = [4,1,8,2,11,1,1,1,1,1]

# pobeg(mocvirje) = 1 + min (pob(mocv[1:]), pob(mocv[2:]), ... pob(mocv[energija:]))

from functools import lru_cache

@lru_cache(maxsize=None)
def pobeg_aux(mocvirje, energija):
    if mocvirje == () or energija >= len(mocvirje):
        return 1
    else:
        moznosti = []
        for e in range(1, energija + 1):  # 1, 2, ... , energija
            if len(mocvirje) > e + 1:
                moznosti.append(pobeg_aux(mocvirje[e:], energija - e + mocvirje[e]))
            else:
                moznosti.append(1)
        return 1 + min(moznosti)

def pobeg(mocvirje):
    return pobeg_aux(tuple(mocvirje), mocvirje[0])

a = pobeg(mocvirje1)
b = pobeg(mocvirje2)

test_mocvirje = [10 for _ in range(50)]
test_mocvirje1 = [1 for _ in range(10)]

pobeg_aux(tuple(test_mocvirje1), 1)
pobeg([1,1,1,1])

# ------------------------------------------------
# uradna resitev

def frog_escape(swamp):
    @lru_cache(maxsize=None)
    def escape(k, e):
        if k >= len(swamp):
            return 0
        else:
            e += swamp[k]
            return 1 + min([escape(k + d, e - d) for d in range(1, e + 1)])
    return escape(0, 0)