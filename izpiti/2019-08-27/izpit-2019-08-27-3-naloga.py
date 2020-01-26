primer_mest = [
    [(1,10),(3,-10)], # 0
    [(2,10),(5,-20)], # 1
    [(3,-10)],        # 2
    [(4,15)],         # 3
    [(5,0)]           # 4
    ]

primer_mest2 = [
    [(1,100),(3,-10)],  # 0
    [(2,10),(5,-2000)], # 1
    [(3,-10),(6,100)],  # 2
    [(4,15)],           # 3
    [(5,0)],            # 4
    [(6,0)]             # 5
    ]

from functools import lru_cache

def pobeg(odprta_mesta):

    @lru_cache(maxsize=None)
    def pobeg_iz(indeks_mesta, denar):
        if indeks_mesta >= len(odprta_mesta) and denar >= 0:
            return []
        elif indeks_mesta >= len(odprta_mesta) and denar < 0:
            return None
        else:
            mozni_pobegi = [] #ubistvu bo tu noter vedno samo en pobeg
            for pot in odprta_mesta[indeks_mesta]:
                pobeg_po_poti = pobeg_iz(pot[0], denar + pot[1])
                hitrosti_moznih_pobegov = list(map(len, mozni_pobegi))
                if pobeg_po_poti != None and (mozni_pobegi == [] or (1 + len(pobeg_po_poti)) <= min(hitrosti_moznih_pobegov)):
                    mozni_pobegi.clear()
                    pobeg_po_poti.insert(0, pot[0])
                    mozni_pobegi.append(pobeg_po_poti)
            if mozni_pobegi == []:
                return None
            else:
                return mozni_pobegi[0]
       
    uspeli_pobeg = pobeg_iz(0, 0) # pomeni start v mestu 0 z 0 denarja
    uspeli_pobeg.insert(0,0) # funkcija `pobeg_iz` ne doda zacetnega mesta odhoda
    return uspeli_pobeg

# print(pobeg(primer_mest))

# ---------------------------------------------
# uradna rešitev

def pobeg_offical(pot):

    @lru_cache(maxsize=None)
    def pobeg_aux(i, denar):
        if i >= len(pot) and denar >= 0:
            return [i]
        elif i >= len(pot):
            return None
        else:
            moznosti = []
            for (skok, stroski) in pot[i]:
                beg = pobeg_aux(skok, denar + stroski)
                if beg is not None:
                    moznosti.append(beg)
            if len(moznosti) == 0:
                return None
            else:
                return [i] + sorted(moznosti, key=len)[0]

    return pobeg_aux(0, 0)

print(pobeg(primer_mest))
print(pobeg_offical(primer_mest))
print(pobeg(primer_mest2))
print(pobeg_offical(primer_mest2))