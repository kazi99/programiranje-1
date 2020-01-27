from functools import lru_cache

###############################################################################
# Napisite funkcijo [najdaljse_narascajoce_podazporedje], ki sprejme seznam in
# poisce najdaljse (ne strogo) narascajoce podzaporedje stevil v seznamu.
#
# Na primer: V seznamu [2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9] je najdaljse naj vrne
# rezultat [2, 3, 4, 4, 6, 7, 8, 9].
###############################################################################


def najdaljse_narascajoce_podzaporedje(sez):
    return None

###############################################################################
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zaplejati

# Robotek se lahko premika le gor, dol, levo in desno, ter ima omejeno količino
# goriva. Napišite funkcijo [pobeg], ki sprejme matriko, ki predstavlja sobo,
# začetno pozicijo in pa število korakov, ki jih robotek lahko naredi z
# gorivom, in izračuna ali lahko robotek pobegne. Soba ima vedno vsaj eno
# polje.
#
# Na primer za laboratorij:
# [[0, 1, 0, 0, 2],
#  [0, 2, 2, 0, 0],
#  [0, 0, 2, 2, 0],
#  [2, 0, 0, 2, 0],
#  [0, 2, 2, 0, 0],
#  [0, 0, 0, 2, 2]]
#
# robotek iz pozicije (3, 1) pobegne čim ima vsaj 5 korakov, iz pozicije (5, 0)
# pa v nobenem primeru ne more, saj je zagrajen.
###############################################################################

soba = [[0, 1, 0, 0, 2],
        [0, 2, 2, 0, 0],
        [0, 0, 2, 2, 0],
        [2, 0, 0, 2, 0],
        [0, 2, 2, 0, 0],
        [0, 0, 0, 2, 2]]


def pobeg(matrika, vr, st, gorivo):
    vrstice = len(matrika)
    stolpci = len(matrika[0])

    @lru_cache(maxsize=None)
    def pobeg_aux(pair, gorivo): 
        vr = pair[0]
        st = pair[1]

        if matrika[vr][st] == 1 and gorivo >= 0:
            return True
        elif gorivo < 0:
            return False
        else:
            def pogoji(pair):
                vr = pair[0]
                st = pair[1]
                if vr < 0 or vr >= vrstice or st < 0 or st >= stolpci:
                # vse koordinate matrike bodo od tu naprej v pravilnih mejah
                    return "zid"
                elif  matrika[vr][st] == 2:
                    return "zid"
                elif matrika[vr][st] == 0 or matrika[vr][st] == 1:
                    return "prosto"
                else: # ni druge moznosti
                    return "zid" 

#                          levo           dol          desno          gor
            moznosti = [(vr, st - 1), (vr + 1, st), (vr, st + 1), (vr - 1, st)]
            situacija = list(map(pogoji, moznosti))

            moznosti_pobega = []
            for i in [0,1,2,3]:
                if situacija[i] == "prosto":
                    moznosti_pobega.append(pobeg_aux(moznosti[i], gorivo - 1))

            return (True in moznosti_pobega)
    
    return pobeg_aux((vr, st), gorivo)

print(pobeg(soba, 1, 0, 100))