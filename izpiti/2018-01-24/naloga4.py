primer = [
    [2, 4, 1, 1],
    [3, 2, 0, 5],
    [8, 0, 7, 2]
]

primer2 = [
    [2, 4, 1, 1],
    [0, 0, 0, 500],
    [8, 0, 7, 2]
]

import random
big_primer = [random.sample(range(0,100),100) for _ in range(100)]

from functools import lru_cache

def pustolovscina(primer, poskusi):
    dolzina_vrst = len(primer[0])
    st_vrst = len(primer)

    @lru_cache(maxsize=None)
    def kraja(vrsta, st, poskusi):
        if poskusi == 0:
            return 0
        elif vrsta >= st_vrst: # lahko bi tudi vrsta == st_vrst ker skacem samo po eno vrsto dol
            return 0
        else:
            if st + 1 == dolzina_vrst: # ce bo naslednji skok ze izven vrste, gre v naslednjo
                return primer[vrsta][st] + kraja(vrsta + 1, 0, poskusi - 1)
            elif st < dolzina_vrst and vrsta < st_vrst: # splosni primer
                # to se odlocamo ali bi skocli eno desno ali pa eno dol
                return primer[vrsta][st] + max(kraja(vrsta, st + 1, poskusi - 1), kraja(vrsta + 1, 0, poskusi - 1))
            else: # nic druga se ne bo zgodilo
                pass
    
    return kraja(0,0,poskusi)

#--------------------------------------------
# uradna rešitev

def max_points(matrix, max_steps):

    @lru_cache(maxsize=None)
    def jumper(r, c, k):
        val = matrix[r][c]
        # No more steps
        if (k == 0):
            return 0
        # Hit boundaries
        elif (r == len(matrix) - 1):
            # Can't go down
            if (c == len(matrix[r]) - 1):
                # Can't go right
                return val
            else:
                # Can go right
                return val + jumper(r, c+1, k-1)
        else:
            # Can go down
            if (c == len(matrix[r]) - 1):
                # Can't go right
                return val + jumper(r+1, 0, k-1)
            else:
                # Can go right
                return val + max(jumper(r, c+1, k-1), jumper(r+1, 0, k-1))

    # Call function
    return jumper(0, 0, max_steps)