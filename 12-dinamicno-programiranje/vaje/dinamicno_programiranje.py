test_matrix = [[ 1 ,2 ,0 ], [ 2 ,4 ,5 ], [ 7 ,0 ,1 ]]

# def max_cheese(cheese):
#     height = len(cheese)
#     width = len(cheese[0])
#     dol = [cheese[i] for i in range(1,height)]
#     desno = [[cheese[i][j] for j in range(1, width)] for i in range(height)]

#     return cheese[0][0] + max(desno[0][0], dol[0][0])

from functools import lru_cache

def max_cheese(cheese):
    max_row = len(cheese[0])
    max_col = len(cheese)

    @lru_cache(maxsize=None)
    def max_index(row, col):
        if row == max_row or col == max_col:
            return 0

        return cheese[row][col] + max(
            max_index(row + 1, col), 
            max_index(row, col + 1)
        )

    return max_index(0,0)

print(max_cheese(
    [[j for j in range(20)] for _ in range(20)]
))

print(max_cheese(
    [[j for j in range(200)] for _ in range(200)]
))

test_matrix = [[1,2,0], [2,4,5], [7,0,1]]

def max_cheese(cheese): 
    height = len(cheese)
    width = len(cheese[0])
    memo = [[0 for _ in range(width)] for _ in range (height)]

    for i in range(height - 1, -1, -1):
        for j in range(width - 1, -1, -1):

            dol = 0 if i == height - 1 else memo[i + 1][j]
            desno = 0 if j == width - 1 else memo[i][j + 1]

            memo[i][j] = cheese[i][j] + max(desno, dol)
            
    print(memo)
    return memo[0][0]

max_cheese(test_matrix)

"""
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.
"""

def alternating_towers(height):
    @lru_cache(maxsize=None)
    def modri(visina):
        if visina <= 1:
            return 0
        elif visina == 2: # (mm) -> 1 
            return 1
        elif visina == 3: # (mmm), (mm)(r) -> 2
            return 2
        else:
            return rdeci(visina - 3) + rdeci(visina - 2)

    @lru_cache(maxsize=None)
    def rdeci(visina):
        if visina <= 0:
            return 0
        elif visina == 1: # (r) -> 1
            return 1
        elif visina == 2: # (rr) -> 1
            return 1
        else:
            return modri(visina - 1) + modri(visina - 2)

    return modri(height) + rdeci(height)    


def f(x):
    if x == 0:
        return 1
    else:
        return g(x)

def g(x):
    if x == 1:
        return 1
    else:
        return f(x)

f(1)