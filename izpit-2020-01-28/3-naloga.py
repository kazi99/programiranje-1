
# postavitev (b,st,k) = postavitev (b - k -1, st -1, k) + postavitev (b - k - 2, st -1, k) + ...
#  pri k = 2                  |x|x|0|(podbalkon)                  |0|x|x|0|(podbalkon)

# a del

from functools import lru_cache

@lru_cache(maxsize=None)
def post(b, st, k):
    "izracuna stevilo kombinacij korit na balkonu dolzne b stevilu korit st in velikosti korit k"
    if st == 0:
        return 1
    elif st == 1:
        if b >= k:
            return b - k + 1
        else:
            return 0
    elif st > 1:
        counter = 0
        for i in range(1, b - 2 * k + 1): # ta range je ekvivalenten pogoju za vsak i je b - k - i >= k ( + 1 je tam zaradi range(n) = 0,1,...n-1)
            if post(b - k - i, st - 1, k) != 0:
                counter += post(b - k - i, st - 1, k)
        return counter

post(5,2,2)
post(9,3,2)

# poskus b)

# def zmesana_post(b, sez):
#     najvecje_korito = max(sez)
#     st_korit = len(sez)

#     def aux(b, kotita):
#         if najvecje_korito > b:
#             return 0
#         else:
#             for 


#     return aux(b, tuple(sez))
