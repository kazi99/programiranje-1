from functools import lru_cache

# Cilj: izračunajte vrednosti Fibonaccijevega zaporadja za 100, 500, 1000,
# 10**5, and 10**6 člen.
# Za vsako definicijo preizkusite kako pozne člene lahko izračuante in poglejte
# zakaj se pojavi problem (neučinkovitost, pregloboka rekurzija,
# premalo spomina ...).

# Definirajte naivno rekurzivno različico.
# Omejitev: Prepočasno.
def fib(n):
    if n == 0:
        return 1
    elif n == 1:
        return 1
    else:
        return fib(n - 1) + fib(n - 2)

# Z uporabo dekoratorja izboljšajte naivno različico.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~350.
@lru_cache(maxsize=None)
def fib_cache(n):
    if n == 0:
        return 1
    elif n == 1:
        return 1
    else:
        return fib_cache(n - 1) + fib_cache(n - 2)

# Nariši drevo klicov za navadno rekurzivno fib funkcijo pri n=5 in
# ugotovi kateri podproblemi so klicani večkrat.

# Definirajte rekurzivno memoizirano funkcijo fib brez uporabe dekoratorja.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~1000.

def fib_memo_rec(n):
    sl = dict()
    def fib(n):
        if n == 0:
            sl[0] = 1
            return 1
        elif n == 1:
            sl[1] = 1
            return 1
        else:
            if (n - 1) in sl and (n - 2) in sl:
                sl[n] = sl[n - 1] + sl[n - 2]
                return sl[n]
            elif (n - 1) in sl:
                sl[n] = sl[n - 1] + fib(n - 2)
                return sl[n]
            elif (n - 2) in sl:
                sl[n] = fib(n - 1) + sl[n - 2]
                return sl[n]
            else:
                sl[n] = fib(n - 1) + fib(n - 1)
                return sl[n]
    return fib(n)


def fib_memo_rec_(n):
    memo = dict()
    def fib_inner(i):
        if i <= 1:
            return 1
        else:
            if i in memo:
                return memo[i]
            else:
                memo[i] = fib_inner(i - 1) + fib_inner(i - 2)
                return memo[i]
    return fib_inner(n)

# Na katere podprobleme se direktno skicuje rekurzivna definicija fib?

# Definirajte fib ki gradi rezultat od spodaj navzgor (torej računa in si zapomni
# vrednosti od 1 proti n.)
def fib_memo_iter(n):
    memo = dict()
    def fib_inner(i):
        a, b = 1, 1
        for _ in range(0, i):
            a, b = b, a + b
        return a
    return fib_inner(n)

# Izboljšajte prejšnjo različico tako, da hrani zgolj rezultate, ki jih v
# nadaljevanju nujno potrebuje.
def fib_iter(n):
    pass
