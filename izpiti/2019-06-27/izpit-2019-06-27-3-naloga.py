ZABOJNIKI = [1,3,4,7,10]

# nacini = natovarjanje(nosilnost - 1) + natovarjanje(nosilnot - 3) ... 

from functools import lru_cache

@lru_cache(maxsize=None)
def nacini(nosilnost, max_zabojnik):
    if nosilnost == 0 or nosilnost < max_zabojnik:
        return 0
    else:
        counter = 0
        preostali_zabojniki = [x for x in ZABOJNIKI if x >= max_zabojnik]
        for zabojnik in preostali_zabojniki:
            if nosilnost - zabojnik == 0:
                counter += 1
            elif nosilnost - zabojnik > 0:
                counter += nacini(nosilnost - zabojnik, zabojnik)
        return counter

def st_natovarjanj(nosilnost):
    return nacini(nosilnost, min(ZABOJNIKI))

nacini(4,1)

@lru_cache(maxsize=None)
def vsa_natovarjanja(nosilnost):
    def vsi_nacini(nosilnost, max_zabojnik):
        if nosilnost == 0 or nosilnost < max_zabojnik:
            return [[]]
        else:
            trenutni_nacini = []
            preostali_zabojniki = [x for x in ZABOJNIKI if x >= max_zabojnik]
            for zabojnik in preostali_zabojniki:
                if nosilnost - zabojnik == 0:
                    trenutni_nacini.append([zabojnik])                    

                elif nosilnost - zabojnik > 0:
                    prejsnji_nacini = vsi_nacini(nosilnost - zabojnik, max_zabojnik) 
                    for nacin in prejsnji_nacini:
                        nacin.insert(0, zabojnik)
                        nacin.sort()
                        if nacin not in trenutni_nacini:
                            trenutni_nacini.insert(0, nacin)
                    
            return trenutni_nacini 

    return vsi_nacini(nosilnost, min(ZABOJNIKI))

vsa_natovarjanja(4)

k = 3
print(st_natovarjanj(k) == len(vsa_natovarjanja(k)))