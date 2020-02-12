#!/usr/bin/env python
import random
with open("samples.txt","w") as f:

    t = 100
    power = 7

    f.write(str(t) + "\n")
    p = 982451653
    for i in range(t):
        try:
            n = random.randint(1, min(10**power, p-1) )
            k = random.randint(1,n - 1)
        except:
            pass

        f.write("{} {} {}\n".format(n,k,p))
