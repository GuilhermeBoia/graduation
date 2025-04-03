import math
r = int(input())

printou = False
stop = int((math.sqrt(r)) // 2)

for x in range(1, stop + 1):
    if (r - (1 + x**2 + x)) % 2 == 0:
        y = (r - (1 + x**2 + x)) // 2
        if y > 0:
            print(x, y)
            printou = True
            exit()

if not printou:
    print("NO")