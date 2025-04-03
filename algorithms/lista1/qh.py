import math
n = int(input())

count, count4, count7 = math.inf, 0, 0
for i in range(n):
    if i * 4 > n or (n-(i*4)) % 7 != 0:
        continue
    j = (n-i*4) // 7
    if i + j < count:
        count, count4, count7 = i+j, i, j

if count == math.inf:
    print(-1)

else:
    for i in range(count4):
        print("4", end="")
    for j in range(count7):
        if j == count7 - 1:
            print("7")
        else:
            print("7", end="")
