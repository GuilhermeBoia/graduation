import math

n = int(input())
line = input()
circle = [int(e) for e in line.split()]
circle.sort()

pi = math.pi
area = 0

for i in range(n-1, -1, -1):
    red = (pi * (circle[i] ** 2))
    if i % 2 == 0:
        area += red
    else:
        area -= red

if area < 0:
    area = area * -1

print(f"{area:.10f}")
