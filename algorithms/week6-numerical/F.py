n, l, r = map(int, input().split())

d = []

for i in range(3):
    d.append(((r - i) // 3) - ((l - i - 1) // 3)) 

mod = 10 ** 9 + 7
a, b, c = d[0], d[1], d[2]
for _ in range(1, n):
    temp1 = (a * d[0] + b * d[2] + c * d[1]) % mod
    temp2 = (a * d[1] + b * d[0] + c * d[2]) % mod
    temp3 = (a * d[2] + b * d[1] + c * d[0]) % mod
    a, b, c = temp1, temp2, temp3

print(a)