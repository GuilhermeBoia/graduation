n, x = map(int, input().split())
coins = list(map(int, input().split()))

count = [0] * (x+1)
count[0] = 1
m = (10**9) + 7

for i in range(1, x+1):
    for c in coins:
        if (i-c) >= 0:
            count[i] += count[i-c]
            count[i] %= m
            
print(count[x])