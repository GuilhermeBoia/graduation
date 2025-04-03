n, x = map(int, input().split())
coins = list(map(int, input().split()))

valor = [float("inf")] * (x+1)
valor[0] = 0

for i in range(1, x+1):
    valor[x] = float("inf")
    for c in coins:
        if (i-c) >= 0 and valor[i-c]+1 < valor[x]:
            valor[i] = valor[i-c]+1
            
if valor[x] == float("inf"):
    print(-1)
else:
    print(valor[x])