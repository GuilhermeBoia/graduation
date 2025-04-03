n = int(input())
sequencia = list(map(int, input().split()))

prefix_sum = [0] * (n + 1)
for i in range(1, n + 1):
    prefix_sum[i] = prefix_sum[i - 1] + sequencia[i - 1]

for _ in range(n):
    p1, p2 = map(int, input().split())
    soma = prefix_sum[p2] - prefix_sum[p1 - 1]
    print(soma)