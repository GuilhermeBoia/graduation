def gcd(a, b):
    if a == 0:
        return b
    return gcd(b % a, a)

n = int(input())
array = list(map(int, input().split()))

resposta = [array[0]]
adiciona = 0
for i in range(0, len(array) - 1):
    p = array[i]
    s = array[i+1]
    if gcd(p, s) != 1:
        adiciona += 1
        resposta.append(1)
    resposta.append(s)

print(adiciona)
for e in resposta:
    print(e, end=" ")