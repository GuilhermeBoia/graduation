n = int(input())
linha = input()
nums = [int(e) for e in linha.split()]

laranja = 0
for e in nums:
    laranja += e

percentual = laranja / n

print(f"{percentual:.12f}")