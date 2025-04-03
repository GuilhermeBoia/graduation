def e_primo(num):
    if num < 1:
        return False
    if num <= 3:
        return True
    if num % 2 == 0 or num % 3 == 0:
        return False
    i = 5
    while i * i <= num:
        if num % i == 0 or num % (i + 2) == 0:
            return False
        i += 6
    return True

def pares(n):
    for a in range(2, (n // 2) + 1):
        b = n - a
        if e_primo(a) and e_primo(b):
            return (a, b)
    return None

alvo = int(input())
result = pares(alvo)
if result:
    print(f"{result[0]} {result[1]}")
else:
    print("-1")