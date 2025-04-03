linha = input().split()
n, m = int(linha[0]), int(linha[1])

passos = 0

if m % n != 0:
    passos = -1

else:
    div = m // n
    while div > 1:
        if div % 2 != 0 and div % 3 != 0: break
        if div % 2 == 0:
            div = div // 2
            passos += 1

        if div % 3 == 0:
            div = div // 3
            passos += 1

    if div > 1:
        passos = -1

print(passos)