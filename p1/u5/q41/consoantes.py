# Guilherme Dantas Boia de Albuquerque
# Prog1 | UFCG - 2022.1
# Mais Consoantes

count = 0
while True:
    palavra = input()

    num_vogais = 0
    for letra in palavra:
        if letra in "aeiou" or letra in "AEIOU":
            num_vogais += 1

    count += 1
    num_consoantes = len(palavra) - num_vogais
    if num_consoantes > num_vogais: break

print(count)
