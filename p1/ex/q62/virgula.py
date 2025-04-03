# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Espaço por vírgula


frase = input()
i = int(input())
j = int(input())

nova = ""
for k in range(i, j):
    if frase[k] == " ":
        nova += ","
    else:
        nova += frase[k]
    if k < j - 1:
        nova += " "

print(nova)
