# Universidade Federal de Campina Grande
# Ciência da Computação
#Programação I

# Guilherme Boia
# Determinar se um ano é ou não bissexto.

ano = int(input())

if ano % 400 == 0 or (ano % 4 == 0 and ano % 100 != 0):
    print(f"{ano} é bissexto")

else:
    print(f"{ano} não é bissexto")
