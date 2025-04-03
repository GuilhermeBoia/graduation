# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Elementos Químicos

elementos = {"H": 1, "S": 32, "O": 16, "C": 12, "Ca": 40, "Na": 23, "P": 31}

massas = []
while True:
    molecula = input()
    if molecula == "fim": break

    lista = [n for n in molecula.split()]
    soma = 0
    for i in range(len(lista)):
        if i < len(lista) - 1:
            if not lista[i].isdigit():
                if lista[i+1].isdigit():
                    massa = elementos[lista[i]] * int(lista[i+1])
                else:
                    massa = elementos[lista[i]]
            
                soma += massa

        if i == len(lista) - 1:
            if not lista[i].isdigit():
                massa = elementos[lista[i]]
                soma += massa

    massas.append(soma)

for e in massas:
    print(e)
