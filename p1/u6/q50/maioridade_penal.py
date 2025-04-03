# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Maioridade Penal

def maioridade_penal(str1, str2):
    maiores = str1.split()
    idades = [int(n) for n in str2.split()]
    
    for i in range(len(maiores) - 1, -1, -1):
        if idades[i] < 18:
            maiores.pop(i)

    saida = ""
    for j in range(len(maiores)):
        if j < len(maiores) - 1:
            saida += maiores[j]
            saida += " "
        else:
            saida += maiores[j]

    return saida
