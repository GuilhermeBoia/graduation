# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Zera Acima ou Abaixo

def zera_acima_ou_abaixo(m):
    soma_acima = 0
    soma_abaixo = 0
    for i in range(len(m)):
        for j in range(len(m[0])):
            if i < j:
                valor = m[i][j]
                soma_acima += valor
            if i > j:
                valor = m[i][j]
                soma_abaixo += valor

    for k in range(len(m)):
        for l in range(len(m[0])):
            if soma_abaixo == soma_acima:
                if k != l:
                    m[k][l] = 0
            if soma_abaixo > soma_acima:
                if k > l:
                    m[k][l] = 0
            
            if soma_acima > soma_abaixo:
                if k < l:
                    m[k][l] = 0
