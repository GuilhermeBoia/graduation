# Guilherme Dantas Boia de Albuquerque
# Reprovados por falta

reprovados = 0
while True:
    linha = input()
    if linha == "-": break

    i = 0
    faltas = 0
    while i <= len(linha) - 1:
        if linha[i] == "f":
            faltas += 1
        i += 1

    if faltas > 8:
        reprovados += 1

print(f"{reprovados} aluno(s) reprovado(s) por falta.")
