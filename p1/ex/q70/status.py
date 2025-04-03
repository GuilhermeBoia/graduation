

mtp = int(input())

notas = []
soma = 0
for i in range(mtp):
    nota = float(input())
    soma += nota
    notas.append(notas)

if mtp <= 2:
    media = soma / len(notas)
else:
    media = (soma / len(notas)) - 0.5

print(f"{media:.1f}")

if mtp == 1:
    print("Aluno ainda não aprovado na unidade")
else:
    if media < 6:
        print("Aluno ainda não aprovado na unidade")
    else:
        print("Aluno aprovado na unidade")
