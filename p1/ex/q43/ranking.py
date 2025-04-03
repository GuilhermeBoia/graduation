# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Imprime Ranking


n = int(input())

posicao = []
pontuacao = []
times = []
for i in range(n):
    time = input() 
    pontos = int(input())
    pontuacao.append(pontos)
    times.append(time)
    posicao.append(i+1)

for j in range(len(times) - 1):
    if pontuacao[j] == pontuacao[j+1]:
        posicao [j+1] = posicao [j]

for k in range(len(times)):
    print(f"{posicao[k]}. {times[k]} ({pontuacao[k]})")
