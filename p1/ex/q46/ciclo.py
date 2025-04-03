# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Ciclo trigonométrico


angulo = int(input())

if angulo <= 360:
    posicao = angulo / 90
else:
    if angulo % 360 == 0:
        posicao = 4
    else:
        posicao = (angulo % 360) / 90

if posicao == 0:
    print("Sobre eixos")

elif posicao < 1:
    print("Quadrante 1")

elif posicao == 1:
    print("Sobre eixos")

elif posicao < 2:
    print("Quadrante 2")

elif posicao == 2:
    print("Sobre eixos")

elif posicao < 3:
    print("Quadrante 3")

elif posicao == 3:
    print("Sobre eixos")
    
elif posicao < 4:
    print("Quadrante 4")

else:
    print("Sobre eixos")
