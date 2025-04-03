# Guilherme Dantas Boia de Albuquerque
# Cálculo de uma ligação telefônica

tempo = int(input())

blocos_de_5 = tempo // 5
resto = tempo % 5

if tempo <= 3:
    preco = 1 + (tempo * 0.50)

else:
    preco = 1 + (blocos_de_5 * 3) + (resto * 0.70)

print(f"R$ {preco:.2f}")
