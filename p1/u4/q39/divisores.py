# Guilherme Dantas Boia de Albuquerque
# Listar os divisores prórpios de um número

numero = int(input())

for divisor in range(1, numero):
    if numero % divisor == 0:
        print(divisor)
