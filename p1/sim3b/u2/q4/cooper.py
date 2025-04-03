import math

raio = float(input())
voltas = float(input())

comprimento = 2 * math.pi * raio
distancia = comprimento * voltas

print(f"A pessoa percorreu {distancia:.3f} metros.")


