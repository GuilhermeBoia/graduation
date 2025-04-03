#Área de uma Esfera
import math

raio = float(input())
area = 4 * math.pi * (raio ** 2)

#Volume de uma Esfera
volume = 4 * math.pi * (raio ** 3) / 3

print(f"{area:.2f}")
print(f"{volume:.2f}")
