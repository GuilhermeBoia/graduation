#Cálculo da Superfície de um Cilindro
import math

print("Cálculo da Superfície de um Cilindro\n---")
diametro = float(input("Medida do diâmetro? "))
altura = float(input("Medida da altura? "))

#Cáculos
raio = diametro / 2
area_base = math.pi * (raio**2)
area_lateral = altura * (2 * math.pi * raio)
area_cilindro = area_lateral + (2 * area_base)

#Prints
print(f"---\nÁrea calculada: {area_cilindro:.2f}")
