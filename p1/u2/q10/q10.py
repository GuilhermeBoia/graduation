#Cálculo de caixas de cerâmica: paredes e chão

#Coleta de dados
import math

capacidade = float(input("Capacidade de revestimento? "))
print("\n== Dados do vão a revestir ==")
comprimento = float(input("Comprimento? "))
largura = float(input("Largura? "))
altura = float(input("Altura? "))

#Cálculos
area_paredes = (2 * comprimento * altura) + (2 * largura * altura) 
area_chao = comprimento * largura
area_total = area_chao + area_paredes
caixas = math.ceil(area_total / capacidade)

#Prints
print("\n== Resultados ==")
print(f"Área total a revestir: {area_total:.1f} m2")
print(f"Número de caixas: {caixas}")

