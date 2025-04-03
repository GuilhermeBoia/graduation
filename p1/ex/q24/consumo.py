# Guilherme Dantas Boia de Albuquerque
# Calculando o consumo de energia em Joules

potencia = float(input())
tempo = float(input())

khw = (potencia / 1000) * (tempo / 60)

print(f"{khw:.1f} kWh")
