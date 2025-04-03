# Guilherme Dantas Boia de Albuquerque
# Programa de média das notas

print("== Estágio 1 ==")
peso1 = float(input("Peso? "))
nota1 = float(input("Nota? "))

print("== Estágio 2 ==")
peso2 = float(input("Peso? "))
nota2 = float(input("Nota? "))

print("== Estágio 3 ==")
peso3 = float(input("Peso? "))
nota3 = float(input("Nota? "))

parcial = (nota1 * peso1) + (nota2 * peso2) + (nota3 * peso3)
final5 = abs(((parcial * 0.6) - 5) / 0.4)
final7 = abs(((parcial * 0.6) - 7) / 0.4)

print("== Resultados ==")
print(f"Média parcial: {parcial:.1f}")
print(f"Nota na final, pra média 5.0 = {final5:.1f}")
print(f"Nota na final, pra média 7.0 = {final7:.1f}")


