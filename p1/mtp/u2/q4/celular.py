#Perda de tempo no celular

d1 = int(input())
d2 = int(input())
d3 = int(input())
d4 = int(input())
d5 = int(input())

total = d1 + d2 + d3 + d4 + d5
media = (d1 + d2 + d3 + d4 + d5) / 5
estudo = total // 60

print(f"Você perdeu {total} min na semana (média de {media:.1f} min por dia).")
print(f"Daria para aproveitar {estudo} intervalos(s) para estudar.")