orcamento = float(input("Orçamento? R$ "))
adultos = int(input("Número de adultos? "))
criancas = int(input("Número de crianças? "))
pizza = float(input("Preço da pizza? R$ "))
refri = float(input("Preço do refrigerante? R$ "))
estacionamento = float(input("Preço do estacionamento? R$ "))
ingresso = float(input("Preço do ingresso do cinema? R$ "))

taxa_cinema = (adultos + criancas) * 2
custo_cinema = (criancas * ingresso/2) + (adultos * ingresso)

alimentacao = refri + pizza
cinema = taxa_cinema + custo_cinema
total = alimentacao + cinema + estacionamento
medio = total / (criancas + adultos)
saldo = orcamento - total

print("========== Despesas do cinema ==========")

print(f"Alimentacao: R$ {alimentacao:.2f}")
print(f"Cinema: R$ {cinema:.2f}")
print(f"Custo médio por pessoa: R$ {medio:.2f}")
print(f"Total: {total:.2f}")
print(f"Saldo após passeio: {saldo:.2f}")

