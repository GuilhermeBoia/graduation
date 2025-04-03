# Calcular o Orçamento de uma Contrução

preco = float(input("Digite o preço da unidade do tijolo (Em reais): "))
altura_tijolo = float(input("Digite a altura do tijolo (Em metros): "))
comp_tijolo = float(input("Digite o comprimento do tijolo (Em metros): "))
altura_parede = float(input("Digite a altura das paredes (Em metros): "))
comp_parede = float(input("Digite o comprimento das paredes (Em metros): "))

tijolos_por_comp = comp_parede / comp_tijolo
tijolos_por_altura = altura_parede / altura_tijolo
tijolos_total = tijolos_por_comp * tijolos_por_altura

orcamento = tijolos_total * preco

print(f"O número total de tijolos é {tijolos_total:.1f} e o orçamento final é de R$ {orcamento:.1f}")
