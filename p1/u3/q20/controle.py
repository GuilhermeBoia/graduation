# Controle de Qualidade

peso_antes = float(input())
peso_depois = float(input())

dif = peso_antes - peso_depois
agua = (dif / peso_antes) * 100

print(f'{agua:.1f}% do peso do produto é de água congelada.')

if agua < 5:
    print('Produto qualis A.')

elif agua < 10:
    print('Produto em conformidade.')

else:
    print('Produto não conforme.')
