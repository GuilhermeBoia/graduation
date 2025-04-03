#Imprimindo cartões de Embarque

#Informações da passagem
identificador = str(input())
horario = str(input())
assento = str(input())
portao = str(input())
sem_imposto = float(input())
total = float(input())

#Cálculo do % de imposto
percentual_imposto = (total - sem_imposto) * 100 / total

#Prints
print('### Cartão de Embarque ###')
print(f'Identificador do voo: {identificador}')
print(f'Horário: {horario}')
print(f'Assento: {assento}')
print(f'Portão: {portao}')
print(f'Total de Imposto: {percentual_imposto:.1f}%')
