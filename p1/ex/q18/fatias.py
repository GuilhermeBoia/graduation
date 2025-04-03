# Número de fatias por convidado

convidados = int(input())

op1_inteira = 32 // convidados
op1_resto  = 32 % convidados
op2 = 32 / convidados

print(f'Opção 1: {op1_inteira} fatias cada, {op1_resto} de resto')
print(f'Opção 2: {op2:.2f} fatias cada')
