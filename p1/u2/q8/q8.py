#Formatação de CPF
cpf1 = int(input())
cpf2 = int(input())
cpf3 = int(input())

#Últimos 2 digitos
last1 = cpf1 % 100
last2 = cpf2 % 100
last3 = cpf3 % 100


#Primeiros 9 digitos do cpf
rest1 = cpf1 // 100
rest2 = cpf2 // 100
rest3 = cpf3 // 100

#Soma dos últimos 2 números
soma1 = (last1 // 10) + (last1 % 10)
soma2 = (last2 // 10) + (last2 % 10)
soma3 = (last3 // 10) + (last3 % 10)

#Prints
print(f"{rest1}-{last1}")
print(soma1)
print(f"{rest2}-{last2}")
print(soma2)
print(f"{rest3}-{last3}")
print(soma3)
