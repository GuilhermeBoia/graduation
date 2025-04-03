salario = float(input())

empregador = 0.12 * salario

if salario <= 1318.07:
    empregado = 0.08 * salario
elif salario <= 2195.12:
    empregado = 0.09 * salario
else:
    empregado = 0.11 * salario

print(f"O valor da contribuição do INSS a ser pago pelo empregador é de R$ {empregador:.2f}")
print(f"O valor da contribuição do INSS a ser pago pelo empregado é de R$ {empregado:.2f}")
