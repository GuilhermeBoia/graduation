#Cálculo do IPTU

area = float(input("Área construída? "))
aliquota = float(input("Alíquota? "))
iptu = (area * aliquota) + 35

iptu_1 = iptu * 0.75

iptu_6 = iptu * 0.95
parcela_6 = iptu_6 / 6

parcela_10 = iptu / 10

#Prints
print(f"IPTU: R$ {iptu:.2f}")

print(f"\nPagamento:")

print(f"1. Quota única. R$ {iptu_1:.2f}")

print(f"2. Em 6 parcelas. Total: R$ {iptu_6:.2f}")
print(f"   6 x R$ {parcela_6:.2f}")

print(f"3. Em 10 parcelas. Total: R$ {iptu:.2f}")
print(f"   10 x R$ {parcela_10:.2f}")
