# Determinar se é ou não triângulo.

a = int(input())
b = int(input())
c = int(input())

b_c = abs(b - c)
a_c = abs(a - c)
a_b = abs(a - b)

perimetro = a + b + c 

# Um lado precisa ser menor que a soma dos dois outros lados.
if a > (b + c) or b > (a + c) or c > (a + b):
    print("triangulo invalido.")

# O lado precisa ser maior que o valor absoluto da diferença entre os outros dois lados.
elif a < b_c or b < a_c or c < a_b:
    print("triangulo invalido.")

else:
    print(f"triangulo valido. {perimetro}")

