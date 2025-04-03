# Divisão de dois números inteira

n1 = int(input())
n2 = int(input())

div = n1 // n2
resto = n1 % n2

print(f"- o quociente é {div}")
print(f"- o resto é {resto}")
print(f"- logo, {n1} = {n2} x {div} + {resto}")
