n = int(input())
m = int(input())
# Representação de shift binário, ex: n = 3 da 3 shifts. 0001 - 0010 - 0100 = 8.
power_of_two = 1 << n
result = m % power_of_two

print(result)