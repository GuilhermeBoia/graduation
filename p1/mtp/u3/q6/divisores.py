# Divisores de um número

n = int(input())

if n % 2 == 0 and n % 3 == 0 and n % 5 == 0:
    print(f"{n} é divisível por 2, 3 e 5.")

elif n % 2 == 0 and n % 3 == 0 and n % 5 != 0:
    print(f"{n} é divisível por 2 e 3 e não por 5.")

elif n % 2 == 0 and n % 3 != 0 and n % 5 == 0:
    print(f"{n} é divisível por 2 e 5 e não por 3.")

elif n % 2 != 0 and n % 3 == 0 and n % 5 == 0:
    print(f"{n} é divisível por 3 e 5 e não por 2.")

elif n % 2 == 0 and n % 3 != 0 and n % 5 != 0:
    print(f"{n} é divisível apenas por 2.")

elif n % 2 != 0 and n % 3 == 0 and n % 5 != 0:
    print(f"{n} é divisível apenas por 3.")

elif n % 2 != 0 and n % 3 != 0 and n % 5 == 0:
    print(f"{n} é divisível apenas por 5.")

else:
    print(f"{n} não é divisível por 2, nem 3 e nem 5.")

