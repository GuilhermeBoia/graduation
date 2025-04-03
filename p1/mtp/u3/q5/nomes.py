#Monta nome

nome1 = input()
nome2 = input()
nome3 = input()

tam1 = len(nome1)
tam2 = len(nome2)
tam3 = len(nome3)

if tam1 % 10 == 0 or tam2 % 10 == 0 or tam3 % 10 == 0:
    print("Divisivelpordez")

elif tam1 % 2 == 0 and tam2 % 3 != 0 and tam3 % 5 != 0:
    print(nome1)

elif tam1 % 2 != 0 and tam2 % 3 == 0 and tam3 % 5 != 0:
    print(nome2)

elif tam1 % 2 != 0 and tam2 % 3 != 0 and tam3 % 5 == 0:
    print(nome3)

elif tam1 % 2 == 0 and tam2 % 3 == 0 and tam3 % 5 != 0:
    print(f"{nome1}{nome2}")

elif tam1 % 2 != 0 and tam2 % 3 == 0 and tam3 % 5 == 0:
    print(f"{nome2}{nome3}")

elif tam1 % 2 == 0 and tam2 % 3 == 0 and tam3 % 5 == 0:
    print(f"{nome1}{nome2}{nome3}")

else:
    print("Labprog1")
