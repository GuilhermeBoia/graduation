# Categoria de um nadador com base na idade

nome = input()
idade = int(input())

if idade < 5:
    print(f"{nome}, {idade} anos, Não pode competir.")

elif idade < 8:
    print(f"{nome}, {idade} anos, Infantil A.")

elif idade < 11:
    print(f"{nome}, {idade} anos, Infantil B.")

elif idade < 14:
    print(f"{nome}, {idade} anos, Juvenil A.")

elif idade < 18:
    print(f"{nome}, {idade} anos, Juvenil B.")

else:
    print(f"{nome}, {idade} anos, Senior.")
