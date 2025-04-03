vitorias = 0

for i in range(6):
    game = input()
    if game == "V":
        vitorias += 1
        continue

if vitorias == 0:
    print(-1)
elif vitorias <= 2:
    print(3)
elif vitorias <= 4:
    print(2)
else:
    print(1)

    