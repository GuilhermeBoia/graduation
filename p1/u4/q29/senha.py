senha = input()

nova_senha = []
nova_senha.append(senha[0])

count = 0
for i in range(1, len(senha)):
    if "a" == senha[i] or "A" == senha[i]:
        nova_senha.append("4")
        count += 1
    elif "e" == senha[i] or "E" == senha[i]:
        nova_senha.append("3")
        count += 1
    elif "i" == senha[i] or "I" == senha[i]:
        nova_senha.append("1")
        count += 1
    elif "o" == senha[i] or "O" == senha[i]:
        nova_senha.append("0")
        count += 1
    else:
        nova_senha.append(senha[i])

for j in range(len(nova_senha)):
    print(nova_senha[j], end="")

print(f" ({count} troca(s))")
