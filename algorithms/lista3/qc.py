n = int(input())

inputs = {}

for i in range(n):
    nome = input()

    if nome not in inputs:
        print("OK")
        inputs[nome] = 1
        
    else:
        print(f"{nome}{inputs[nome]}")
        inputs[nome] += 1
