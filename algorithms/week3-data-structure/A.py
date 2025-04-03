n = int(input())

for i in range(n):
    string = input()
    pilha = []
    for j in range(len(string)):
        if string[j] == "B":
            if len(pilha) > 0:
                pilha.pop()
            else:
                pilha.append("B")
        else:
            pilha.append("A")

    print(len(pilha))