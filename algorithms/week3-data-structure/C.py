n = int(input())

unicos = set()
repete = set()
for i in range(n):
    entrada = input().split()
    quant = int(entrada[0])
    nome = entrada[1:]

    alias = ""
    for l in range(quant):
        alias += nome[l][0]
    
    if alias in unicos:
        repete.add(alias)
    else:
        unicos.add(alias)
    
print(len(unicos) - len(repete))