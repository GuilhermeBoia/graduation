def sortSecond(val):
    return val[1] 

n = int(input())

horarios = []
for _ in range(n):
    init, end = map(int, input().split())
    horarios.append((init, end))
    
horarios.sort(key=sortSecond)
    
filmes = 1
fim = horarios[0][1]
for i in range(1, n):
    if fim <= horarios[i][0]:
        filmes += 1
        fim = horarios[i][1]
        
print(filmes)