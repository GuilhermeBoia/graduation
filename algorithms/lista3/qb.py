n = int(input())

horarios = []
for i in range(n):
    hora = [int(e) for e in input().split()]
    comeca, termina = hora[0], hora[1]
    programa = (comeca, termina)
    horarios.append(programa)

horarios.sort()

pode = True
for i in range(n-2):
    if horarios[i][1] >= horarios[i+2][0]:
        pode = False
#        if horarios[i+1][1] < horarios[i+2][0]:
#            pode = True
#            if horarios[i+2][1] >= horarios[i+3][0]:
#                pode = False
#               break

if pode:
    print("YES")
else:
    print("NO")