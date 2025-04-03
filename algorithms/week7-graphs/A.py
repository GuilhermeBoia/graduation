from collections import deque

mx = [1, -1, 0, 0]
my = [0, 0, 1, -1]

def can(x, y):
    if x < 0 or y < 0 or x > n - 1 or y > m - 1:
        return False
    return True

n, m = map(int, input().split())

mapa = ['' for i in range(n)]
visited = [[False] * m for i in range(n)]

# Preenche matriz do mapa
for i in range(n):
    mapa[i] = input()

rooms = 0

def preenche_quarto(i, j):
    fila = deque([])
    fila.append((i, j))
    visited[i][j] = True

    while len(fila) > 0:
        atual = fila.popleft()
        x, y = atual[0], atual[1]
        for k in range(4):
            newx = x + mx[k]
            newy = y + my[k]
            if can(newx, newy) and not visited[newx][newy] and mapa[newx][newy] == '.':
                fila.append((newx, newy))
                visited[newx][newy] = True

for i in range(n):
    for j in range(m):
        if not visited[i][j] and mapa[i][j] == '.':
            rooms += 1
            preenche_quarto(i, j)

print(rooms)