quartos = [0] * 10

n = int(input())
ordem = input()

for i in range(n):
    if ordem[i] == 'L':
        for j in range(10):
            if quartos[j] == 0:
                quartos[j] = 1
                break
    elif ordem[i] == 'R':
        for j in range(9, -1, -1):
            if quartos[j] == 0:
                quartos[j] = 1
                break
    else:
        quartos[int(ordem[i])] = 0

for e in quartos:
    print(e, end="")