n = int(input())

ans = []
for i in range(n):
    line = input()
    if "B" not in line:
        ans.append(len(line))
        continue
    
    linha = []
    for e in line:
        linha.append(e)

    j = 1
    i = 0
    count = 0
    while j < len(line) - count:
        string = linha[i] + linha[j]

        if string == "AB" or string == "BB":
            linha.pop(j)
            linha.pop(i)
            i = 0
            j = 1
            count += 2
        else:
            i += 1
            j += 1
    
    resp = len(line) - count
    ans.append(resp)

for e in ans:
    print(e)