n = int(input())

dic = {}

for i in range(n):
    nomes = input().split()
    old, new = nomes[0], nomes[1]
    
    if new not in dic.values():
        if old in dic.values():
            for i in dic.items():
                if i[1] == old:
                    dic[i[0]] = new
            
        else:
            dic[old] = new

print(len(dic.keys()))
for i in dic.keys():
    print(i, dic[i])