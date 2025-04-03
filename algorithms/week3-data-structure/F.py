n, x = map(int, input().split())
array = list(map(int, input().split()))

if x == 1:
    print("IMPOSSIBLE")
    exit()
    
module = {}
for i in range(n):
    comp = x - array[i]
    if comp in module:
        print(module[comp] + 1, i + 1)
        break
    
    module[array[i]] = i
else:
    print("IMPOSSIBLE")