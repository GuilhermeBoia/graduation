n = int(input())

tem = False
for i in range(0, n+1, 1234567):
    if tem: break
    for j in range(0, n-i+1, 123456):
    
        if (n - i - j) % 1234 == 0:
            tem = True
            break

if tem:
    print("YES")
else:
    print("NO")