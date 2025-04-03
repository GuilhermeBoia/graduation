t = int(input())

for i in range(t):
    a, b, c = map(int, input().split())
    x = pow(10, (c-1))
    y = pow(10, (c-1))

    while(x < pow(10, a-1)):
        x *= 2
    
    while (y < pow(10, b-1)):
        y *= 3
    
    print(x, y)