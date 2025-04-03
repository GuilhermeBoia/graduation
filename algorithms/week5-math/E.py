t = int(input())
    
for _ in range(t):
    num = int(input())
    count = 0
    for i in range(1, num + 1):
        if num % i != 0:
            break
        count += 1
    
    print(count)