n = int(input())
balls = list(map(int, input().split()))

final = []
for i in range(n):
    num = balls[i]
    
    while len(final) > 0 and final[-1] == num:
        num += 1
        final.pop()
    
    final.append(num)

print(len(final))