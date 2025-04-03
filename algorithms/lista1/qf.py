
n = int(input())
line = input().split()
sides = [int(e) for e in line]
sides.sort()

#|b - c| < a < b + c
#|a - c| < b < a + c
#|a - b| < c < a + b

if n == 3:
    if sides[2] < sides[1] + sides[0]:
        print("YES")
    else:
        print("NO")


exist = False
if n > 3:
    for i in range(2, n):
        if sides[i] < sides[i-1] + sides[i-2]:
            exist = True
            break

    if exist:
        print("YES")
    else:
        print("NO")