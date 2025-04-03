
line = input()
nums = [int(e) for e in line.split()]

n, a, b, c = nums[0], nums[1], nums[2], nums[3]

if n % 4 == 0:
    gasto = 0
elif 4 - (n % 4) == 1:
    gasto = min(a, b+c, c*3)
elif 4 - (n % 4) == 2:
    gasto = min(a*2, b, c*2)
elif 4 - (n % 4) == 3:
    gasto = min(a*3, a+b, c)

print(gasto)