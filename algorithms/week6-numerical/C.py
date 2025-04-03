n = int(input())

a = int(input("? 1 2\n"))
b = int(input("? 1 3\n"))
c = int(input("? 2 3\n"))

num1 = int((a + b - c) / 2)
num2 = int(a - num1)
num3 = int(b - num1)

array = ['!', num1, num2, num3]

for i in range(4, n + 1):
    next = int(input(f"? 1 {i}\n"))
    array.append(int(next - num1))

print(*array)
