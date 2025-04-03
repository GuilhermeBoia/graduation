def mmc(a, b):
    maior = max(a, b)
    menor = min(a, b)
    for i in range(maior, a*b+1, maior):
        if i % menor == 0:
            return i

b = int(input())

nums = []
for a in range(1, 10**8):
    if mmc(a, b) % a == 0:
        num = mmc(a, b) // a
        if num not in nums:
            nums.append(num)
        if mmc(a, b) == a:
            break

print(len(nums))