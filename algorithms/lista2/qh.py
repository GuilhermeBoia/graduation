n = int(input())
linha = input()
nums = [int(e) for e in linha.split()]
nums.sort()

final = 0
temp = 0
i = 0
j = 0
while j <= n-1:
    if nums[j] - nums[i] <= 5:
        j += 1
        temp = j-i

    else:
        i += 1
        temp = 0

    if final <= temp:
        final = temp

print(final)