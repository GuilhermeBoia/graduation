n = int(input())
linha = input()
nums = [int(e) for e in linha.split()]
nums.sort()

total = sum(nums)
minha = 0
count = 0
for i in range(n-1, -1, -1):
    minha += nums[i]
    count += 1
    if minha > total // 2: break

print(count)