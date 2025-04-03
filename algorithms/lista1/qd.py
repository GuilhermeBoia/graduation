n = int(input())
line = input().split()
nums = [int(e) for e in line]

nums.sort()

count = 0
i = 0
j = 0
q = 1

if n == 1:
    count = 1

if n > 1:
    while True:
        if j > n - 1:
            break
        if i > n - 1:
            break

        if (q) <= nums[i]:
            count += 1
            i += 1
            j += 1
            q += 1
            continue
        else:
            if (q) <= nums[j]:
                count += 1
                j += 1
                i += 2
                q += 1
            else:
                j += 1


print(count)