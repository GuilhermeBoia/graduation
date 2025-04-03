
n = int(input())
line = input().split()
email = [int(e) for e in line]

count = 0
last = 1
find = False

for i in email:
    if find:
        if i == 1:
            if last == 1:
                count += 1
            else:
                count += 2
        last = i

    else:
        if i == 1:
            find = True
            count += 1

print(count)