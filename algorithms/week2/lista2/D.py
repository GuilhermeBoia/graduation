n = int(input())
prof = list(map(int, input().split()))
aluno = list(map(int, input().split()))

dif = []
for k in range(n):
    d = prof[k] - aluno[k]
    dif.append(d)
    
dif.sort()
    
count = 0
for i in range(n):
    if dif[i] > 0:
         count += n - i - 1
    else:
         left, right = i + 1, n
         while left < right:
            mid = (left + right) // 2
            if dif[i] + dif[mid] > 0:
                 right = mid
            else:
                left = mid + 1
         count += n - left
    
print(count)