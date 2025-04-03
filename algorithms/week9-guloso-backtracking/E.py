
def solve(n, w, a):
    info = []
    for i in range(n):
        info.append((a[i], i+1))
    info.sort(reverse=True)
    
    ans = []
    sum = 0
    for i in range(n):
        if sum + info[i][0] <= w:
            sum += info[i][0]
            ans.append(info[i][1])

    if sum < (w + 1) // 2:
        print(-1)
    else:
        print(len(ans))
        print(" ".join(map(str, reversed(ans))))

t = int(input())

for _ in range(t):
    n, w = map(int, input().split())
    a = list(map(int, input().split()))
    solve(n, w, a)