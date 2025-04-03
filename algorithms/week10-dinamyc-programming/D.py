n = int(input())
grid = [input() for _ in range(n)]

m = 10**9 + 7    
dp = [[0] * n for _ in range(n)]
    
if grid[0][0] == '.':
    dp[0][0] = 1
    
for i in range(n):
    for j in range(n):
        if grid[i][j] == '*':
            dp[i][j] = 0
        else:
            if i > 0:
                dp[i][j] += dp[i-1][j]
            if j > 0:
                dp[i][j] += dp[i][j-1]
            dp[i][j] %= m

print(dp[n-1][n-1])
