def coin_combinations(n, x, coins):
    MOD = 10**9 + 7
    dp = [0] * (x + 1)
    dp[0] = 1

    for i in range(1, x + 1):
        for coin in coins:
            if i - coin >= 0:
                dp[i] = (dp[i] + dp[i - coin]) % MOD

    return dp[x]

# Leitura dos dados de entrada
n, x = map(int, input().split())
coins = list(map(int, input().split()))

# Calcula e imprime o número de combinações
print(coin_combinations(n, x, coins))