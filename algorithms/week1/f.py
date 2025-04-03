def conta_palindromo(A, B):
    count = 0
    for num in range(A, B + 1):
        if str(num) == str(num)[::-1]:
            count += 1
    return count

# Exemplo de uso:
A, B = map(int, input().split())
print(conta_palindromo(A, B))