def encontrar_posicoes(n, x, array):
    array_indices = [(valor, i + 1) for i, valor in enumerate(array)]
    
    array_indices.sort()
    
    left = 0
    right = n - 1
    
    while left < right:
        soma = array_indices[left][0] + array_indices[right][0]
        
        if soma == x:
            return array_indices[left][1], array_indices[right][1]
        elif soma < x:
            left += 1
        else:
            right -= 1
    
    return "IMPOSSIBLE"

n, x = map(int, input().split())
array = list(map(int, input().split()))

resultado = encontrar_posicoes(n, x, array)
if resultado == "IMPOSSIBLE":
    print(resultado)
else:
    print(resultado[0], resultado[1])