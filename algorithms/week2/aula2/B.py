def max_common_letters(n, s):
    # Contar a frequência de cada letra na string inteira
    total_count = {}
    for char in s:
        if char in total_count:
            total_count[char] += 1
        else:
            total_count[char] = 1
    
    # Conjuntos para armazenar letras distintas na primeira e segunda parte
    left_set = set()
    right_count = total_count.copy()
    
    max_common = 0
    
    # Iterar sobre todas as posições possíveis de corte
    for i in range(n - 1):
        left_set.add(s[i])
        right_count[s[i]] -= 1
        if right_count[s[i]] == 0:
            del right_count[s[i]]
        
        # Calcular o número de letras distintas em ambas as partes
        common_count = len(left_set.intersection(right_count.keys()))
        max_common = max(max_common, common_count)
    
    return max_common

# Leitura dos inputs
n = int(input())
s = input().strip()

# Chamada da função e impressão do resultado
resultado = max_common_letters(n, s)
print(resultado)