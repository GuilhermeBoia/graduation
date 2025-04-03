def processo(n, q, array, consultas):
    count_ones = sum(array)
    count_zeros = n - count_ones
    results = []

    for query in consultas:
        t, x = query
        if t == 1:
            if array[x - 1] == 1:
                count_ones -= 1
                count_zeros += 1
            else:
                count_ones += 1
                count_zeros -= 1
            array[x - 1] = 1 - array[x - 1]  # Alterna o valor entre 0 e 1
        elif t == 2:
            if x <= count_ones:
                results.append(1)
            else:
                results.append(0)

    return results

n, q = map(int, input().split())
array = list(map(int, input().split()))
consultas = [tuple(map(int, input().split())) for _ in range(q)]
results = processo(n, q, array, consultas)


for result in results:
    print(result)