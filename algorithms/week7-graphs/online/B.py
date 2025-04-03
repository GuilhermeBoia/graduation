def solve(sequence):
    count = {x: sequence.count(x) for x in sequence}
    start = None
    for num in sequence:
        if count.get(num * 3, 0) == 0 and (num % 2 != 0 or count.get(num // 2, 0) == 0):
            start = num
            break

    result = [start]
    current = start
    while len(result) < len(sequence):
        if current % 3 == 0 and count.get(current // 3, 0) > 0:
            current //= 3
        else:
            current *= 2
        result.append(current)
        count[current] -= 1

    return result

n = int(input())
sequence = list(map(int, input().split()))
nova = solve(sequence)
print(*nova)