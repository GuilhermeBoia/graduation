s, n = map(int, input().split())

sizes = []
values = []
for _ in range(n):
    size, value = map(int, input().split())
    sizes.append(size)
    values.append(value)

max_value = [0] * (s + 1)

for k in range(n):
    for x in range(s, sizes[k] - 1, -1):
        max_value[x] = max(max_value[x], max_value[x - sizes[k]] + values[k])

print(max_value[s])