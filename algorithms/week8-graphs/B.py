def get_group(groups, u):
    if groups[u] != u:
        groups[u] = get_group(groups, groups[u])
    return groups[u]

def main():
    while True:
        m, n = map(int, input().split())
        if m == 0 and n == 0:
            break

        streets = []
        original_cost = 0

        for _ in range(n):
            u, v, length = map(int, input().split())
            streets.append((u, v, length))
            original_cost += length

        streets.sort(key=lambda x: x[2])

        groups = list(range(m))
        cost = 0
        street_count = 0

        for u, v, length in streets:
            group_u = get_group(groups, u)
            group_v = get_group(groups, v)
            if group_u != group_v:
                cost += length
                groups[group_v] = group_u
                street_count += 1
                if street_count == m - 1:
                    break

        print(original_cost - cost)

if __name__ == "__main__":
    main()