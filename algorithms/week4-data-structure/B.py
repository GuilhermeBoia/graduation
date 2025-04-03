t = int(input())

for _ in range(t):
    n, q = map(int, input().split())
    forces = list(map(int, input().split()))
    queue = [(forces[i], i + 1) for i in range(n)]
    wins = [0] * n
    max_force = max(forces)
    max_index = forces.index(max_force) + 1
    rounds = 0
    
    while queue[0][0] != max_force:
        rounds += 1
        a, idx_a = queue.pop(0)
        b, idx_b = queue.pop(0)
        
        if a > b:
            wins[idx_a - 1] += 1
            queue.insert(0, (a, idx_a))
            queue.append((b, idx_b))
        else:
            wins[idx_b - 1] += 1
            queue.insert(0, (b, idx_b))
            queue.append((a, idx_a))
    
    results = []
    for _ in range(q):
        i, k = map(int, input().split())
        
        if k <= rounds:
            results.append(wins[i - 1])
        else:
            additional_wins = max(0, k - rounds) if i == max_index else 0
            results.append(wins[i - 1] + additional_wins)
    
    for result in results:
        print(result)