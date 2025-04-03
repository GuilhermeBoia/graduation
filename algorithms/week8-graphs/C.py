class UnionFind:
    def __init__(self, n):
        self.parent = list(range(n))
        self.rank = [1] * n
        self.size = [1] * n
        self.contagem = n
        self.largest = 1

    def find(self, a):
        if self.parent[a] != a:
            self.parent[a] = self.find(self.parent[a])
        return self.parent[a]

    def union(self, a, b):
        rootA = self.find(a)
        rootB = self.find(b)

        if rootA != rootB:
            if self.rank[rootA] > self.rank[rootB]:
                self.parent[rootB] = rootA
                self.size[rootA] += self.size[rootB]
                self.largest = max(self.largest, self.size[rootA])
            elif self.rank[rootA] < self.rank[rootB]:
                self.parent[rootA] = rootB
                self.size[rootB] += self.size[rootA]
                self.largest = max(self.largest, self.size[rootB])
            else:
                self.parent[rootB] = rootA
                self.size[rootA] += self.size[rootB]
                self.rank[rootA] += 1
                self.largest = max(self.largest, self.size[rootA])

            self.contagem -= 1

def solve(n, edges):
    uf = UnionFind(n)
    results = []

    for a, b in edges:
        uf.union(a - 1, b - 1)
        results.append((uf.contagem, uf.largest))

    return results

# Example usage: 
edges = []
n, m = map(int, input().split())
for _ in range(m):
    a, b = map(int, input().split())
    edges.append((a, b))

results = solve(n, edges)
for contagem, tamanho in results:
    print(contagem, tamanho)