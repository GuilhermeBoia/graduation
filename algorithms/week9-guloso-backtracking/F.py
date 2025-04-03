from itertools import permutations

string = list(input())

p = permutations(string)
p_unicas = set(p)
sorted = sorted(p_unicas)
print(len(sorted))

for p in sorted:
    print(''.join(p))