n = int(input())

e1 = [int(e) for e in input().split()]
e2 = [int(e) for e in input().split()]
e3 = [int(e) for e in input().split()]

primeiro, segundo, terceiro = sum(e1), sum(e2), sum(e3)

print(abs(segundo - primeiro))
print(abs(terceiro - segundo))
