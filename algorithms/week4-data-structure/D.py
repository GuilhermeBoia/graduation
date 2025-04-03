n, k = map(int, input().split())
powers = list(map(int, input().split()))

if k >= n:
    print(max(powers))
    exit()

winner = -1

k_movel = k
while k_movel > 0:
    if powers[0] > powers[1]:
        final = powers.pop(1)
        k_movel -= 1
    else:
        final = powers.pop(0)
        k_movel = k - 1
    powers.append(final)

print(powers[0])