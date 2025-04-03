n = int(input())
sequencia = list(map(int, input().split()))

max_movel = max_final = sequencia[0]
    
for num in sequencia[1:]:
    max_movel = max(num, max_movel + num)
    if max_movel > max_final:
        max_final = max_movel
    
print(max_final)