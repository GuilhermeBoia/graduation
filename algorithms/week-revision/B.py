n = int(input())
array = list(map(int, input().split()))

sum1 = array[0]
sum3 = array[-1]

first = 0
last = n - 1
maximo = 0
while first < last:
    if sum1 == sum3:
        maximo = sum1
        last -= 1
        sum3 += array[last]

    elif sum1 > sum3:
        last -= 1
        sum3 += array[last]
    else:
        first += 1
        sum1 += array[first]

print(maximo)