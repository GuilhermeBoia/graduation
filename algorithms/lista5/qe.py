def factors(num):  
    divisores = []
    i = 1
    while i * i <= num:
        if num % i == 0:
            if num / i == i:
                divisores.append(i)
            else:
                divisores.append(i)
                divisores.append(int(num/i))
        
        i = i + 1

    return divisores


n = int(input())

nums = [int(e) for e in input().split()]

for i in range(n):
    number = nums[i]
    
    if len(factors(number)) == 3:
        print("YES")
    else:
        print("NO")
