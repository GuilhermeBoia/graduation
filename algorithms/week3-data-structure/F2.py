n, x = map(int, input().split())
array = list(map(int, input().split()))

array_with_indices = [(array[i], i + 1) for i in range(n)]
array_with_indices.sort()

left = 0
right = n - 1

while left < right:
    current_sum = array_with_indices[left][0] + array_with_indices[right][0]
    if current_sum == x:
        print(array_with_indices[left][1], array_with_indices[right][1])
        break
    elif current_sum < x:
        left += 1
    else:
        right -= 1
else:
    print("IMPOSSIBLE")