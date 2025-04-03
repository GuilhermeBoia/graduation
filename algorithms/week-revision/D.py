def can_divide(nums, k, max_sum):
    subarray_count = 1
    current_sum = 0
    
    for num in nums:
        if current_sum + num > max_sum:
            subarray_count += 1
            current_sum = num
            if subarray_count > k:
                return False
        else:
            current_sum += num
    
    return True

n, k = map(int, input().split())
array = list(map(int, input().split()))

maximo = max(array)
total = sum(array)
low, high = maximo, total

while low < high:
    mid = (low + high) // 2
    if can_divide(array, k, mid):
        high = mid
    else:
        low = mid + 1

print(low)