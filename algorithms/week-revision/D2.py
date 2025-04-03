def can_divide(nums, n, k, max_sum):
    current_sum = 0
    subarrays = 1
    for num in nums:
        if current_sum + num > max_sum:
            subarrays += 1
            current_sum = num
            if subarrays > k:
                return False
        else:
            current_sum += num
    return True

def find_minimum_max_sum(nums, n, k):
    low = max(nums)
    high = sum(nums)
    
    while low < high:
        mid = (low + high) // 2
        if can_divide(nums, n, k, mid):
            high = mid
        else:
            low = mid + 1
    
    return low

n, k = map(int, input().split())
array = list(map(int, input().split()))
print(find_minimum_max_sum(array, n, k))