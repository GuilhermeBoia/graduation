def can_win_with_k(k, array):
    n = len(array)
    array = sorted(array)
    for i in range(k):
        threshold = k - i
        index = -1
        for j in range(len(array)):
            if array[j] <= threshold:
                index = j
                break
        if index == -1:
            return False
        array.pop(index)
        if array:
            array[-1] += threshold
            array = sorted(array)
    return True


def max_k(n, array):
    left, right = 0, n
    while left < right:
        mid = (left + right + 1) // 2
        if can_win_with_k(mid, array[:]):
            left = mid
        else:
            right = mid - 1
    return left

t = int(input())
results = []
for _ in range(t):
    n = int(input())
    array = list(map(int, input().split()))
    results.append(max_k(n, array))

for result in results:
    print(result)