linha = input().split()

nums = [int(e) for e in linha]
nums.sort()
a, b, c = nums[0], nums[1], nums[2]

pontoa = b-a + c-a
pontob = b-a + c-b
pontoc = c-b + c-a

print(min(pontoa, pontob, pontoc))