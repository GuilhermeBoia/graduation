n = int(input())
line = input()

zeros = line.count("0")
ones = line.count("1")

remove = 2 * min(ones, zeros)

new_len = n - remove

print(new_len)