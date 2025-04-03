n = int(input())

agenda = []
for _ in range(n):
    msg = input()
    agenda.append(msg)

dic = {}
for i in range(n - 1, -1, -1):
    if agenda[i] not in dic:
        print(agenda[i])
        dic[agenda[i]] = 1