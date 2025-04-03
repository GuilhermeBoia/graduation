def inverte3a3(string):
    nova_string = ""

    for i in range(len(string) -1, -1, -3):
        nova_string += string[i-2]
        nova_string += string[i-1]
        nova_string += string[i]

    return nova_string
