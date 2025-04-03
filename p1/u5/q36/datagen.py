import sys
import random

if len(sys.argv) < 2:
    print("uso: python3 datagen.py <seed>")
    sys.exit(1)

random.seed(int(sys.argv[1]))
arq = open("dados.dat", mode="w")
num = 100_000_000
for i in range(num):
    arq.write(f"{random.randint(0, 1000000000)}\n")
