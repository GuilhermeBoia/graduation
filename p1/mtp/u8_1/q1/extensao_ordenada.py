# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Extensão ordenada

def extensao_ordenada(nums, novos):
    for e in novos:
        nums.append(e)
        for i in range(len(nums)):
            for j in range(len(nums) -1 -i):
                if nums[j] > nums[j+1]:
                    nums[j], nums[j+1] = nums[j+1], nums[j]
