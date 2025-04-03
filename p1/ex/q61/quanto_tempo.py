# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Diferença de tempo


def quanto_tempo(horario1, horario2):

    hr1 = [int(hr) for hr in horario1.split(":")]
    hr2 = [int(hr) for hr in horario2.split(":")]

    if hr2[1] >= hr1[1]:
        dif_hr = hr2[0] - hr1[0]
        dif_min = hr2[1] - hr1[1]
    else:
        hr2[0] -= 1
        hr2[1] += 60
        dif_hr = hr2[0] - hr1[0]
        dif_min = hr2[1] - hr1[1]
        

    return (f"{dif_hr} hora(s) e {dif_min} minuto(s)")
