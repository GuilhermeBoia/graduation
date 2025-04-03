# Tempo de um Triatlo

recorde = input().split()
natacao = input().split()
bike = input().split()
corrida = input().split()

hr_recorde = int(recorde[0])
min_recorde = int(recorde[1])
seg_recorde = int(recorde[2])

hr_novo =  int(natacao[0]) + int(bike[0]) + int(corrida[0])
min_novo = int(natacao[1]) + int(bike[1]) + int(corrida[1])
seg_novo = int(natacao[2]) + int(bike[2]) + int(corrida[2])

recorde_em_seg = (hr_recorde * 3600) + (min_recorde * 60) + seg_recorde
novo_em_seg = (hr_novo * 3600) + (min_novo * 60) + seg_novo

if recorde_em_seg < novo_em_seg:
    print("recorde mantido")

elif recorde_em_seg == novo_em_seg:
    print("recorde empatado")

else:
    if seg_novo < 60 and min_novo < 60:
        print(f"recorde quebrado ({hr_novo}h{min_novo}min{seg_novo}seg)")

    else:
        if seg_novo > 60:
            seg_novo2 = seg_novo % 60
            min_novo2 = min_novo + seg_novo // 60
            if min_novo2 > 60:
                min_novo3 = min_novo2 % 60
                hr_novo2 = hr_novo + min_novo2 // 60
                print(f"recorde quebrado ({hr_novo2}h{min_novo3}min{seg_novo2}seg)")

        if seg_novo < 60 and min_novo > 60:
            min_novo2 = min_novo % 60
            hr_novo2 = hr_novo + min_novo // 60
            print(f"recorde quebrado ({hr_novo2}h{min_novo2}min{seg_novo}seg)")
