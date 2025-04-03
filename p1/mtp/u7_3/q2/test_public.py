from undertst import ajusta_lista

def test_exemplo():
    l1 = ['b', 'tv', 'ceu', 'casa assombrada', 'bola redonda']
    assert ajusta_lista(l1) == None
    assert l1 == ['b', 'tv', 'sessenta', 'bola redonda12']

def test_exemplo2():
    l2 = ['dez letras', 'dez letras um perigo', 'rica velhice', 'procura']
    assert ajusta_lista(l2) == None
    assert l2 == ['20dez letras um perigo', 'rica velhice12', 'procura']
