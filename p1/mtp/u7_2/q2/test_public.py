from undertst import atualiza_convocados

def test_publico():
    l = [('Alisson', 0), ('Marquinhos', 0), ('Casemiro', 1), ('Neymar', 2)]
    assert atualiza_convocados(l, ('Gabigol', 2)) == None
    assert l == [('Alisson', 0), ('Marquinhos', 0), ('Casemiro', 1), ('Gabigol', 2),('Neymar', 2)]
