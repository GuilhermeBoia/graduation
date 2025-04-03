import unittest
import sys

module = sys.argv[-1].split(".py")[0]

class PublicTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        global filtra_votos_validos
        undertest = __import__(module)
        filtra_votos_validos = getattr(undertest, 'filtra_votos_validos', None)

    def test_exemplo(self):
        votos = [1,1,1,1,12,1,2,1,2,2,2]
        candidatos = [1,2]
        filtra_votos_validos(votos,candidatos)
        assert votos == [1,1,1,1,1,2,1,2,2,2]


    def test_exemplo_2(self):
        votos = [1,2]
        candidatos = [1,2]
        filtra_votos_validos(votos,candidatos)
        assert votos == [1,2]

if __name__ == '__main__':
    loader = unittest.TestLoader()
    runner = unittest.TextTestRunner()
    runner.run(loader.loadTestsFromModule(sys.modules[__name__]))
