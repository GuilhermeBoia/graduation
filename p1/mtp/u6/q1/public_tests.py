import unittest
import sys

module = sys.argv[-1].split(".py")[0]

class PublicTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        global primeiro_numero_acima
        undertest = __import__(sys.argv[-1].split(".py")[0])
        primeiro_numero_acima = getattr(undertest, 'primeiro_numero_acima', None)

    def test_simples(self):
        numeros = [4, 5, 6, 7, 8]
        assert primeiro_numero_acima(numeros) == 4

    def test_2(self):
        numeros = [6, 4, 5]
        assert primeiro_numero_acima(numeros) == 1

if __name__ == '__main__':
    loader = unittest.TestLoader()
    runner = unittest.TextTestRunner()
    runner.run(loader.loadTestsFromModule(sys.modules[__name__]))
