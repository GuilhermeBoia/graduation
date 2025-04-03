import unittest
import sys

module = sys.argv[-1].split(".py")[0]

class PublicTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        global ordena_tipos
        undertest = __import__(module)
        ordena_tipos = getattr(undertest, 'ordena_tipos', None)


    def test_basico(self):
        assert ordena_tipos(['1a', '2', 'e', '4', '4.4', 'e6', '8']) == ['2', '4', '8', 'e', '1a', '4.4', 'e6']


if __name__ == '__main__':
    loader = unittest.TestLoader()
    runner = unittest.TextTestRunner()
    runner.run(loader.loadTestsFromModule(sys.modules[__name__]))
