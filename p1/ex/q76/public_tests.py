import unittest
import sys

undertest = __import__(sys.argv[-1].split(".py")[0])
get_choque_horario = getattr(undertest, 'get_choque_horario', None)

class PublicTests(unittest.TestCase):

    def test_example(self):
        l1 = ["oac-4", "so-5", "atal-5", "prog1-1", "es-4"]
        assert get_choque_horario(l1) == ["oac-4", "es-4", "so-5", "atal-5"]

    def test2(self):
        l1 = ["oac-4", "loac-4", "so-5", "atal-5", "prog1-1", "es-4"]
        assert get_choque_horario(l1) == ["oac-4", "loac-4", "es-4", "so-5", "atal-5"]

if __name__ == '__main__':
    loader = unittest.TestLoader()
    runner = unittest.TextTestRunner()
    runner.run(loader.loadTestsFromModule(sys.modules[__name__]))
