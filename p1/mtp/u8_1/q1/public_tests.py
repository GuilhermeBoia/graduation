import unittest
import sys

module = sys.argv[-1].split(".py")[0]

class PublicTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        global extensao_ordenada
        undertest = __import__(module)
        extensao_ordenada = getattr(undertest, 'extensao_ordenada', None)

    def test_exemplo(self):
        nums = [10, 20, 30, 40]
        novos = [15, 54]
        extensao_ordenada(nums, novos)
        assert nums == [10, 15, 20, 30, 40, 54]
    
if __name__ == '__main__':
    loader = unittest.TestLoader()
    runner = unittest.TextTestRunner()
    runner.run(loader.loadTestsFromModule(sys.modules[__name__]))
