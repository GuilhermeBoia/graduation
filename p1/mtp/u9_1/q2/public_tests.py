import unittest
import sys

module = sys.argv[-1].split(".py")[0]

class PublicTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        global scroll
        undertest = __import__(module)
        scroll = getattr(undertest, 'scroll', None)

    def test_exemplo(self):
        m = [[  1,  2,  3,  4 ],
             [  5,  6,  7,  8 ],
             [  9, 10, 11, 12 ],
             [ 13, 14, 15, 16 ],
             [ 17, 18, 19, 20 ]]
            
        scroll(m)
        assert m == [[  5,  6,  7,  8 ],
                     [  9, 10, 11, 12 ],
                     [ 13, 14, 15, 16 ],
                     [ 17, 18, 19, 20 ],
                     [  0,  0,  0,  0 ]]
    
    

if __name__ == '__main__':
    loader = unittest.TestLoader()
    runner = unittest.TextTestRunner()
    runner.run(loader.loadTestsFromModule(sys.modules[__name__]))
