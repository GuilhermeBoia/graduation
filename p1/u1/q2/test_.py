import re
from pathlib import Path

import pytest

ARQUIVO = "questoes.txt"
RESPOSTAS = ["pwd", "PATH", "PS1", "exit", "id", "ps", "echo", "pip"]

@pytest.fixture
def texto():
    try:
        return open(ARQUIVO).read()
    except:
        return ""

@pytest.fixture
def tem_token(texto):
    def tem_token(token):
        return re.search(r"\b" + token + r"\b", texto)
    return tem_token

@pytest.mark.parametrize("token", RESPOSTAS)
def test_q1(token, tem_token):
    assert tem_token(token)

def test_q2(texto):
    assert re.search(r"/home/.*/p1", texto)

def test_q3(texto):
    assert re.search(r".local/bin", texto)
