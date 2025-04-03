import re
from pathlib import Path

import pytest

ARQUIVO = "questoes.txt"

@pytest.fixture
def texto():
    try:
        return open(ARQUIVO).read()
    except:
        return ""

@pytest.fixture
def tem_token(texto):
    def tem_token(token, icase=False):
        icase = "(?i)" if icase else ""
        return re.search(icase + r"\b" + token + r"\b", texto)
    return tem_token


@pytest.mark.parametrize("ic_token", ["Bash", "Python"])
def test_case_insensitive(tem_token, ic_token):
    assert tem_token(ic_token, icase=True)


@pytest.mark.parametrize("token", ["mkdir", "cd", "ls"])
def test_case_sensitive(tem_token, token):
    assert tem_token(token, icase=False)
