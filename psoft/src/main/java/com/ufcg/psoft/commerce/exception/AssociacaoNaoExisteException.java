package com.ufcg.psoft.commerce.exception;

public class AssociacaoNaoExisteException extends CommerceException {
    public AssociacaoNaoExisteException() {
        super("A associação consultada nao existe!");
    }
}
