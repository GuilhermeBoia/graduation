package com.ufcg.psoft.commerce.exception;

public class AssociacaoInvalidaException extends CommerceException {
    public AssociacaoInvalidaException() {
        super("Essa associação não se refere ao seu estabelecimento.");
    }
}
