package com.ufcg.psoft.commerce.exception;

public class SaborIndisponivelException extends CommerceException {

    public SaborIndisponivelException(){
        super("Sabor esta indisponivel no momento.");
    }
}
