package com.ufcg.psoft.commerce.exception;

public class SaborNaoEncontradoException extends CommerceException {
    
    public SaborNaoEncontradoException(){
        super("Sabor nao encontrado nesse estabelecimento.");
    }
}
