package com.ufcg.psoft.commerce.exception;

public class CommerceException extends RuntimeException {
    public CommerceException() {
        super("Erro inesperado no Pits A!");
    }

    public CommerceException(String message) {
        super(message);
    }
}
