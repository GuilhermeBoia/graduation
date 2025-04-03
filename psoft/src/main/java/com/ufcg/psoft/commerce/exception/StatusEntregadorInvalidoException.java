package com.ufcg.psoft.commerce.exception;

public class StatusEntregadorInvalidoException extends CommerceException {
    public StatusEntregadorInvalidoException() {
        super("Entregador não pode alterar Status para Ocupado");
    }
    
}
