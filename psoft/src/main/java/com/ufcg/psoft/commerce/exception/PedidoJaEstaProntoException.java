package com.ufcg.psoft.commerce.exception;

public class PedidoJaEstaProntoException extends CommerceException {
    
    public PedidoJaEstaProntoException() {
        super("O pedido ja esta pronto!");
    }
    
}
