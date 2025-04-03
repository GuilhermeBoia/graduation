package com.ufcg.psoft.commerce.exception;

public class PedidoNaoEstaProntoException extends CommerceException {
    
    public PedidoNaoEstaProntoException() {
        super("Pedido nao esta pronto!");
    }
    
}
