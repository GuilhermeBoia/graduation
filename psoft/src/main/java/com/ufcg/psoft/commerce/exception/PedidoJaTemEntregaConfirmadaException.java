package com.ufcg.psoft.commerce.exception;

public class PedidoJaTemEntregaConfirmadaException extends CommerceException {
    
    public PedidoJaTemEntregaConfirmadaException() {
        super("O pedido ja tem entrega confirmada!");
    }
    
}
