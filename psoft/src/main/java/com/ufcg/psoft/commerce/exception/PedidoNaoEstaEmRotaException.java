package com.ufcg.psoft.commerce.exception;

public class PedidoNaoEstaEmRotaException extends CommerceException {
    public PedidoNaoEstaEmRotaException() {
        super("Pedido não esta em rota");
    }
    
}
