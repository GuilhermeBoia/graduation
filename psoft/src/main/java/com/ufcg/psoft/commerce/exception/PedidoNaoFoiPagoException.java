package com.ufcg.psoft.commerce.exception;

public class PedidoNaoFoiPagoException extends CommerceException {
    
    public PedidoNaoFoiPagoException() {
        super("Pedido ainda nao foi pago");
    }
    
}
