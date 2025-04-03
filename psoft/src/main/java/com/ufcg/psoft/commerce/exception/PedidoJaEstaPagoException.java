package com.ufcg.psoft.commerce.exception;

public class PedidoJaEstaPagoException extends CommerceException {

    public PedidoJaEstaPagoException() {
        super("O pedido ja esta pago!");
    }

}