package com.ufcg.psoft.commerce.exception;

public class PedidoJaPossuiEntregadorException extends CommerceException{

    public PedidoJaPossuiEntregadorException(){
        super("Esse pedido ja possui entregador.");
    }

}
