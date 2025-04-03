package com.ufcg.psoft.commerce.exception;

public class PedidoClienteInvalidoException extends CommerceException{

    public PedidoClienteInvalidoException(){
        super("Pedido nao pertence a esse cliente.");
    }

}
