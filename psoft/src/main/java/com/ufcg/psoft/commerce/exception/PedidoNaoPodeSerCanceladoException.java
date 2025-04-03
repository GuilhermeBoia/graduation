package com.ufcg.psoft.commerce.exception;

public class PedidoNaoPodeSerCanceladoException extends CommerceException {

    public PedidoNaoPodeSerCanceladoException() {
        super("O pedido nao pode ser cancelado depois de pronto!");
    }

}