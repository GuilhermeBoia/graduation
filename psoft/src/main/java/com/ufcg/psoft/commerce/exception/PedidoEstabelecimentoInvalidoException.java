package com.ufcg.psoft.commerce.exception;

public class PedidoEstabelecimentoInvalidoException extends CommerceException{

    public PedidoEstabelecimentoInvalidoException(){
        super("Pedido nao pertence a esse estabelecimento.");
    }

}
