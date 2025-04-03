package com.ufcg.psoft.commerce.service.pedido.statePedido;

import com.ufcg.psoft.commerce.exception.PedidoJaTemEntregaConfirmadaException;
import com.ufcg.psoft.commerce.exception.PedidoNaoPodeSerCanceladoException;
import com.ufcg.psoft.commerce.model.Pedido;

public class PedidoStateEntregue implements PedidoState {

    @SuppressWarnings("unused")
    private Pedido pedido;

    public PedidoStateEntregue(Pedido pedido) {
        this.pedido = pedido;
    }
    
    @Override
    public void pagar() {     
        throw new PedidoJaTemEntregaConfirmadaException();
    }

    @Override
    public void pronto() {
        throw new PedidoJaTemEntregaConfirmadaException();
    }

    @Override
    public void entregar() {
        throw new PedidoJaTemEntregaConfirmadaException();
    }

    @Override
    public void confirmar() {
        throw new PedidoJaTemEntregaConfirmadaException();
    }

    @Override
    public void cancelar() {
        throw new PedidoNaoPodeSerCanceladoException();
    }


    @Override
    public void pendente(){}
    
}
