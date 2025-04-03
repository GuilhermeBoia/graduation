package com.ufcg.psoft.commerce.service.pedido.statePedido;

import com.ufcg.psoft.commerce.exception.PedidoNaoFoiPagoException;
import com.ufcg.psoft.commerce.model.Pedido;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;

public class PedidoStateRecebido implements PedidoState {

    private Pedido pedido;

    public PedidoStateRecebido(Pedido pedido) {
        this.pedido = pedido;
    }
    
    @Override
    public void pagar() {
        pedido.setStatus(StatusPedido.PEDIDO_EM_PREPARO);
        pedido.setState(new PedidoStateEmPreparo(this.pedido));
    }

    @Override
    public void pronto() {
        throw new PedidoNaoFoiPagoException();
    }

    @Override
    public void entregar() {
        throw new PedidoNaoFoiPagoException();
    }

    @Override
    public void confirmar() {
        throw new PedidoNaoFoiPagoException();
    }

    @Override
    public void cancelar() {
        //Do nothing
    }

    @Override
    public void pendente(){}
    
}
