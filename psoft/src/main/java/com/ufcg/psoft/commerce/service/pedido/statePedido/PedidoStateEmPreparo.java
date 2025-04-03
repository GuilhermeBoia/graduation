package com.ufcg.psoft.commerce.service.pedido.statePedido;

import com.ufcg.psoft.commerce.exception.PedidoJaEstaPagoException;
import com.ufcg.psoft.commerce.exception.PedidoNaoEstaProntoException;
import com.ufcg.psoft.commerce.model.Pedido;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;

public class PedidoStateEmPreparo implements PedidoState {

    private Pedido pedido;

    public PedidoStateEmPreparo(Pedido pedido) {
        this.pedido = pedido;
    }

    @Override
    public void pagar() {
        throw new PedidoJaEstaPagoException();
    }

    @Override
    public void pronto() {
        this.pedido.setStatus(StatusPedido.PEDIDO_PRONTO);
        this.pedido.setState(new PedidoStatePronto(pedido));
    }

    @Override
    public void entregar() {
        throw new PedidoNaoEstaProntoException();
    }

    @Override
    public void confirmar() {
        throw new PedidoNaoEstaProntoException();
    }

    @Override
    public void cancelar() {
        //Do nothing
    }


    @Override
    public void pendente(){}

}
