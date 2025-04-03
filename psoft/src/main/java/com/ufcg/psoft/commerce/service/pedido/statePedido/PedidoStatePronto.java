package com.ufcg.psoft.commerce.service.pedido.statePedido;

import com.ufcg.psoft.commerce.exception.PedidoJaEstaPagoException;
import com.ufcg.psoft.commerce.exception.PedidoJaEstaProntoException;
import com.ufcg.psoft.commerce.exception.PedidoNaoPodeSerCanceladoException;
import com.ufcg.psoft.commerce.exception.PedidoNaoSaiuParaEntregaException;
import com.ufcg.psoft.commerce.model.Pedido;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;

public class PedidoStatePronto implements PedidoState {

    private Pedido pedido;

    public PedidoStatePronto(Pedido pedido) {
        this.pedido = pedido;
    }

    @Override
    public void pagar() {
        throw new PedidoJaEstaPagoException();
    }

    @Override
    public void pronto() {
        throw new PedidoJaEstaProntoException();
    }

    @Override
    public void entregar() {
        this.pedido.setStatus(StatusPedido.PEDIDO_EM_ROTA);
        this.pedido.setState(new PedidoStateEmRota(pedido));
    }

    @Override
    public void confirmar() {
        throw new PedidoNaoSaiuParaEntregaException();
    }

    @Override
    public void cancelar() {
        throw new PedidoNaoPodeSerCanceladoException();
    }

    @Override
    public void pendente(){}

}
