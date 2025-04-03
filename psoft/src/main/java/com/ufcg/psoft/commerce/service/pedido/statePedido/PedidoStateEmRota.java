package com.ufcg.psoft.commerce.service.pedido.statePedido;

import com.ufcg.psoft.commerce.exception.PedidoJaEstaPagoException;
import com.ufcg.psoft.commerce.exception.PedidoJaEstaProntoException;
import com.ufcg.psoft.commerce.exception.PedidoJaSaiuParaEntregaException;
import com.ufcg.psoft.commerce.exception.PedidoNaoPodeSerCanceladoException;
import com.ufcg.psoft.commerce.model.Pedido;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;

public class PedidoStateEmRota implements PedidoState {

    private Pedido pedido;

    public PedidoStateEmRota(Pedido pedido) {
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
        throw new PedidoJaSaiuParaEntregaException();
    }

    @Override
    public void confirmar() {
        this.pedido.setStatus(StatusPedido.PEDIDO_ENTREGUE);
        this.pedido.setState(new PedidoStateEntregue(pedido));
    }

    @Override
    public void cancelar() {
        throw new PedidoNaoPodeSerCanceladoException();
    }


    @Override
    public void pendente(){}
    
}
