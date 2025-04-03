package com.ufcg.psoft.commerce.service.pedido.statePedido;

public interface PedidoState {
    void pagar();
    void pronto();
    void entregar();
    void confirmar();
    void cancelar();
    void pendente();
}
