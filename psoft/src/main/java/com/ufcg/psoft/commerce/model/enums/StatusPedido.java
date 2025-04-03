package com.ufcg.psoft.commerce.model.enums;
import java.util.Comparator;

public enum StatusPedido {
    PEDIDO_RECEBIDO(1),
    PEDIDO_EM_PREPARO(2),
    PEDIDO_PRONTO(3),
    PEDIDO_EM_ROTA(5),
    PEDIDO_ENTREGUE(6),
    PEDIDO_PENDENTE(4);

    private final int prioridade;

    StatusPedido(int prioridade) {
        this.prioridade = prioridade;
    }

    public int getPrioridade() {
        return prioridade;
    }

    public static Comparator<StatusPedido> byPriority() {
        return Comparator.comparingInt(StatusPedido::getPrioridade);
    }
}









