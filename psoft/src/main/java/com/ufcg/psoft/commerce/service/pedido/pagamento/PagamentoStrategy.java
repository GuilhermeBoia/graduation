package com.ufcg.psoft.commerce.service.pedido.pagamento;

import com.ufcg.psoft.commerce.model.Pedido;

public interface PagamentoStrategy {
    void pagar(Pedido pedido);
}
