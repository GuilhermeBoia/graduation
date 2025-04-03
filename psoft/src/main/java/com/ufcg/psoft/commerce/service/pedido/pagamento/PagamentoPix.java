package com.ufcg.psoft.commerce.service.pedido.pagamento;

import com.ufcg.psoft.commerce.model.Pedido;
import lombok.NoArgsConstructor;

@NoArgsConstructor
public class PagamentoPix implements PagamentoStrategy {

    @Override
    public void pagar(Pedido pedido) {
        Double desconto = pedido.getTotal() * 0.95;
        pedido.setTotal(desconto);
        pedido.setPago(true);
    }
}
