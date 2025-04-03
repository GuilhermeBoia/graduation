package com.ufcg.psoft.commerce.service.pedido.pagamento;

import com.ufcg.psoft.commerce.model.Pedido;
import lombok.NoArgsConstructor;

@NoArgsConstructor
public class PagamentoCredito implements PagamentoStrategy {

    @Override
    public void pagar(Pedido pedido) {
        pedido.setPago(true);
    }
}
