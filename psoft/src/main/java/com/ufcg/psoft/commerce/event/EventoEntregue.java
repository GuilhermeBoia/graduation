package com.ufcg.psoft.commerce.event;

import com.ufcg.psoft.commerce.model.Cliente;
import com.ufcg.psoft.commerce.model.Entregador;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.Pedido;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EventoEntregue {
    
    private Pedido pedido;
    private Cliente cliente;
    private Estabelecimento estabelecimento;
    private Entregador entregador;

    public EventoEntregue(Pedido pedido){
        this.pedido = pedido;
        this.cliente = pedido.getCliente();
        this.estabelecimento = pedido.getEstabelecimento();
        this.entregador = pedido.getEntregador();
    }

}
