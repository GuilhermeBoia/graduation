package com.ufcg.psoft.commerce.event;

import com.ufcg.psoft.commerce.model.Cliente;
import com.ufcg.psoft.commerce.model.Entregador;
import com.ufcg.psoft.commerce.model.Pedido;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EventoEmRota {
    
    private Entregador entregador;
    private Cliente cliente;
    private Pedido pedido;

    public EventoEmRota(Pedido pedido){
        this.entregador = pedido.getEntregador();
        this.cliente = pedido.getCliente();
        this.pedido = pedido;
    }


}
