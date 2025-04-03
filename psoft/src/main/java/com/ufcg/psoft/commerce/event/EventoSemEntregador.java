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
public class EventoSemEntregador {
    
    private Cliente cliente;
    private Pedido pedido;

    public EventoSemEntregador(Pedido pedido){
        this.cliente = pedido.getCliente();
        this.pedido = pedido;
    }


}
