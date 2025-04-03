package com.ufcg.psoft.commerce.service.notifica;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import com.ufcg.psoft.commerce.event.EventoEntregue;

@Component
public class NotificaPedidoEntregueImpl implements NotificaPedidoEntregue{

    @Override
    @EventListener
    public void notificaPedidoEntregue(EventoEntregue evento) {

        String notificacao = "Olá, Estabelecimento " + evento.getEstabelecimento().getId() +
        " O pedido " + evento.getPedido().getId() + " já foi Entregue para o cliente " +
        evento.getCliente().getNome() + " pelo entregador " + evento.getEntregador().getNome();

        System.out.println(notificacao);
    }
    
}
