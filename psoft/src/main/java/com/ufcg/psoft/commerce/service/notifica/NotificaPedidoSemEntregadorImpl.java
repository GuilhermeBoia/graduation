package com.ufcg.psoft.commerce.service.notifica;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import com.ufcg.psoft.commerce.event.EventoSemEntregador;

@Component
public class NotificaPedidoSemEntregadorImpl implements NotificaPedidoSemEntregador{

    @Override
    @EventListener
    public void notificaPedidoSemEntregador(EventoSemEntregador evento) {
        String notificacao = "Olá " + evento.getCliente().getNome() + " \n" +
        "O seu pedido " + evento.getPedido().getId() + " já está Pronto, mas infelizmente estamos sem entregadores disponíveis no momento!\n" +
        "Assim que um entregador puder entregar seu pedido avisaremos!";

        System.out.println(notificacao);
    }
    
}
