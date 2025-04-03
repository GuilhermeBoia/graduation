package com.ufcg.psoft.commerce.service.notifica;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import com.ufcg.psoft.commerce.event.EventoEmRota;

@Component
public class NotificaPedidoEmRotaImpl implements NotificaPedidoEmRota {
    
    @EventListener
    public void notificaPedidoEmRota(EventoEmRota evento){

        String notificacao = "Olá " + evento.getCliente().getNome() +
        " Seu Pedido " + evento.getPedido().getId() + 
        " está em Rota!! O entregador é: " + evento.getEntregador().getNome() +
        " No veículo: " + evento.getEntregador().getVeiculo().getTipoVeiculo() +
        " Cor: " + evento.getEntregador().getVeiculo().getCorVeiculo() +
        " Placa: " + evento.getEntregador().getVeiculo().getPlaca();

        System.out.println(notificacao);

    }

}
