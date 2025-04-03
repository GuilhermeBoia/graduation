package com.ufcg.psoft.commerce.service.notifica;

import com.ufcg.psoft.commerce.event.EventoSemEntregador;

public interface NotificaPedidoSemEntregador {

    void notificaPedidoSemEntregador(EventoSemEntregador evento);
}
