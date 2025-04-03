package com.ufcg.psoft.commerce.service.notifica;

import com.ufcg.psoft.commerce.model.Cliente;
import org.springframework.stereotype.Service;

@Service
public class NotificaInteresseSaborPizzaServiceImpl implements NotificaInteresseSaborPizzaService {

    public void notificarClientes(Cliente cliente, String saborPizza) {
            System.out.println("Notificando o cliente " + cliente.getNome() +
                    ": O sabor " + saborPizza + " está disponível!");

    }
}
