package com.ufcg.psoft.commerce.service.InteresseSaborPizza;

import com.ufcg.psoft.commerce.dto.InteresseSaborPizza.InteresseSaborPizzaResponseDTO;

public interface InteresseSaborPizzaService {

    InteresseSaborPizzaResponseDTO registrarInteresse (Long clienteId, Long idSaborPizza);
    String notificarClientes(Long idSaborPizza);
    
}
