package com.ufcg.psoft.commerce.service.pizza;

import com.ufcg.psoft.commerce.dto.pizza.PizzaPostPutDTO;
import com.ufcg.psoft.commerce.model.Pizza;

public interface PizzaService {

    public Pizza criar(Long idEstabelecimento, PizzaPostPutDTO pizzaPostPutDTO);

}
