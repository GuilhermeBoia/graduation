package com.ufcg.psoft.commerce.service.cardapio;

import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaCardapioDTO;

import java.util.List;

public interface EstabelecimentoVisualizaCardapioService {

    List<SaborPizzaCardapioDTO> visualizarCardapio(Long idEstabelecimento, String codigoAcesso);
    String editaDisponibilidadePizza(Long idEstabelecimento, String codigoAcesso, String nomePizza);

}
