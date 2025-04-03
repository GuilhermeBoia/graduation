package com.ufcg.psoft.commerce.service.cardapio;

import java.util.List;

import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaCardapioDTO;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;

public interface ClienteVisualizaCardapioService {
    
    List<SaborPizzaCardapioDTO> visualizaCardapioEstabelecimento(Long idEstabelecimento);
    List<SaborPizzaCardapioDTO> visualizaCardapioEstabelecimentoTipo(Long idEstabelecimento, TipoPizza tipoPizza);

}
