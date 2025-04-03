package com.ufcg.psoft.commerce.service.saborPizza;

import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaResponseDTO;

import java.util.List;

public interface SaborPizzaService {
    
    SaborPizzaResponseDTO alterar(Long id, Long idEstabelecimento, String codigoAcesso, SaborPizzaPostPutRequestDTO saborPizzaPostPutRequestDTO);

    List<SaborPizzaResponseDTO> listar(Long idEstabelecimento, String codigoAcesso);

    SaborPizzaResponseDTO recuperar(Long id, Long idEstabelecimento, String codigoAcesso);

    SaborPizzaResponseDTO criar(Long idEstabelecimento, String codigoAcesso, SaborPizzaPostPutRequestDTO saborPizzaPostPutRequestDTO);
  
    void remover(Long id, Long idEstabelecimento, String codigoAcesso);
  
}
