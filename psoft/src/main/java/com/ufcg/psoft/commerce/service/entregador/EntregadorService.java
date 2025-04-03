package com.ufcg.psoft.commerce.service.entregador;

import com.ufcg.psoft.commerce.dto.entregador.EntregadorPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.entregador.EntregadorResponseDTO;

import java.util.List;

public interface EntregadorService {

    EntregadorResponseDTO alterar(Long id, String codigoAcesso, EntregadorPostPutRequestDTO entregadorPostPutRequestDTO);

    List<EntregadorResponseDTO> listar();

    EntregadorResponseDTO recuperar(Long id);

    EntregadorResponseDTO criar(EntregadorPostPutRequestDTO entregadorPostPutRequestDTO);

    void remover(Long id, String codigoAcesso);

}
