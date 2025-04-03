package com.ufcg.psoft.commerce.service.estabelecimento;

import com.ufcg.psoft.commerce.dto.estabelecimento.EstabelecimentoPostPutDTO;
import com.ufcg.psoft.commerce.dto.estabelecimento.EstabelecimentoResponseDTO;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.SaborPizza;

public interface EstabelecimentoService {

    EstabelecimentoResponseDTO alterar(Long id, String codigoAcesso, EstabelecimentoPostPutDTO estabelecimentoPostPutDTO);

    EstabelecimentoResponseDTO criar(EstabelecimentoPostPutDTO estabelecimentoPostPutDTO);

    void remover(Long id, String codigoAcesso);

    Estabelecimento findEstabelecimento(Long idEstabelecimento);

    void adicionarSabor(Long id, SaborPizza sabor);

    void removerSabor(Long id, SaborPizza sabor);

    boolean existsById (Long id);

}
