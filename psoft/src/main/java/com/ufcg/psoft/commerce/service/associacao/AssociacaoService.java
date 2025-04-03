package com.ufcg.psoft.commerce.service.associacao;

import com.ufcg.psoft.commerce.dto.associacao.AssociacaoResponseDTO;
import com.ufcg.psoft.commerce.model.enums.StatusAssociacao;
import com.ufcg.psoft.commerce.model.enums.StatusEntregador;

import org.springframework.stereotype.Service;

@Service
public interface AssociacaoService {


    AssociacaoResponseDTO associarEntregador (Long idEntregador, String codigoAcesso, Long idEstabelecimento);
    AssociacaoResponseDTO analisaAssociacao(Long id, String codigoAcesso, Long entregadorId, StatusAssociacao statusAssociacao);
    AssociacaoResponseDTO alteraStatusEntregador(Long id, String codigoAcesso, Long associacaoId, StatusEntregador status);
    }
