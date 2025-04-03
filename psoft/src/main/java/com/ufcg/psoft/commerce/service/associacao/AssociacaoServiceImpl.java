package com.ufcg.psoft.commerce.service.associacao;

import com.ufcg.psoft.commerce.dto.associacao.AssociacaoResponseDTO;
import com.ufcg.psoft.commerce.exception.*;
import com.ufcg.psoft.commerce.model.Associacao;
import com.ufcg.psoft.commerce.model.Entregador;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.enums.StatusAssociacao;
import com.ufcg.psoft.commerce.model.enums.StatusEntregador;
import com.ufcg.psoft.commerce.repository.AssociacaoRepository;
import com.ufcg.psoft.commerce.repository.EntregadorRepository;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
import com.ufcg.psoft.commerce.service.estabelecimento.EstabelecimentoService;
import com.ufcg.psoft.commerce.service.pedido.PedidoService;

import java.time.LocalDateTime;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cglib.core.Local;
import org.springframework.stereotype.Service;

@Service
public class AssociacaoServiceImpl implements AssociacaoService{

    @Autowired
    EntregadorRepository entregadorRepository;

    @Autowired
    EstabelecimentoRepository estabelecimentoRepository;

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    AssociacaoRepository associacaoRepository;

    @Autowired
    PedidoService pedidoService;

    @Override
    public AssociacaoResponseDTO associarEntregador(Long idEntregador, String codigoAcesso, Long idEstabelecimento) {

        Entregador entregador = entregadorRepository.findById(idEntregador).orElseThrow(EntregadorNaoExisteException::new);
        if (!entregador.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }

        Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                .orElseThrow(EstabelecimentoNaoExisteException::new);

        Associacao associacao = new Associacao(entregador, estabelecimento);
        Associacao saved = associacaoRepository.save(associacao);
        return modelMapper.map(saved, AssociacaoResponseDTO.class);

    }

    @Override
    public AssociacaoResponseDTO analisaAssociacao(Long idEstabelecimento, String codigoAcesso, Long associacaoId, StatusAssociacao statusAssociacao) {

        Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento).orElseThrow(EstabelecimentoNaoExisteException::new);
        if (!estabelecimento.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }

        Associacao associacao = associacaoRepository.findById(associacaoId).orElseThrow(AssociacaoNaoExisteException::new);
        if (associacao.getEstabelecimento().getId() != idEstabelecimento) {
            throw new AssociacaoInvalidaException();
        }

        associacao.setStatus(statusAssociacao);

        if (statusAssociacao.equals(StatusAssociacao.APROVADO)) {
            associacao.setStatusEntregador(StatusEntregador.DESCANSO);
        }

        Associacao saved = associacaoRepository.save(associacao);
        return modelMapper.map(saved, AssociacaoResponseDTO.class);
    }

    @Override
    public AssociacaoResponseDTO alteraStatusEntregador(Long idEntregador, String codigoAcesso, Long associacaoId, StatusEntregador status) {

        Entregador entregador = entregadorRepository.findById(idEntregador).orElseThrow(EntregadorNaoExisteException::new);

        if (!entregador.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }

        Associacao associacao = associacaoRepository.findById(associacaoId).orElseThrow(AssociacaoNaoExisteException::new);
        if (associacao.getEntregador().getId() != idEntregador) {
            throw new AssociacaoInvalidaException();
        }

        if (status.equals(StatusEntregador.OCUPADO)) {
            throw new StatusEntregadorInvalidoException();
        }

        associacao.setStatusEntregador(status);

        if (status.equals(StatusEntregador.ATIVO)){
            associacao.setTempoAtivo(LocalDateTime.now());
            pedidoService.novoEntregadorAtivo(associacao.getEstabelecimento().getId(), idEntregador);
        } else {
            associacao.setTempoAtivo(null);
        }

        Associacao saved = associacaoRepository.save(associacao);
        return modelMapper.map(saved, AssociacaoResponseDTO.class);
    }

}
