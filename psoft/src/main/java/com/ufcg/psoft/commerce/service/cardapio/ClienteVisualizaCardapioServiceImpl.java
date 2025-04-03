package com.ufcg.psoft.commerce.service.cardapio;

import java.util.List;

import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaCardapioDTO;
import com.ufcg.psoft.commerce.exception.EstabelecimentoNaoExisteException;
import com.ufcg.psoft.commerce.repository.SaborPizzaRepository;
import com.ufcg.psoft.commerce.service.estabelecimento.EstabelecimentoService;

@Service
public class ClienteVisualizaCardapioServiceImpl implements ClienteVisualizaCardapioService {

    @Autowired
    SaborPizzaRepository saborPizzaRepository;
    @Autowired
    EstabelecimentoService estabelecimentoService;

    @Override
    public List<SaborPizzaCardapioDTO> visualizaCardapioEstabelecimento(Long idEstabelecimento) {
        if (!estabelecimentoService.existsById(idEstabelecimento)){
            throw new EstabelecimentoNaoExisteException();
        }
        return saborPizzaRepository.findByEstabelecimentoId(idEstabelecimento).stream()
                .filter(SaborPizza::getDisponivel)
                .map(SaborPizzaCardapioDTO::new)
                .toList();
    }

    @Override
    public List<SaborPizzaCardapioDTO> visualizaCardapioEstabelecimentoTipo(Long idEstabelecimento, TipoPizza tipoPizza) {
        if (!estabelecimentoService.existsById(idEstabelecimento)){
            throw new EstabelecimentoNaoExisteException();
        }
        return saborPizzaRepository.findByEstabelecimentoIdAndTipo(idEstabelecimento, tipoPizza).stream()
                .filter(SaborPizza::getDisponivel)
                .map(SaborPizzaCardapioDTO::new)
                .toList();
    }
    
}
