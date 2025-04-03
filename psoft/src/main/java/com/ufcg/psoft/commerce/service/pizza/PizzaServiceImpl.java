package com.ufcg.psoft.commerce.service.pizza;

import com.ufcg.psoft.commerce.dto.pizza.PizzaPostPutDTO;
import com.ufcg.psoft.commerce.exception.*;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.Pizza;
import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.model.enums.TamanhoPizza;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
import com.ufcg.psoft.commerce.repository.PizzaRepository;
import com.ufcg.psoft.commerce.repository.SaborPizzaRepository;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class PizzaServiceImpl implements PizzaService {

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    PizzaRepository pizzaRepository;
    
    @Autowired
    EstabelecimentoRepository estabelecimentoRepository;
    
    @Autowired
    SaborPizzaRepository saborPizzaRepository;

    public Pizza criar(Long idEstabelecimento, PizzaPostPutDTO pizzaPostPutDTO) {
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento).orElseThrow(EstabelecimentoNaoExisteException::new);

        List<Long> saboresId = pizzaPostPutDTO.getSaboresId();
        boolean saborInexistente = saboresId.stream().anyMatch(saborId -> !saborPizzaRepository.existsById(saborId));
        if (saborInexistente) {
            throw new SaborNaoExisteException();
        }

        TamanhoPizza tamanhoPizza = pizzaPostPutDTO.getTamanho();
        if ((saboresId.size() > 1 && tamanhoPizza == TamanhoPizza.MEDIA) || (saboresId.size() > 2)) {
            throw new PizzaQuantidadeSaboresInvalidaException();
        }

        List<SaborPizza> sabores = saborPizzaRepository.findAllById(saboresId);
        boolean saborIndisponivel = sabores.stream().anyMatch(sabor -> !sabor.getDisponivel());
        if (saborIndisponivel) {
            throw new SaborIndisponivelException();
        }
        boolean saborInvalidoEstabelecimento = sabores.stream().anyMatch(sabor -> sabor.getEstabelecimento() != estabelecimento);
        if (saborInvalidoEstabelecimento) {
            throw new SaborNaoEncontradoException();
        }

        Pizza pizza = new Pizza(tamanhoPizza, sabores);
        return pizzaRepository.save(pizza);
    }

}
