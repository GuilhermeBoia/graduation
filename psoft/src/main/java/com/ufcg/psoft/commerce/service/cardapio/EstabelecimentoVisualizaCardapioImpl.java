package com.ufcg.psoft.commerce.service.cardapio;

import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaCardapioDTO;
import com.ufcg.psoft.commerce.exception.CodigoDeAcessoInvalidoException;
import com.ufcg.psoft.commerce.exception.EstabelecimentoNaoExisteException;
import com.ufcg.psoft.commerce.exception.SaborNaoExisteException;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
import com.ufcg.psoft.commerce.repository.InteresseSaborPizzaRepository;
import com.ufcg.psoft.commerce.repository.SaborPizzaRepository;
import com.ufcg.psoft.commerce.service.estabelecimento.EstabelecimentoService;
import com.ufcg.psoft.commerce.service.notifica.NotificaInteresseSaborPizzaServiceImpl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Service
public class EstabelecimentoVisualizaCardapioImpl implements EstabelecimentoVisualizaCardapioService{

    @Autowired
    SaborPizzaRepository saborPizzaRepository;
    @Autowired
    EstabelecimentoRepository estabelecimentoRepository;
    @Autowired
    EstabelecimentoService estabelecimentoService;
    @Autowired
    InteresseSaborPizzaRepository interesseSaborPizzaRepository;
    @Autowired
    NotificaInteresseSaborPizzaServiceImpl notificaInteresseSaborPizzaService;

    @Override
    public List<SaborPizzaCardapioDTO> visualizarCardapio(Long idEstabelecimento, String codigoAcesso) {
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                .orElseThrow(EstabelecimentoNaoExisteException::new);
        if (!estabelecimentoService.existsById(idEstabelecimento)){
            throw new EstabelecimentoNaoExisteException();
        }
        if (!estabelecimento.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }
        ArrayList<SaborPizzaCardapioDTO> sabores = new java.util.ArrayList<>(saborPizzaRepository.findByEstabelecimentoId(idEstabelecimento).stream()
                .map(SaborPizzaCardapioDTO::new)
                .toList());

        sabores.sort(Comparator.comparing(SaborPizzaCardapioDTO::getDisponivel).reversed());

        return sabores;
    }


    @Override
    public String editaDisponibilidadePizza(Long idEstabelecimento, String codigoAcesso, String nomePizza) {
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                .orElseThrow(EstabelecimentoNaoExisteException::new);
        if (!estabelecimento.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }
        SaborPizza sabor = estabelecimento.getSabores().stream()
                .filter(s -> s.getNome().equals(nomePizza))
                .findFirst()
                .orElseThrow(SaborNaoExisteException::new);

        boolean estadoAnterior = sabor.getDisponivel();
        sabor.setDisponivel(!estadoAnterior);

        saborPizzaRepository.save(sabor);

        return "Disponibilidade alterada de " + estadoAnterior + " para " + sabor.getDisponivel();

    }
}
