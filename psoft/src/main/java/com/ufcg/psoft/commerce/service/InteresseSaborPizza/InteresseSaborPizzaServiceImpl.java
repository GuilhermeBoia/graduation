package com.ufcg.psoft.commerce.service.InteresseSaborPizza;

import com.ufcg.psoft.commerce.dto.InteresseSaborPizza.InteresseSaborPizzaResponseDTO;
import com.ufcg.psoft.commerce.exception.ClienteNaoExisteException;
import com.ufcg.psoft.commerce.exception.SaborNaoExisteException;
import com.ufcg.psoft.commerce.model.Cliente;
import com.ufcg.psoft.commerce.model.InteresseSaborPizza;
import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.repository.ClienteRepository;
import com.ufcg.psoft.commerce.repository.InteresseSaborPizzaRepository;
import com.ufcg.psoft.commerce.repository.SaborPizzaRepository;
import com.ufcg.psoft.commerce.service.notifica.NotificaInteresseSaborPizzaService;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;


@Service
public class InteresseSaborPizzaServiceImpl implements InteresseSaborPizzaService {


    @Autowired
    InteresseSaborPizzaRepository interesseSaborPizzaRepository;
    @Autowired
    ClienteRepository clienteRepository;
    @Autowired
    SaborPizzaRepository saborPizzaRepository;
    @Autowired
    ModelMapper modelMapper;
    @Autowired
    NotificaInteresseSaborPizzaService notificaInteresseSaborPizzaService;

    @Override
    public InteresseSaborPizzaResponseDTO registrarInteresse(Long idCliente, Long idSaborPizza) {
        SaborPizza sabor = saborPizzaRepository.findById(idSaborPizza)
                .orElseThrow(SaborNaoExisteException::new);
        Cliente cliente = clienteRepository.findById(idCliente)
                .orElseThrow(ClienteNaoExisteException::new);

        InteresseSaborPizza interesseSaborPizza = new InteresseSaborPizza(null, cliente, sabor, false);

        InteresseSaborPizza saved = interesseSaborPizzaRepository.save(interesseSaborPizza);

        return modelMapper.map(saved, InteresseSaborPizzaResponseDTO.class);
    }

    @Override
    public String notificarClientes(Long idSaborPizza) {
        SaborPizza sabor = saborPizzaRepository.findById(idSaborPizza)
                .orElseThrow(() -> new SaborNaoExisteException());

        List<InteresseSaborPizza> interesses = interesseSaborPizzaRepository
                .findBySaborPizzaAndNotificadoFalse(sabor);

        if (interesses.isEmpty()) {
            return "Nenhum cliente com interesse neste sabor.";
        }

        Integer quantidadeNotificados = 0;

        for (InteresseSaborPizza interesse : interesses) {
            quantidadeNotificados++;
            notificaInteresseSaborPizzaService.notificarClientes(interesse.getCliente(), sabor.getNome());
            interesse.setNotificado(true);
            interesseSaborPizzaRepository.save(interesse);
        }

        return quantidadeNotificados + " clientes notificados!";
    }


}
