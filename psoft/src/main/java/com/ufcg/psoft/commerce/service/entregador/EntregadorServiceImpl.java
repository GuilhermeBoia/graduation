package com.ufcg.psoft.commerce.service.entregador;

import com.ufcg.psoft.commerce.exception.CodigoDeAcessoInvalidoException;
import com.ufcg.psoft.commerce.exception.EntregadorNaoExisteException;
import com.ufcg.psoft.commerce.model.Veiculo;
import com.ufcg.psoft.commerce.model.Entregador;
import com.ufcg.psoft.commerce.dto.entregador.EntregadorPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.entregador.EntregadorResponseDTO;
import com.ufcg.psoft.commerce.repository.VeiculoRepository;
import com.ufcg.psoft.commerce.repository.EntregadorRepository;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class EntregadorServiceImpl implements EntregadorService {
    @Autowired
    EntregadorRepository entregadorRepository;
    @Autowired
    VeiculoRepository veiculoRepository;
    @Autowired
    ModelMapper modelMapper;

    @Override
    public EntregadorResponseDTO alterar(Long id, String codigoAcesso, EntregadorPostPutRequestDTO entregadorPostPutRequestDTO) {
        Entregador entregador = entregadorRepository.findById(id).orElseThrow(EntregadorNaoExisteException::new);
        if (!entregador.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }

        modelMapper.map(entregadorPostPutRequestDTO, entregador);

        Veiculo veiculo = entregador.getVeiculo();
        veiculoRepository.save(veiculo);

        entregadorRepository.save(entregador);
        return modelMapper.map(entregador, EntregadorResponseDTO.class);
    }

    @Override
    public EntregadorResponseDTO criar(EntregadorPostPutRequestDTO entregadorPostPutRequestDTO) {
        Entregador entregador = modelMapper.map(entregadorPostPutRequestDTO, Entregador.class);
        Veiculo veiculo = entregador.getVeiculo();
        veiculoRepository.save(veiculo);
        entregadorRepository.save(entregador);
        return modelMapper.map(entregador, EntregadorResponseDTO.class);
    }

    @Override
    public void remover(Long id, String codigoAcesso) {
        Entregador entregador = entregadorRepository.findById(id).orElseThrow(EntregadorNaoExisteException::new);
        if (!entregador.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }
        Veiculo veiculo = entregador.getVeiculo();
        entregadorRepository.delete(entregador);
        veiculoRepository.delete(veiculo);
    }

    @Override
    public List<EntregadorResponseDTO> listar() {
        List<Entregador> entregadores = entregadorRepository.findAll();
        return entregadores.stream()
                .map(entregador -> modelMapper.map(entregador, EntregadorResponseDTO.class))
                .collect(Collectors.toList());
    }

    @Override
    public EntregadorResponseDTO recuperar(Long id) {
        Entregador entregador = entregadorRepository.findById(id).orElseThrow(EntregadorNaoExisteException::new);
        return modelMapper.map(entregador, EntregadorResponseDTO.class);
    }
}
