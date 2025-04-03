package com.ufcg.psoft.commerce.service.saborPizza;

import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaResponseDTO;
import com.ufcg.psoft.commerce.exception.CodigoDeAcessoInvalidoException;
import com.ufcg.psoft.commerce.exception.EstabelecimentoNaoExisteException;
import com.ufcg.psoft.commerce.exception.SaborNaoEncontradoException;
import com.ufcg.psoft.commerce.exception.SaborNaoExisteException;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
import com.ufcg.psoft.commerce.repository.SaborPizzaRepository;
import com.ufcg.psoft.commerce.service.estabelecimento.EstabelecimentoService;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class SaborPizzaServiceImpl implements SaborPizzaService {

    @Autowired
    ModelMapper modelMapper;
    @Autowired
    SaborPizzaRepository saborPizzaRepository;
    @Autowired
    EstabelecimentoRepository estabelecimentoRepository;
    @Autowired
    EstabelecimentoService estabelecimentoService;
    
    @Override
    public SaborPizzaResponseDTO alterar(Long id, Long idEstabelecimento, String codigoAcesso, SaborPizzaPostPutRequestDTO saborPizzaPostPutRequestDTO) {
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                                        .orElseThrow(EstabelecimentoNaoExisteException::new);
        if (!estabelecimento.getCodigo().equals(codigoAcesso)){
            throw new CodigoDeAcessoInvalidoException();
        }

        SaborPizza sabor = saborPizzaRepository.findById(id).orElseThrow(SaborNaoExisteException::new);
        if (!sabor.getEstabelecimento().getId().equals(idEstabelecimento)){
            throw new SaborNaoEncontradoException();
        }
        modelMapper.map(saborPizzaPostPutRequestDTO, sabor);
        saborPizzaRepository.save(sabor);
        return modelMapper.map(sabor, SaborPizzaResponseDTO.class);
    }

    @Override
    public SaborPizzaResponseDTO criar(Long idEstabelecimento, String codigoAcesso, SaborPizzaPostPutRequestDTO saborPizzaPostPutRequestDTO) {
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                                        .orElseThrow(EstabelecimentoNaoExisteException::new);
        if (!estabelecimento.getCodigo().equals(codigoAcesso)){
            throw new CodigoDeAcessoInvalidoException();
        }

        SaborPizza sabor = modelMapper.map(saborPizzaPostPutRequestDTO, SaborPizza.class);
        sabor.setEstabelecimento(estabelecimentoService.findEstabelecimento(idEstabelecimento));
        saborPizzaRepository.save(sabor);
        estabelecimentoService.adicionarSabor(idEstabelecimento, sabor);
        return modelMapper.map(sabor, SaborPizzaResponseDTO.class);
    }

    @Override
    public void remover(Long id, Long idEstabelecimento, String codigoAcesso) {
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                                        .orElseThrow(EstabelecimentoNaoExisteException::new);
        if (!estabelecimento.getCodigo().equals(codigoAcesso)){
            throw new CodigoDeAcessoInvalidoException();
        }

        SaborPizza sabor = saborPizzaRepository.findById(id).orElseThrow(SaborNaoExisteException::new);
        if (!sabor.getEstabelecimento().getId().equals(idEstabelecimento)){
            throw new SaborNaoEncontradoException();
        }
        estabelecimentoService.removerSabor(id, sabor);
        saborPizzaRepository.delete(sabor);
    }


    @Override
    public List<SaborPizzaResponseDTO> listar(Long idEstabelecimento, String codigoAcesso) {
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                                        .orElseThrow(EstabelecimentoNaoExisteException::new);
        if (!estabelecimento.getCodigo().equals(codigoAcesso)){
            throw new CodigoDeAcessoInvalidoException();
        }
    
        List<SaborPizza> sabores = saborPizzaRepository.findByEstabelecimentoId(idEstabelecimento);
        return sabores.stream()
                .map(SaborPizzaResponseDTO::new)
                .collect(Collectors.toList());
    }

    @Override
    public SaborPizzaResponseDTO recuperar(Long id, Long idEstabelecimento, String codigoAcesso) {
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                                        .orElseThrow(EstabelecimentoNaoExisteException::new);
        if (!estabelecimento.getCodigo().equals(codigoAcesso)){
            throw new CodigoDeAcessoInvalidoException();
        }

        SaborPizza sabor = saborPizzaRepository.findById(id).orElseThrow(SaborNaoExisteException::new); 
        if (!sabor.getEstabelecimento().getId().equals(idEstabelecimento)){
            throw new SaborNaoEncontradoException();
        }
        return new SaborPizzaResponseDTO(sabor);
    }
}
