package com.ufcg.psoft.commerce.service.estabelecimento;


import com.ufcg.psoft.commerce.dto.estabelecimento.EstabelecimentoPostPutDTO;
import com.ufcg.psoft.commerce.dto.estabelecimento.EstabelecimentoResponseDTO;
import com.ufcg.psoft.commerce.exception.CodigoDeAcessoInvalidoException;
import com.ufcg.psoft.commerce.exception.EstabelecimentoNaoExisteException;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;

import java.util.List;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class EstabelecimentoServiceImpl implements EstabelecimentoService {

    @Autowired
    ModelMapper modelMapper;
    @Autowired
    EstabelecimentoRepository estabelecimentoRepository;


    @Override
    public EstabelecimentoResponseDTO alterar(Long id, String codigoAcesso, EstabelecimentoPostPutDTO estabelecimentoPostPutDTO) {
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(id).orElseThrow(EstabelecimentoNaoExisteException::new);
        if (!estabelecimento.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }
        modelMapper.map(estabelecimentoPostPutDTO, estabelecimento);
        estabelecimentoRepository.save(estabelecimento);
        return modelMapper.map(estabelecimento, EstabelecimentoResponseDTO.class);
    }

    @Override
    public EstabelecimentoResponseDTO criar(EstabelecimentoPostPutDTO estabelecimentoPostPutDTO) {
        Estabelecimento estabelecimento = modelMapper.map(estabelecimentoPostPutDTO, Estabelecimento.class);
        estabelecimentoRepository.save(estabelecimento);
        return modelMapper.map(estabelecimento, EstabelecimentoResponseDTO.class);
    }

    @Override
    public void remover(Long id, String codigoAcesso) {
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(id).orElseThrow(EstabelecimentoNaoExisteException::new);
        if (!estabelecimento.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }
        estabelecimentoRepository.delete(estabelecimento);
    }

    public Estabelecimento findEstabelecimento(Long id){
        return estabelecimentoRepository.findById(id).orElseThrow(EstabelecimentoNaoExisteException::new);
    }

    public void adicionarSabor(Long id, SaborPizza sabor){
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(id).orElseThrow(EstabelecimentoNaoExisteException::new);
        List<SaborPizza> sabores = estabelecimento.getSabores();
        sabores.add(sabor);
        estabelecimento.setSabores(sabores);
    }

    public void removerSabor(Long id, SaborPizza sabor){
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(id).orElseThrow(EstabelecimentoNaoExisteException::new);
        List<SaborPizza> sabores = estabelecimento.getSabores();
        sabores.remove(sabor);
        estabelecimento.setSabores(sabores);
    }

    public boolean existsById(Long id) {
        return estabelecimentoRepository.existsById(id);
    }
}
