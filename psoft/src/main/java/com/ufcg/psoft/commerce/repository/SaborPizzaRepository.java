package com.ufcg.psoft.commerce.repository;

import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

public interface SaborPizzaRepository extends JpaRepository<SaborPizza, Long> {
    List<SaborPizza> findByEstabelecimentoId(Long estabelecimentoId);

    List<SaborPizza> findByEstabelecimentoIdAndTipo(Long idEstabelecimento, TipoPizza tipoPizza);
}
