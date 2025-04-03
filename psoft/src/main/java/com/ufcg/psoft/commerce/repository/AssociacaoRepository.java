package com.ufcg.psoft.commerce.repository;

import com.ufcg.psoft.commerce.model.Associacao;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.Optional;

import java.util.List;

public interface AssociacaoRepository extends JpaRepository<Associacao, Long> {

    List<Associacao> findByEntregadorId(Long entregadorId);

    Optional<Associacao> findByEstabelecimentoIdAndEntregadorId(Long estabelecimentoId, Long entregadorI);

}
