package com.ufcg.psoft.commerce.repository;

import com.ufcg.psoft.commerce.model.Entregador;
import com.ufcg.psoft.commerce.model.enums.StatusEntregador;
import org.springframework.data.domain.Sort;


import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

public interface EntregadorRepository extends JpaRepository<Entregador, Long> {

    List<Entregador> findAllByAssociacoesStatusEntregadorAndAssociacoesEstabelecimentoId(StatusEntregador status, Long idEstabelecimento, Sort sort);

}
