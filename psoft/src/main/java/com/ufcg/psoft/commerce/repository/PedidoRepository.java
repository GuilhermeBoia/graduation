package com.ufcg.psoft.commerce.repository;

import com.ufcg.psoft.commerce.model.Pedido;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;


@Repository
public interface PedidoRepository extends JpaRepository<Pedido, Long> {

    List<Pedido> findAllByClienteId(Long idCliente);
    List<Pedido> findAllByStatusAndEntregadorIdNull(StatusPedido status, Sort sort);
}
