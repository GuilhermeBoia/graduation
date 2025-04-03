package com.ufcg.psoft.commerce.service.pedido;

import com.ufcg.psoft.commerce.dto.pedido.PedidoPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoResponseDTO;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;
import com.ufcg.psoft.commerce.model.enums.TipoPagamento;

import java.util.List;

public interface PedidoService {

    PedidoResponseDTO criar(Long idCliente, String codigoAcesso, Long idEstabelecimento, PedidoPostPutRequestDTO pedidoPostPutRequestDTO);
    PedidoResponseDTO recuperar(Long pedidoId, Long idCliente, String codigoAcesso);
    List<PedidoResponseDTO> listar(Long idCliente, String codigoAcesso);
    List<PedidoResponseDTO> listarPorStatus(Long idCliente, String codigoAcesso, StatusPedido status);
    void remover(Long pedidoId, Long idCliente, String codigoAcesso);
    PedidoResponseDTO atualizar(Long idPedido, Long idCliente, String codigoAcesso, PedidoPostPutRequestDTO pedidoPostPutRequestDTO);
    PedidoResponseDTO confirmarPagamento(Long idPedido, Long idCliente, String codigoAcesso, TipoPagamento tipoPagamento);
    PedidoResponseDTO pedidoPronto(Long idPedido, Long idEstabelecimento, String codigoAcesso);
    void adicionarEntregador(Long idPedido, Long idEstabelecimento);
    PedidoResponseDTO confirmarEntrega(Long idPedido, Long idCliente, String codigoAcesso);
    void novoEntregadorAtivo(Long idEstabelecimento, Long idEntregador);
}
