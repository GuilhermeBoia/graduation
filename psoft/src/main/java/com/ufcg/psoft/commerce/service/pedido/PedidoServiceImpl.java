package com.ufcg.psoft.commerce.service.pedido;

import com.ufcg.psoft.commerce.dto.pedido.PedidoPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoResponseDTO;
import com.ufcg.psoft.commerce.event.EventoEmRota;
import com.ufcg.psoft.commerce.event.EventoEntregue;
import com.ufcg.psoft.commerce.event.EventoSemEntregador;
import com.ufcg.psoft.commerce.exception.*;
import com.ufcg.psoft.commerce.model.*;
import com.ufcg.psoft.commerce.model.enums.StatusAssociacao;
import com.ufcg.psoft.commerce.model.enums.StatusEntregador;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;
import com.ufcg.psoft.commerce.model.enums.TipoPagamento;
import com.ufcg.psoft.commerce.repository.AssociacaoRepository;
import com.ufcg.psoft.commerce.repository.ClienteRepository;
import com.ufcg.psoft.commerce.repository.EnderecoRepository;
import com.ufcg.psoft.commerce.repository.EntregadorRepository;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
import com.ufcg.psoft.commerce.repository.PedidoRepository;
import com.ufcg.psoft.commerce.repository.SaborPizzaRepository;
import com.ufcg.psoft.commerce.service.notifica.NotificaPedidoEmRota;
import com.ufcg.psoft.commerce.service.pedido.pagamento.*;
import com.ufcg.psoft.commerce.service.pizza.PizzaService;

import jakarta.validation.Valid;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.stream.Collectors;
import org.springframework.data.domain.Sort;


@Service
public class PedidoServiceImpl implements PedidoService {

    private Map<TipoPagamento, PagamentoStrategy> pagamentoMap = Map.of(
            TipoPagamento.CREDITO, new PagamentoCredito(),
            TipoPagamento.DEBITO, new PagamentoDebito(),
            TipoPagamento.PIX, new PagamentoPix()
    );

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    PedidoRepository pedidoRepository;

    @Autowired
    ClienteRepository clienteRepository;

    @Autowired
    EstabelecimentoRepository estabelecimentoRepository;

    @Autowired
    SaborPizzaRepository saborPizzaRepository;
    @Autowired
    PizzaService pizzaService;

    @Autowired
    EnderecoRepository enderecoRepository;

    @Autowired
    EntregadorRepository entregadorRepository;

    @Autowired
    AssociacaoRepository associacaoRepository;

    @Autowired
    NotificaPedidoEmRota notificaPedidoEmRota;

    @Autowired
    ApplicationEventPublisher eventPublisher;

    public PedidoResponseDTO criar(Long idCliente, String codigoAcesso, Long idEstabelecimento, @Valid PedidoPostPutRequestDTO pedidoPostPutRequestDTO) {

        Cliente cliente = clienteRepository.findById(idCliente)
                        .orElseThrow(ClienteNaoExisteException::new);
        Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                        .orElseThrow(EstabelecimentoNaoExisteException::new);

        if (!cliente.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }

        List<Pizza> pizzas = pedidoPostPutRequestDTO.getPizzas().stream()
                .map(pizzaDTO -> pizzaService.criar(idEstabelecimento, pizzaDTO))
                .collect(Collectors.toList());

        Endereco endereco = (pedidoPostPutRequestDTO.getEndereco() != null )
                ? modelMapper.map(pedidoPostPutRequestDTO.getEndereco(), Endereco.class)
                : cliente.getEndereco();
        enderecoRepository.save(endereco);
        double total = calcularPreco(pizzas);
        Pedido pedido = new Pedido(cliente, estabelecimento, pizzas, endereco, total);
        pizzas.forEach(pizza -> pizza.setPedido(pedido));
        pedidoRepository.save(pedido);
        
        return new PedidoResponseDTO(pedido);
    }

    public PedidoResponseDTO recuperar(Long pedidoId, Long idCliente, String codigoAcesso) {
        Cliente cliente = clienteRepository.findById(idCliente).orElseThrow(ClienteNaoExisteException::new);
        if (!cliente.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }
        Pedido pedido = pedidoRepository.findById(pedidoId).orElseThrow(PedidoNaoExisteException::new);
        if (pedido.getCliente() != cliente) {
            throw new PedidoClienteInvalidoException();
        }
        return new PedidoResponseDTO(pedido);
    }

    public List<PedidoResponseDTO> listar(Long idCliente, String codigoAcesso) {
        Cliente cliente = clienteRepository.findById(idCliente)
                        .orElseThrow(ClienteNaoExisteException::new);
        if (!cliente.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }
        List<Pedido> pedidos = pedidoRepository.findAllByClienteId(idCliente);
        pedidos.sort(Comparator
                .comparing(Pedido::getStatus, StatusPedido.byPriority())
                .thenComparing(Pedido::getDataHora, Comparator.reverseOrder())
        );
        return pedidos.stream()
               .map(PedidoResponseDTO::new)
               .collect(Collectors.toList());
    }

    public List<PedidoResponseDTO> listarPorStatus(Long idCliente, String codigoAcesso, StatusPedido status) {
        Cliente cliente = clienteRepository.findById(idCliente)
                .orElseThrow(ClienteNaoExisteException::new);
        if (!cliente.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }

        List<Pedido> pedidos = pedidoRepository.findAllByClienteId(idCliente);

        List<Pedido> pedidosFiltradosPorStatus = pedidos.stream()
                .filter(pedido -> pedido.getStatus().equals(status))
                .collect(Collectors.toList());

        return pedidosFiltradosPorStatus.stream()
                .map(PedidoResponseDTO::new)
                .collect(Collectors.toList());
    }

    public void remover(Long pedidoId, Long idCliente, String codigoAcesso) {
        Cliente cliente = clienteRepository.findById(idCliente).orElseThrow(ClienteNaoExisteException::new);
        if (!cliente.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }
        Pedido pedido = pedidoRepository.findById(pedidoId).orElseThrow(PedidoNaoExisteException::new);

        if (!pedido.getCliente().equals(cliente)) {
            throw new PedidoClienteInvalidoException();
        }

        pedido.getState().cancelar();
        pedidoRepository.delete(pedido);
    }

    public PedidoResponseDTO atualizar(Long idPedido, Long idCliente, String codigoAcesso, @Valid PedidoPostPutRequestDTO pedidoPostPutRequestDTO) {

        Cliente cliente = clienteRepository.findById(idCliente)
                          .orElseThrow(ClienteNaoExisteException::new);
        if (!cliente.getCodigo().equals(codigoAcesso)) {
            throw new CodigoDeAcessoInvalidoException();
        }

        Pedido pedido = pedidoRepository.findById(idPedido)
                        .orElseThrow(PedidoNaoExisteException::new);
        if (pedido.getCliente() != cliente) {
            throw new PedidoClienteInvalidoException();
        }

        pedido.getPizzas().clear();
        List<Pizza> novasPizzas = pedidoPostPutRequestDTO.getPizzas().stream()
                    .map(pizzaDTO -> {
                        Pizza pizza = pizzaService.criar(pedido.getEstabelecimento().getId(), pizzaDTO);
                        pizza.setPedido(pedido);
                        return pizza;
                    })
                    .collect(Collectors.toList());
        pedido.getPizzas().addAll(novasPizzas);

        Endereco endereco = (pedidoPostPutRequestDTO.getEndereco() != null )
                ? modelMapper.map(pedidoPostPutRequestDTO.getEndereco(), Endereco.class)
                : cliente.getEndereco();
        enderecoRepository.save(endereco);
        pedido.setEndereco(endereco);
        pedido.setTotal(calcularPreco(novasPizzas));
        Pedido pedidoAtualizado = pedidoRepository.save(pedido);

        return new PedidoResponseDTO(pedidoAtualizado);

        }

        private double calcularPreco(List<Pizza> pizzas) {
            return pizzas.stream()
                    .mapToDouble(pizza -> pizza.calculaPreco())
                    .sum();
        }

        public PedidoResponseDTO confirmarPagamento(Long idPedido, Long idCliente, String codigoAcesso, TipoPagamento tipoPagamento) {
            Cliente cliente = clienteRepository.findById(idCliente)
                    .orElseThrow(ClienteNaoExisteException::new);
            if (!cliente.getCodigo().equals(codigoAcesso)) {
                throw new CodigoDeAcessoInvalidoException();
            }

            Pedido pedido = pedidoRepository.findById(idPedido)
                    .orElseThrow(PedidoNaoExisteException::new);
            if (pedido.getCliente() != cliente) {
                throw new PedidoClienteInvalidoException();
            }
            if (pedido.isPago()) {
                throw new PedidoJaEstaPagoException();
            }

            pagamentoMap.get(tipoPagamento).pagar(pedido);
            // Pedido Em Recebido para Pedido Em Preparo
            pedido.getState().pagar();
            pedidoRepository.flush();
            return new PedidoResponseDTO(pedido);
        }

        public PedidoResponseDTO pedidoPronto(Long idPedido, Long idEstabelecimento, String codigoAcesso) {
            Pedido pedido = pedidoRepository.findById(idPedido)
                    .orElseThrow(PedidoNaoExisteException::new);

            Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                    .orElseThrow(EstabelecimentoNaoExisteException::new);

            if (!estabelecimento.getCodigo().equals(codigoAcesso)) {
                throw new CodigoDeAcessoInvalidoException();
            }

            if (pedido.getEstabelecimento() != estabelecimento) {
                throw new PedidoEstabelecimentoInvalidoException();
            }

            // Pedido Em Preparo para Pedido pronto
            pedido.getState().pronto();
            adicionarEntregador(idPedido, idEstabelecimento);
            pedidoRepository.flush();
            return new PedidoResponseDTO(pedido);
        }

        public void adicionarEntregador(Long idPedido, Long idEstabelecimento) {

            Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                    .orElseThrow(EstabelecimentoNaoExisteException::new);

            Pedido pedido = pedidoRepository.findById(idPedido)
                    .orElseThrow(PedidoNaoExisteException::new);

            if (pedido.getEstabelecimento() != estabelecimento) {
                throw new PedidoEstabelecimentoInvalidoException();
            }

            if (pedido.getEntregador() != null) {
                throw new PedidoJaPossuiEntregadorException();
            }
            
            Sort sort = Sort.by(Sort.Order.asc("associacoes.tempoAtivo"));
            List<Entregador> entregadoresDisponiveis = entregadorRepository.findAllByAssociacoesStatusEntregadorAndAssociacoesEstabelecimentoId(StatusEntregador.ATIVO, idEstabelecimento, sort);

            if (entregadoresDisponiveis.isEmpty()){
                pedido.setTempoEsperaEntregador(LocalDateTime.now());
                notificaPedidoSemEntregador(pedido);
            } else {

                Entregador entregador = entregadoresDisponiveis.remove(0);
                pedido.setEntregador(entregador);
                pedido.getState().entregar();

                Associacao associacao = associacaoRepository.findByEstabelecimentoIdAndEntregadorId(idEstabelecimento, entregador.getId())
                                    .orElseThrow(AssociacaoInvalidaException::new);
                associacao.setStatusEntregador(StatusEntregador.OCUPADO);

                notificaPedidoEmRota(pedido);
            }
            pedidoRepository.flush();

        }

        private void adicionarEntregadorEspecifico(Long idPedido, Long idEstabelecimento, Long idEntregador){

            Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
                    .orElseThrow(EstabelecimentoNaoExisteException::new);

            Pedido pedido = pedidoRepository.findById(idPedido)
                    .orElseThrow(PedidoNaoExisteException::new);

            Entregador entregador = entregadorRepository.findById(idEntregador)
                    .orElseThrow(EntregadorNaoExisteException::new);

            if (pedido.getEstabelecimento() != estabelecimento) {
                throw new PedidoEstabelecimentoInvalidoException();
            }

            if (pedido.getEntregador() != null) {
                throw new PedidoJaPossuiEntregadorException();
            }

            pedido.setEntregador(entregador);
            pedido.getState().entregar();

            Associacao associacao = associacaoRepository.findByEstabelecimentoIdAndEntregadorId(idEstabelecimento, entregador.getId())
                                    .orElseThrow(AssociacaoInvalidaException::new);
            associacao.setStatusEntregador(StatusEntregador.OCUPADO);

            notificaPedidoEmRota(pedido);
            pedidoRepository.flush();

        }

        private void notificaPedidoSemEntregador(Pedido pedido){
            EventoSemEntregador evento = new EventoSemEntregador(pedido);
            eventPublisher.publishEvent(evento);
        }

        private void notificaPedidoEmRota(Pedido pedido){
            EventoEmRota evento = new EventoEmRota(pedido);
            eventPublisher.publishEvent(evento);
        }

        public PedidoResponseDTO confirmarEntrega(Long idPedido, Long idCliente, String codigoAcesso) {
            Pedido pedido = pedidoRepository.findById(idPedido)
                    .orElseThrow(PedidoNaoExisteException::new);

            Cliente cliente = clienteRepository.findById(idCliente)
                    .orElseThrow(ClienteNaoExisteException::new);

            if (!cliente.getCodigo().equals(codigoAcesso)) {
                throw new CodigoDeAcessoInvalidoException();
            }

            if (pedido.getCliente() != cliente) {
                throw new PedidoClienteInvalidoException();
            }

            // Pedido pronto para Pedido Entregue
            pedido.getState().confirmar();
            notificaPedidoEntregue(pedido);

            Associacao associacao = associacaoRepository.findByEstabelecimentoIdAndEntregadorId(pedido.getEstabelecimento().getId(), pedido.getEntregador().getId())
                        .orElseThrow(AssociacaoInvalidaException::new);
            associacao.setStatusEntregador(StatusEntregador.ATIVO);
            novoEntregadorAtivo(associacao.getEstabelecimento().getId(), associacao.getEntregador().getId());
            
            pedidoRepository.flush();
            return new PedidoResponseDTO(pedido);
        }

        private void notificaPedidoEntregue(Pedido pedido){
            EventoEntregue evento = new EventoEntregue(pedido);
            eventPublisher.publishEvent(evento);
        }

        @Override
        public void novoEntregadorAtivo(Long idEstabelecimento, Long idEntregador){

            Estabelecimento estabelecimento = estabelecimentoRepository.findById(idEstabelecimento)
            .orElseThrow(EstabelecimentoNaoExisteException::new);

            Entregador entregador = entregadorRepository.findById(idEntregador)
            .orElseThrow(EntregadorNaoExisteException::new);

            Associacao associacao = associacaoRepository.findByEstabelecimentoIdAndEntregadorId(estabelecimento.getId(), entregador.getId())
                                    .orElseThrow(AssociacaoNaoExisteException::new);

            Sort sort = Sort.by(Sort.Order.asc("tempoEsperaEntregador"));
            List<Pedido> pedidosPendentes = pedidoRepository.findAllByStatusAndEntregadorIdNull(StatusPedido.PEDIDO_PRONTO, sort);
            if (!pedidosPendentes.isEmpty()) {
                Pedido pedidoPendente = pedidosPendentes.remove(0);
                adicionarEntregadorEspecifico(pedidoPendente.getId(), idEstabelecimento, idEntregador);
            }

            estabelecimentoRepository.flush();

        }
}
