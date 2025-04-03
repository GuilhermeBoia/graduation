package com.ufcg.psoft.commerce.controller.pedido;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.ufcg.psoft.commerce.dto.cliente.EnderecoDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoResponseDTO;
import com.ufcg.psoft.commerce.dto.pizza.PizzaPostPutDTO;
import com.ufcg.psoft.commerce.event.EventoEmRota;
import com.ufcg.psoft.commerce.event.EventoEntregue;
import com.ufcg.psoft.commerce.exception.CustomErrorType;
import com.ufcg.psoft.commerce.model.Associacao;
import com.ufcg.psoft.commerce.model.Cliente;
import com.ufcg.psoft.commerce.model.Endereco;
import com.ufcg.psoft.commerce.model.Entregador;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.Pedido;
import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.model.Veiculo;
import com.ufcg.psoft.commerce.model.enums.StatusAssociacao;
import com.ufcg.psoft.commerce.model.enums.StatusEntregador;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;
import com.ufcg.psoft.commerce.model.enums.TamanhoPizza;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;
import com.ufcg.psoft.commerce.model.enums.TipoVeiculo;
import com.ufcg.psoft.commerce.repository.AssociacaoRepository;
import com.ufcg.psoft.commerce.repository.ClienteRepository;
import com.ufcg.psoft.commerce.repository.EnderecoRepository;
import com.ufcg.psoft.commerce.repository.EntregadorRepository;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
import com.ufcg.psoft.commerce.repository.PedidoRepository;
import com.ufcg.psoft.commerce.repository.PizzaRepository;
import com.ufcg.psoft.commerce.repository.SaborPizzaRepository;
import com.ufcg.psoft.commerce.repository.VeiculoRepository;

import jakarta.transaction.Transactional;

@Transactional
@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("Conjunto de casos de Create do controlador de Pedidos")
public class PedidoControllerTestsState {
    
    final String URI_PEDIDOS = "/pedido";

    @Autowired
    MockMvc mockMvc;

    @Autowired
    PedidoRepository pedidoRepository;

    @Autowired
    ClienteRepository clienteRepository;

    @Autowired
    EstabelecimentoRepository estabelecimentoRepository;

    @Autowired
    EnderecoRepository enderecoRepository;

    @Autowired
    SaborPizzaRepository saborPizzaRepository;

    @Autowired
    PizzaRepository pizzaRepository;

    @Autowired
    EntregadorRepository entregadorRepository;

    @Autowired
    VeiculoRepository veiculoRepository;

    @Autowired
    AssociacaoRepository associacaoRepository;
    
    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final PrintStream originalOut = System.out;
    
    ObjectMapper objectMapper = new ObjectMapper();

    Cliente cliente;
    Estabelecimento estabelecimento;
    Veiculo veiculo;
    Associacao associacao;
    Entregador entregador;
    Endereco endereco;
    Endereco enderecoCliente;
    Pedido pedido;
    PedidoPostPutRequestDTO pedidoPostPutRequestDTO;
    SaborPizza saborPizza1;
    SaborPizza saborPizza2;
    SaborPizza saborPizza3;
    SaborPizza saborPizza4;
    SaborPizza saborPizza5;

    @BeforeEach
    void setup() {
        objectMapper.registerModule(new JavaTimeModule());

        endereco = enderecoRepository.save(Endereco.builder()
                .rua("Rua dos Pedidos")
                .numero("456")
                .bairro("Bairro Pedido")
                .cidade("Cidade Pedido")
                .estado("Estado Pedido")
                .cep("98765-432")
                .complemento("comp")
                .build());
        
        enderecoCliente = enderecoRepository.save(Endereco.builder()
                .rua("Rua do Cliente")
                .numero("456")
                .bairro("Bairro Cliente")
                .cidade("Cidade Cliente")
                .estado("Estado Cliente")
                .cep("58046-518")
                .complemento("comp")
                .build());

        cliente = clienteRepository.save(Cliente.builder()
                .nome("Cliente Pedido")
                .endereco(enderecoCliente)
                .codigo("654321")
                .build());

        estabelecimento = estabelecimentoRepository.save(Estabelecimento.builder()
                .codigo("654321") 
                .build());

        veiculo = veiculoRepository.save(Veiculo.builder()
                .placa("ABC1234")
                .corVeiculo("Azul")
                .tipoVeiculo(TipoVeiculo.MOTO)
                .build());

        entregador = entregadorRepository.save(Entregador.builder()
                .nome("Entregador Pedido")
                .veiculo(veiculo)
                .codigo("123456")
                .build());
        
        associacao = associacaoRepository.save(Associacao.builder()
                .estabelecimento(estabelecimento)
                .entregador(entregador)
                .status(StatusAssociacao.APROVADO)
                .statusEntregador(StatusEntregador.ATIVO)
                .tempoAtivo(LocalDateTime.of(2024, 9, 17, 9, 5))
                .build());

        pedidoPostPutRequestDTO = PedidoPostPutRequestDTO.builder()
                .endereco(EnderecoDTO.builder()
                        .rua(endereco.getRua())
                        .numero(endereco.getNumero())
                        .bairro(endereco.getBairro())
                        .cidade(endereco.getCidade())
                        .estado(endereco.getEstado())
                        .cep(endereco.getCep())
                        .complemento(endereco.getComplemento())
                        .build())
                .pizzas(new ArrayList<>())
                .build();

        saborPizza1 = saborPizzaRepository.save(SaborPizza.builder()
                .nome("Mussarela")
                .disponivel(true)
                .estabelecimento(estabelecimento)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(10.0)
                .precoGrande(20.0)
                .build());

        saborPizza2 = saborPizzaRepository.save(SaborPizza.builder()
                .nome("Carne")
                .disponivel(true)
                .estabelecimento(estabelecimento)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(20.0)
                .precoGrande(30.0)
                .build());

        saborPizza3 = saborPizzaRepository.save(SaborPizza.builder()
                .nome("Chocolate")
                .disponivel(true)
                .estabelecimento(estabelecimento)
                .tipo(TipoPizza.DOCE)
                .precoMedia(40.0)
                .precoGrande(50.0)
                .build());

        saborPizza4 = saborPizzaRepository.save(SaborPizza.builder()
                .nome("Calabresa")
                .disponivel(false)
                .estabelecimento(estabelecimento)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(15.0)
                .precoGrande(25.0)
                .build());
        
        saborPizza5 = saborPizzaRepository.save(SaborPizza.builder()
                .nome("Frango")
                .disponivel(false)
                .estabelecimento(estabelecimento)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(15.0)
                .precoGrande(25.0)
                .build());
    }

@Nested
@DisplayName("State: Pedido Recebido")
class PedidoRecebidoTests {

        @BeforeEach
        void setUpCriarPedido() throws Exception {
                pedidoRepository.deleteAll();
                List<PizzaPostPutDTO> pizzas = new ArrayList<PizzaPostPutDTO>();
                List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
                PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                        .saboresId(ids)
                        .tamanho(TamanhoPizza.GRANDE)
                        .build();

                pizzas.add(pizza1);
                pedidoPostPutRequestDTO.setPizzas(pizzas);

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                        .andExpect(status().isCreated())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();
                
                PedidoResponseDTO resultado = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);
                pedido = pedidoRepository.findById(resultado.getId()).get();
        }

        @Test
        @DisplayName("Quando um pedido recebido tenta ser pago - Sucesso")
        void quandoUmPedidoRecebidoEPago() throws Exception {

                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_RECEBIDO);

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")    
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .param("tipoPagamento", "PIX"))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

                PedidoResponseDTO response = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);

                assert(response.isPago());
                assertEquals(response.getStatus(), StatusPedido.PEDIDO_EM_PREPARO);
        }

        @Test
        @DisplayName("Quando um pedido recebido tenta ser pronto - Erro")
        void quandoUmPedidoRecebidoTentaFicarPronto() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_RECEBIDO);

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pronto")    
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .param("codigoAcesso", estabelecimento.getCodigo()))
                        .andExpect(status().isBadRequest())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertEquals("Pedido ainda nao foi pago", resultado.getMessage());
        }



        @Test
        @DisplayName("Quando um pedido recebido tenta ser confirmado - Erro")
        void quandoUmPedidoRecebidoTentaSerConfirmado() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_RECEBIDO);

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/confirmar-entrega")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo()))
                        .andExpect(status().isBadRequest())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertEquals("Pedido ainda nao foi pago", resultado.getMessage());
        }
}

@Nested
@DisplayName("State: Pedido Em Preparo")
class PedidoRecebidoTestsEmPreparo {

        @BeforeEach
        void setUpCriarPedido() throws Exception {
                pedidoRepository.deleteAll();
                List<PizzaPostPutDTO> pizzas = new ArrayList<PizzaPostPutDTO>();
                List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
                PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                        .saboresId(ids)
                        .tamanho(TamanhoPizza.GRANDE)
                        .build();

                pizzas.add(pizza1);
                pedidoPostPutRequestDTO.setPizzas(pizzas);

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                        .andExpect(status().isCreated())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();
                
                PedidoResponseDTO resultado = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);
                pedido = pedidoRepository.findById(resultado.getId()).get();

                mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("tipoPagamento", "PIX"))
                        .andExpect(status().isOk())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();
        }

        @Test
        @DisplayName("Quando um pedido pago se torna pronto - sucesso")
        void quandoUmPedidoEmPreparoFicaPronto() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_EM_PREPARO);
                assert(pedido.isPago());

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pronto")    
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .param("codigoAcesso", estabelecimento.getCodigo()))
                        .andExpect(status().isOk())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                PedidoResponseDTO response = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);

                assertEquals(response.getStatus(), StatusPedido.PEDIDO_EM_ROTA);
        }

        @Test
        @DisplayName("Quando um pedido pago tenta ser pago - Erro")
        void quandoUmPedidoEmPreparoTentaSerPago() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_EM_PREPARO);
                assert(pedido.isPago());

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("tipoPagamento", "PIX"))
                        .andExpect(status().isBadRequest())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertEquals("O pedido ja esta pago!", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando um pedido pago tenta ser confirmado - Erro")
        void quandoUmPedidoEmPreparoTentaSerConfirmado() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_EM_PREPARO);
                assert(pedido.isPago());

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/confirmar-entrega")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo()))
                        .andExpect(status().isBadRequest())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertEquals("Pedido nao esta pronto!", resultado.getMessage());
        }

}

@Nested
@DisplayName("State: Pedido Pronto")
class PedidoRecebidoTestsPronto {
        @BeforeEach
        void setUpCriarPedido() throws Exception {
                pedidoRepository.deleteAll();
                List<PizzaPostPutDTO> pizzas = new ArrayList<PizzaPostPutDTO>();
                List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
                PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                        .saboresId(ids)
                        .tamanho(TamanhoPizza.GRANDE)
                        .build();

                pizzas.add(pizza1);
                pedidoPostPutRequestDTO.setPizzas(pizzas);

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                        .andExpect(status().isCreated())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();
                
                PedidoResponseDTO resultado = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);
                pedido = pedidoRepository.findById(resultado.getId()).get();

                mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("tipoPagamento", "PIX"))
                        .andExpect(status().isOk())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pronto")    
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .param("codigoAcesso", estabelecimento.getCodigo()))
                        .andExpect(status().isOk())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();
        }


        @Test
        @DisplayName("Quando um pedido pronto tenta ser pago - Erro")
        void quandoUmPedidoProntoTentaSerPago() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_EM_ROTA);
                assert(pedido.isPago());

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("tipoPagamento", "PIX"))
                        .andExpect(status().isBadRequest())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertEquals("O pedido ja esta pago!", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando um pedido pronto tenta ser pronto - Erro")
        void quandoUmPedidoProntoTentaSerPronto() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_EM_ROTA);
                assert(pedido.isPago());

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pronto")    
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .param("codigoAcesso", estabelecimento.getCodigo()))
                        .andExpect(status().isBadRequest())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertEquals("O pedido ja esta pronto!", resultado.getMessage());
        }

}

@Nested
@DisplayName("State: Pedido Em Rota")
class PedidoStateEmRota {

        @BeforeEach
        void setUpCriarPedido() throws Exception {
                pedidoRepository.deleteAll();
                List<PizzaPostPutDTO> pizzas = new ArrayList<PizzaPostPutDTO>();
                List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
                PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                        .saboresId(ids)
                        .tamanho(TamanhoPizza.GRANDE)
                        .build();

                pizzas.add(pizza1);
                pedidoPostPutRequestDTO.setPizzas(pizzas);

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                        .andExpect(status().isCreated())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();
                
                PedidoResponseDTO resultado = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);
                pedido = pedidoRepository.findById(resultado.getId()).get();

                mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("tipoPagamento", "PIX"))
                        .andExpect(status().isOk())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pronto")    
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .param("codigoAcesso", estabelecimento.getCodigo()))
                        .andExpect(status().isOk())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/entregar")    
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .param("codigoAcesso", estabelecimento.getCodigo())
                                .param("idEntregador", entregador.getId().toString()));
        }

        @Test
        @DisplayName("Quando um pedido em rota tenta ser entregue - sucesso")
        void quandoUmPedidoEmRotaEntregue() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_EM_ROTA);
                assert(pedido.isPago());
                System.setOut(new PrintStream(outContent));


                String responseJsonString = (mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/confirmar-entrega")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo()))
                        .andExpect(status().isOk())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString());

                PedidoResponseDTO response = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);
                assertEquals(response.getStatus(), StatusPedido.PEDIDO_ENTREGUE);
        }

        @Test
        @DisplayName("Notificacao pro Estabelecimento: Quando um pedido em rota tenta ser entregue - sucesso")
        void quandoUmPedidoEmRotaEntregueNotificacao() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_EM_ROTA);
                assert(pedido.isPago());
                System.setOut(new PrintStream(outContent));


                String responseJsonString = (mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/confirmar-entrega")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo()))
                        .andExpect(status().isOk())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString());

                PedidoResponseDTO response = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);
                assertEquals(response.getStatus(), StatusPedido.PEDIDO_ENTREGUE);

                EventoEntregue evento = new EventoEntregue(pedido);
                String notificacao = "Olá, Estabelecimento " + evento.getEstabelecimento().getId() +
                    " O pedido " + evento.getPedido().getId() + " já foi Entregue para o cliente " +
                    evento.getCliente().getNome() + " pelo entregador " + evento.getEntregador().getNome();
                assertTrue(outContent.toString().trim().contains(notificacao));

        }

        @Test
        @DisplayName("Quando um pedido em rota tenta ser pago - Erro")
        void quandoUmPedidoEmRotaTentaSerPago() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_EM_ROTA);
                assert(pedido.isPago());

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("tipoPagamento", "PIX"))
                        .andExpect(status().isBadRequest())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertEquals("O pedido ja esta pago!", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando um pedido em rota tenta ser pronto - Erro")
        void quandoUmPedidoEmRotaTentaSerPronto() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_EM_ROTA);
                assert(pedido.isPago());

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pronto")    
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .param("codigoAcesso", estabelecimento.getCodigo()))
                        .andExpect(status().isBadRequest())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertEquals("O pedido ja esta pronto!", resultado.getMessage());
        }


       
}

@Nested
@DisplayName("State: Pedido Entregue")
class PedidoStateEntregue {

        @BeforeEach
        void setUpCriarPedido() throws Exception {
                pedidoRepository.deleteAll();
                List<PizzaPostPutDTO> pizzas = new ArrayList<PizzaPostPutDTO>();
                List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
                PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                        .saboresId(ids)
                        .tamanho(TamanhoPizza.GRANDE)
                        .build();

                pizzas.add(pizza1);
                pedidoPostPutRequestDTO.setPizzas(pizzas);

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                        .andExpect(status().isCreated())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();
                
                PedidoResponseDTO resultado = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);
                pedido = pedidoRepository.findById(resultado.getId()).get();

                mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("tipoPagamento", "PIX"))
                        .andExpect(status().isOk())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pronto")    
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .param("codigoAcesso", estabelecimento.getCodigo()))
                        .andExpect(status().isOk())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/entregar")    
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .param("codigoAcesso", estabelecimento.getCodigo())
                                .param("idEntregador", entregador.getId().toString()));

                mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/confirmar-entrega")
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo()));
        }

        @Test
        @DisplayName("Quando um pedido entregue tenta ser pago - Erro")
        void quandoUmPedidoEntregueTentaSerPago() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_ENTREGUE);
                assert(pedido.isPago());

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo())
                                .param("tipoPagamento", "PIX"))
                        .andExpect(status().isBadRequest())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertEquals("O pedido ja esta pago!", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando um pedido entregue tenta ser pronto - Erro")
        void quandoUmPedidoEntregueTentaSerPronto() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_ENTREGUE);
                assert(pedido.isPago());

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pronto")    
                                .param("idEstabelecimento", estabelecimento.getId().toString())
                                .param("codigoAcesso", estabelecimento.getCodigo()))
                        .andExpect(status().isBadRequest())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertEquals("O pedido ja tem entrega confirmada!", resultado.getMessage());
        }



        @Test
        @DisplayName("Quando um pedido entregue tenta ser confirmado - Erro")
        void quandoUmPedidoEntregueTentaSerConfirmado() throws Exception {
                assertEquals(pedido.getStatus(), StatusPedido.PEDIDO_ENTREGUE);
                assert(pedido.isPago());

                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/confirmar-entrega")    
                                .param("idCliente", cliente.getId().toString())
                                .param("codigoAcesso", cliente.getCodigo()))
                        .andExpect(status().isBadRequest())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertEquals("O pedido ja tem entrega confirmada!", resultado.getMessage());
        }

}

}