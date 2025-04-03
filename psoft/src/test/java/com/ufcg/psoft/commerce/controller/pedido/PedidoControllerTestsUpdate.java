package com.ufcg.psoft.commerce.controller.pedido;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.ufcg.psoft.commerce.dto.cliente.EnderecoDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoResponseDTO;
import com.ufcg.psoft.commerce.dto.pizza.PizzaPostPutDTO;
import com.ufcg.psoft.commerce.exception.CustomErrorType;
import com.ufcg.psoft.commerce.model.*;
import com.ufcg.psoft.commerce.model.enums.TamanhoPizza;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;
import com.ufcg.psoft.commerce.repository.*;
import jakarta.transaction.Transactional;
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

import java.util.ArrayList;
import java.util.List;

import static org.hibernate.validator.internal.util.Contracts.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Transactional
@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("Conjunto de casos de Update do controlador de Pedidos")
public class PedidoControllerTestsUpdate {

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

    ObjectMapper objectMapper = new ObjectMapper();

    Cliente cliente;
    Estabelecimento estabelecimento;
    Endereco endereco;
    Endereco enderecoCliente;
    Pedido pedido;
    PedidoPostPutRequestDTO pedidoPostPutRequestDTO;
    SaborPizza saborPizza1;
    SaborPizza saborPizza2;
    SaborPizza saborPizza3;
    SaborPizza saborPizza4;

    @BeforeEach
    void setup() throws Exception {
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

    @Nested
    @DisplayName("Conjunto de casos de atualização de pedidos - Sucesso")
    class AtualizacaoDePedidosSucesso {

        @Test
        @DisplayName("Quando atualizamos um pedido com dados válidos")
        void quandoAtualizamosPedidoValido() throws Exception {

            List<PizzaPostPutDTO> pizzas = new ArrayList<>();

            List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();

            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);

            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId())
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isOk())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            PedidoResponseDTO resultado = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);

            // Assert
            assertNotNull(resultado.getId());
            assertEquals(cliente.getId(), resultado.getIdCliente());
            assertEquals(estabelecimento.getId(), resultado.getIdEstabelecimento());
            assertEquals(pedidoPostPutRequestDTO.getEndereco().getCep(), resultado.getEndereco().getCep());
            assertEquals(pedidoPostPutRequestDTO.getPizzas().size(), resultado.getPizzas().size());
        }
    }

    @Nested
    @DisplayName("Conjunto de casos de atualização de pedidos - Erros")
    class AtualizacaoDePedidosErros {

        @Test
        @DisplayName("Quando tentamos atualizar um pedido inexistente")
        void quandoAtualizamosPedidoInexistente() throws Exception {
            // Arrange
            Long pedidoInexistenteId = 999L; // ID que não existe

            List<PizzaPostPutDTO> pizzas = new ArrayList<>();

            List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();

            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);

            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedidoInexistenteId)
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertEquals("O pedido consultado nao existe!", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando atualizamos um pedido com cliente inexistente")
        void quandoAtualizamosPedidoComClienteInexistente() throws Exception {
            // Arrange
            Long idClienteInexistente = 99L; // ID de cliente que não existe

            List<PizzaPostPutDTO> pizzas = new ArrayList<>();

            List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();

            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);

            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId())
                            .param("idCliente", idClienteInexistente.toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertEquals("O cliente consultado nao existe!", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando atualizamos um pedido que não pertence ao cliente")
        void quandoAtualizamosPedidoComIdClienteErrado() throws Exception {
            // Arrange
            Cliente outroCliente = clienteRepository.save(Cliente.builder()
                    .nome("Outro Cliente")
                    .endereco(endereco)
                    .codigo("123456")
                    .build());

            List<PizzaPostPutDTO> pizzas = new ArrayList<>();

            List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                        .saboresId(ids)
                        .tamanho(TamanhoPizza.GRANDE)
                        .build();
        
            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);

            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId())
                            .param("idCliente", outroCliente.getId().toString())
                            .param("codigoAcesso", outroCliente.getCodigo())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertEquals("Pedido nao pertence a esse cliente.", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando atualizamos um pedido com código de acesso errado")
        void quandoAtualizamosPedidoComCodigoAcessoErrado() throws Exception {
            // Arrange
            String codigoAcessoErrado = "wrongCode";

            List<PizzaPostPutDTO> pizzas = new ArrayList<>();

            List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();

            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);

            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId())
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", codigoAcessoErrado)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertEquals("Codigo de acesso invalido!", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando atualizamos um pedido com uma pizza de sabor inexistente")
        void quandoAtualizamosPedidoComSaborInexistente() throws Exception {
            // Arrange
            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
            List<Long> ids = List.of(999L); // ID de sabor inexistente
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();

            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);

            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId())
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertEquals("O sabor consultado nao existe!", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando atualizamos um pedido com um sabor que não é do estabelecimento")
        void quandoAtualizamosPedidoComSaborDeOutroEstabelecimento() throws Exception {
            // Arrange
            Estabelecimento outroEstabelecimento = estabelecimentoRepository.save(Estabelecimento.builder()
                    .codigo("123456")
                    .build());

            SaborPizza saborPizzaOutroEstabelecimento = saborPizzaRepository.save(SaborPizza.builder()
                    .nome("Mussarela")
                    .disponivel(true)
                    .estabelecimento(outroEstabelecimento)
                    .tipo(TipoPizza.SALGADA)
                    .precoMedia(10.0)
                    .precoGrande(20.0)
                    .build());

            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
            List<Long> ids = List.of(saborPizzaOutroEstabelecimento.getId()); // ID de sabor de outro estabelecimento
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();

            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);

            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId())
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertEquals("Sabor nao encontrado nesse estabelecimento.", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando atualizamos um pedido com um sabor indisponível")
        void quandoAtualizamosPedidoComSaborIndisponivel() throws Exception {
            // Arrange
            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
            List<Long> ids = List.of(saborPizza4.getId()); // ID de sabor indisponível
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();

            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);

            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId())
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertEquals("Sabor esta indisponivel no momento.", resultado.getMessage());
        }
        
    }
}