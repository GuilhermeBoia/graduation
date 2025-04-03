package com.ufcg.psoft.commerce.controller.pedido;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.ufcg.psoft.commerce.dto.cliente.EnderecoDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoResponseDTO;
import com.ufcg.psoft.commerce.dto.pizza.PizzaPostPutDTO;
import com.ufcg.psoft.commerce.exception.CustomErrorType;
import com.ufcg.psoft.commerce.model.Cliente;
import com.ufcg.psoft.commerce.model.Endereco;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.Pedido;
import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.model.enums.TamanhoPizza;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;
import com.ufcg.psoft.commerce.repository.ClienteRepository;
import com.ufcg.psoft.commerce.repository.EnderecoRepository;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
import com.ufcg.psoft.commerce.repository.PedidoRepository;
import com.ufcg.psoft.commerce.repository.PizzaRepository;
import com.ufcg.psoft.commerce.repository.SaborPizzaRepository;

import jakarta.transaction.Transactional;

@Transactional
@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("Conjunto de casos de Create do controlador de Pedidos")
public class PedidoControllerTestsCreate {
    
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

        pedido = pedidoRepository.save(Pedido.builder()
                .cliente(cliente)
                .estabelecimento(estabelecimento)
                .endereco(endereco)
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
@DisplayName("Conjunto de casos de criação de pedidos - Sucesso")
class CriacaoDePedidosSucesso {

        @Test
        @DisplayName("Quando criamos um pedido com dados válidos")
        void quandoCriamosPedidoValido() throws Exception {
        
                List<PizzaPostPutDTO> pizzas = new ArrayList<PizzaPostPutDTO>();

                List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
                PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                        .saboresId(ids)
                        .tamanho(TamanhoPizza.GRANDE)
                        .build();

                pizzas.add(pizza1);

                pedidoPostPutRequestDTO.setPizzas(pizzas);

                // Act
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

                // Assert
                assertNotNull(resultado.getId());
                assertEquals(cliente.getId(), resultado.getIdCliente());
                assertEquals(estabelecimento.getId(), resultado.getIdEstabelecimento());
                assertEquals(pedidoPostPutRequestDTO.getEndereco().getCep(), resultado.getEndereco().getCep());
                assertEquals(pedidoPostPutRequestDTO.getPizzas().size(), resultado.getPizzas().size());
        }

        @Test
        @DisplayName("Quando criamos um pedido com mais de uma pizza")
        void quandoCriamosPedidoComMaisDeUmaPizza() throws Exception {
                // Arrange
                List<PizzaPostPutDTO> pizzas = new ArrayList<>();

                List<Long> ids1 = List.of(saborPizza1.getId());
                PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                        .saboresId(ids1)
                        .tamanho(TamanhoPizza.GRANDE)
                        .build();

                List<Long> ids2 = List.of(saborPizza2.getId());
                PizzaPostPutDTO pizza2 = PizzaPostPutDTO.builder()
                        .saboresId(ids2)
                        .tamanho(TamanhoPizza.GRANDE)
                        .build();

                pizzas.add(pizza1);
                pizzas.add(pizza2);
                pedidoPostPutRequestDTO.setPizzas(pizzas);

                // Act
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

                // Assert
                assertNotNull(resultado.getId());
                assertEquals(2, resultado.getPizzas().size());
        }
    
        @Test
        @DisplayName("Quando criamos uma pizza grande com apenas um sabor")
        void quandoCriamosPizzaGrandeComUmSabor() throws Exception {
            // Arrange
            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
    
            List<Long> ids = List.of(saborPizza1.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();
    
            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);
    
            // Act
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
    
            // Assert
            assertNotNull(resultado.getId());
        }

        @Test
        @DisplayName("Quando criamos um pedido sem endereço e utilizamos o endereço do cliente")
        void quandoCriamosPedidoSemEnderecoUtilizandoEnderecoDoCliente() throws Exception {
            // Arrange
            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
    
            List<Long> ids = List.of(saborPizza1.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();
    
            pizzas.add(pizza1);
    
            // Criar o DTO sem o endereço
            pedidoPostPutRequestDTO = PedidoPostPutRequestDTO.builder()
                    .pizzas(pizzas)
                    .build();
    
            // Act
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
    
            // Assert
            assertNotNull(resultado.getId());
            assertEquals(cliente.getId(), resultado.getIdCliente());
            assertEquals(estabelecimento.getId(), resultado.getIdEstabelecimento());
            assertEquals(pedidoPostPutRequestDTO.getPizzas().size(), resultado.getPizzas().size());
            assertEquals(cliente.getEndereco().getCep(), resultado.getEndereco().getCep());
        }
}

@Nested
@DisplayName("Conjunto de casos de criação de pedidos - Erros")
class CriacaoDePedidosErros {

    @Test
        @DisplayName("Quando criamos uma pizza média com mais de um sabor")
        void quandoCriamosPizzaMediaMaisDeUmSabor() throws Exception {
            // Arrange
            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
    
            List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId());
            PizzaPostPutDTO pizzasDTO = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.MEDIA)
                    .build();
    
            pizzas.add(pizzasDTO);
            pedidoPostPutRequestDTO.setPizzas(pizzas);
    
            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .param("idEstabelecimento", estabelecimento.getId().toString())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();
    
            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
    
            // Assert
            assertEquals("Quantidade de sabores invalida.", resultado.getMessage());
        }
    
        @Test
        @DisplayName("Quando criamos uma pizza grande com mais de dois sabores")
        void quandoCriamosPizzaGrandeMaisDeDoisSabores() throws Exception {
            // Arrange
            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
    
            List<Long> ids = List.of(saborPizza1.getId(), saborPizza2.getId(), saborPizza3.getId());
            PizzaPostPutDTO pizzaDTO = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();
    
            pizzas.add(pizzaDTO);
            pedidoPostPutRequestDTO.setPizzas(pizzas);
    
            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .param("idEstabelecimento", estabelecimento.getId().toString())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();
    
            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
    
            // Assert
            assertEquals("Quantidade de sabores invalida.", resultado.getMessage());
        }
    
        @Test
        @DisplayName("Quando criamos uma pizza com sabor que não pertence ao estabelecimento")
        void quandoCriamosPizzaComSaborForaDoEstabelecimento() throws Exception {
            // Arrange
            Estabelecimento outroEstabelecimento = estabelecimentoRepository.save(Estabelecimento.builder()
                    .codigo("123456")
                    .build());
    
            SaborPizza saborFora = saborPizzaRepository.save(SaborPizza.builder()
                    .nome("Calabresa")
                    .disponivel(true)
                    .estabelecimento(outroEstabelecimento)
                    .tipo(TipoPizza.SALGADA)
                    .precoMedia(15.0)
                    .precoGrande(25.0)
                    .build());
    
            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
            List<Long> ids = List.of(saborFora.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();
    
            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);
    
            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .param("idEstabelecimento", estabelecimento.getId().toString())
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
        @DisplayName("Quando criamos um pedido com código de acesso errado")
        void quandoCriamosPedidoComCodigoAcessoErrado() throws Exception {
            // Arrange
            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
    
            List<Long> ids = List.of(saborPizza1.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();
    
            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);
    
            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", "wrongCode")
                            .param("idEstabelecimento", estabelecimento.getId().toString())
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
        @DisplayName("Quando criamos uma pizza com um sabor que não existe")
        void quandoCriamosPizzaComSaborInexistente() throws Exception {
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
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .param("idEstabelecimento", estabelecimento.getId().toString())
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
        @DisplayName("Quando criamos uma pizza com sabor indisponível")
        void quandoCriamosPizzaComSaborIndisponivel() throws Exception {
            // Arrange
            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
    
            List<Long> ids = List.of(saborPizza4.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();
    
            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);
    
            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .param("idEstabelecimento", estabelecimento.getId().toString())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();
    
            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
    
            // Assert
            assertEquals("Sabor esta indisponivel no momento.", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando criamos uma pizza com mais de um sabor indisponível")
        void quandoCriamosPizzaComMaisDeUmSaborIndisponivel() throws Exception {
            // Arrange
            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
    
            List<Long> ids = List.of(saborPizza4.getId(), saborPizza5.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();
    
            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);
    
            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .param("idEstabelecimento", estabelecimento.getId().toString())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(pedidoPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();
    
            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
    
            // Assert
            assertEquals("Sabor esta indisponivel no momento.", resultado.getMessage());
        }

        @Test
        @DisplayName("Quando criamos um pedido com um sabor disponível e outro indisponível")
        void quandoCriamosPedidoComSaborDisponivelEIndisponivel() throws Exception {
            // Arrange
            List<PizzaPostPutDTO> pizzas = new ArrayList<>();
    
            List<Long> ids = List.of(saborPizza1.getId(), saborPizza4.getId());
            PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                    .saboresId(ids)
                    .tamanho(TamanhoPizza.GRANDE)
                    .build();
    
            pizzas.add(pizza1);
            pedidoPostPutRequestDTO.setPizzas(pizzas);
    
            // Act
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_PEDIDOS)
                            .param("idCliente", cliente.getId().toString())
                            .param("codigoAcesso", cliente.getCodigo())
                            .param("idEstabelecimento", estabelecimento.getId().toString())
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