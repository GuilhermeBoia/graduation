package com.ufcg.psoft.commerce.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.ufcg.psoft.commerce.dto.InteresseSaborPizza.InteresseSaborPizzaResponseDTO;
import com.ufcg.psoft.commerce.exception.CustomErrorType;
import com.ufcg.psoft.commerce.model.*;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;
import com.ufcg.psoft.commerce.repository.ClienteRepository;
import com.ufcg.psoft.commerce.repository.EnderecoRepository;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
import com.ufcg.psoft.commerce.repository.InteresseSaborPizzaRepository;
import com.ufcg.psoft.commerce.repository.SaborPizzaRepository;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("Testes do controlador de Interesse")
public class InteresseSaborPizzaControllerTests {
    final String URI_INTERESSE = "/interesse";

    @Autowired
    MockMvc mockMvc;

    @Autowired
    ClienteRepository clienteRepository;

    @Autowired
    EnderecoRepository enderecoRepository;

    @Autowired
    InteresseSaborPizzaRepository interesseSaborPizzaRepository;

    @Autowired
    SaborPizzaRepository   saborPizzaRepository;

    @Autowired
    EstabelecimentoRepository estabelecimentoRepository;



    ObjectMapper objectMapper = new ObjectMapper();

    Cliente cliente;
    Endereco endereco;
    SaborPizza saborPizza;
    Estabelecimento estabelecimento;


    @BeforeEach
    void setUp() {
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
                
                estabelecimento = estabelecimentoRepository.save(Estabelecimento.builder()
                .codigo("654321") 
                .build());

                saborPizza = saborPizzaRepository.save(SaborPizza.builder()
                .nome("Mussarela")
                .disponivel(false)
                .estabelecimento(estabelecimento)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(10.0)
                .precoGrande(20.0)
                .build());
        cliente = clienteRepository.save(Cliente.builder()
                .nome("Cliente Pedido")
                .endereco(endereco)
                .codigo("654321")
                .build());
    }

    @AfterEach
    void tearDown() {
        interesseSaborPizzaRepository.deleteAll();
    }


        @Test
        @DisplayName("Quando o cliente nao existe")
        void ClienteValido () throws Exception {

            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_INTERESSE + "/registrar")
                            .param("clienteId", String.valueOf(9999))
                            .param("saborPizzaId", String.valueOf(saborPizza.getId()))
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(new InteresseSaborPizzaResponseDTO())))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
            assertAll(
                    () -> assertEquals("O cliente consultado nao existe!", resultado.getMessage()));

        }
    
        @Test
        @DisplayName("Quando o sabor da pizza nao existe")
        void SaborPizzaValido () throws Exception {

            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_INTERESSE + "/registrar")
                            .param("clienteId", String.valueOf(cliente.getId()))
                            .param("saborPizzaId", String.valueOf(9999))
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(new InteresseSaborPizzaResponseDTO())))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
            assertAll(
                    () -> assertEquals("O sabor consultado nao existe!", resultado.getMessage()));

        }

        @Test
        @DisplayName("Quando o cliente e o sabor da pizza existem e o interesse é registrado com sucesso")
        void registrarInteresseComSucesso() throws Exception {
                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_INTERESSE + "/registrar")
                        .param("clienteId", String.valueOf(cliente.getId()))
                        .param("saborPizzaId", String.valueOf(saborPizza.getId()))
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(new InteresseSaborPizzaResponseDTO())))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

                assertEquals("Interesse registrado com sucesso!", responseJsonString);
}




        @Test
        @DisplayName("Quando o sabor da pizza é inválido ao notificar clientes")
        void notificarClientesSaborPizzaInvalido() throws Exception {
                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_INTERESSE + "/notificar")
                        .param("saborPizzaId", String.valueOf(5L))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
                assertAll(
                        () -> assertEquals("O sabor consultado nao existe!", resultado.getMessage()));
                }
        @Test
        @DisplayName("Quando não há interesse registrado e um cliente é notificado")
        void notificarClientesSemInteresse() throws Exception {
    
        interesseSaborPizzaRepository.deleteAll();

        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_INTERESSE + "/notificar")
                    .param("saborPizzaId", String.valueOf(saborPizza.getId()))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andDo(print())
            .andReturn().getResponse().getContentAsString();

        assertEquals("Nenhum cliente com interesse neste sabor.", responseJsonString);
        }
        @Test
        @DisplayName("Quando o sabor é inválido ao notificar clientes")
        void notificarClientesClienteInvalido() throws Exception {
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_INTERESSE + "/notificar")
                            .param("saborPizzaId", String.valueOf(saborPizza.getId()))
                            .contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();
        
            assertEquals("Nenhum cliente com interesse neste sabor.", responseJsonString);
        }
}

