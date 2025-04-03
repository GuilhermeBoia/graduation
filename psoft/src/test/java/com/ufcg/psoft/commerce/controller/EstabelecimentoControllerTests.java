package com.ufcg.psoft.commerce.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.ufcg.psoft.commerce.dto.estabelecimento.EstabelecimentoPostPutDTO;
import com.ufcg.psoft.commerce.exception.CustomErrorType;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("Testes do controlador de Estabelecimento")
public class EstabelecimentoControllerTests {

    final String URI_ESTABELECIMENTO = "/estabelecimento";

    @Autowired
    MockMvc mockMvc;

    @Autowired
    EstabelecimentoRepository estabelecimentoRepository;

    ObjectMapper objectMapper = new ObjectMapper();
    Estabelecimento estabelecimento;
    EstabelecimentoPostPutDTO estabelecimentoPostPutDTO;

    @BeforeEach
    void setup() {
        
        objectMapper.registerModule(new JavaTimeModule());

        
        
        estabelecimento = Estabelecimento.builder()
                .codigo("123456") 
                .build();
        estabelecimento = estabelecimentoRepository.save(estabelecimento);

        estabelecimentoPostPutDTO = EstabelecimentoPostPutDTO.builder()
                .codigoAcesso(estabelecimento.getCodigo()) 
                .build();
    }
@Nested
@DisplayName("Conjunto de casos de verificação do código de acesso")
class EstabelecimentoVerificacaoCodigoAcesso {

        @Test
        @DisplayName("Quando alteramos o código de acesso para nulo")
        void quandoAlteramosCodigoAcessoParaNulo() throws Exception {
            // Arrange
            estabelecimentoPostPutDTO.setCodigoAcesso(null);

            // Act
            String responseJsonString = mockMvc.perform(put(URI_ESTABELECIMENTO + "/" + estabelecimento.getId())
            .contentType(MediaType.APPLICATION_JSON)
            .param("codigoAcesso", estabelecimento.getCodigo())
            .content(objectMapper.writeValueAsString(estabelecimentoPostPutDTO)))
            .andExpect(status().isBadRequest())
            .andDo(print())
            .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

// Assert
            assertAll(
                () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
                () -> assertEquals("Codigo de acesso obrigatorio", resultado.getErrors().get(0))
            );
        }

        @Test
        @DisplayName("Quando alteramos o código de acesso para mais de 6 dígitos")
        void quandoAlteramosCodigoAcessoParaMaisDe6Digitos() throws Exception {
            // Arrange
            estabelecimentoPostPutDTO.setCodigoAcesso("1234567");

            // Act
            String responseJsonString = mockMvc.perform(put(URI_ESTABELECIMENTO + "/" + estabelecimento.getId())
            .contentType(MediaType.APPLICATION_JSON)
            .param("codigoAcesso", estabelecimento.getCodigo())
            .content(objectMapper.writeValueAsString(estabelecimentoPostPutDTO)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertAll(
                () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
                () -> assertEquals("Codigo de acesso deve ter exatamente 6 digitos numericos", resultado.getErrors().get(0))
            );
        }

        @Test
        @DisplayName("Quando alteramos o código de acesso para menos de 6 dígitos")
        void quandoAlteramosCodigoAcessoParaMenosDe6Digitos() throws Exception {
            // Arrange
            estabelecimentoPostPutDTO.setCodigoAcesso("12345");

            // Act
            String responseJsonString = mockMvc.perform(put(URI_ESTABELECIMENTO + "/" + estabelecimento.getId())
            .contentType(MediaType.APPLICATION_JSON)
            .param("codigoAcesso", estabelecimento.getCodigo())
            .content(objectMapper.writeValueAsString(estabelecimento)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertAll(
                () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
                () -> assertEquals("Codigo de acesso obrigatorio", resultado.getErrors().get(0))
            );
        }

        @Test
        @DisplayName("Quando alteramos o código de acesso para caracteres não numéricos")
        void quandoAlteramosCodigoAcessoParaCaracteresNaoNumericos() throws Exception {
            // Arrange
            estabelecimentoPostPutDTO.setCodigoAcesso("a*c4e@");

            // Act
            String responseJsonString = mockMvc.perform(put(URI_ESTABELECIMENTO + "/" + estabelecimento.getId())
            .contentType(MediaType.APPLICATION_JSON)
            .param("codigoAcesso", estabelecimento.getCodigo())
            .content(objectMapper.writeValueAsString(estabelecimentoPostPutDTO)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertAll(
                () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
                () -> assertEquals("Codigo de acesso deve ter exatamente 6 digitos numericos", resultado.getErrors().get(0))
            );
        }

        @Test
        @DisplayName("Quando criamos um novo cliente com dados válidos")
        void quandoCriarEstabelecimentoValido() throws Exception {
            // Arrange
            EstabelecimentoPostPutDTO novoEstabelecimentoPostPutDTO = EstabelecimentoPostPutDTO.builder()
                    .codigoAcesso("654321")
                    .build();
                    String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_ESTABELECIMENTO)
                    .contentType(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(novoEstabelecimentoPostPutDTO)))
                        .andExpect(status().isCreated()) // Código 201
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                Estabelecimento resultado = objectMapper.readValue(responseJsonString, Estabelecimento.class);

    

            assertNotNull(novoEstabelecimentoPostPutDTO.getCodigoAcesso());
    }
  }
}