package com.ufcg.psoft.commerce.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.ufcg.psoft.commerce.dto.associacao.AssociacaoResponseDTO;
import com.ufcg.psoft.commerce.dto.entregador.EntregadorPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.entregador.EntregadorResponseDTO;
import com.ufcg.psoft.commerce.dto.entregador.VeiculoDTO;
import com.ufcg.psoft.commerce.exception.CustomErrorType;
import com.ufcg.psoft.commerce.model.Associacao;
import com.ufcg.psoft.commerce.model.Entregador;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.Veiculo;
import com.ufcg.psoft.commerce.model.enums.StatusAssociacao;
import com.ufcg.psoft.commerce.model.enums.StatusEntregador;
import com.ufcg.psoft.commerce.model.enums.TipoVeiculo;
import com.ufcg.psoft.commerce.repository.AssociacaoRepository;
import com.ufcg.psoft.commerce.repository.EntregadorRepository;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
import com.ufcg.psoft.commerce.repository.VeiculoRepository;
import com.ufcg.psoft.commerce.service.associacao.AssociacaoService;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.LocalDateTime;

@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("Testes do controlador de Entregador")
public class EntregadorControllerTests {

    final String URI_ENTREGADOR = "/entregador";
    final String URI_VEICULO = "/veiculo";

    @Autowired
    MockMvc mockMvc;

    @MockBean
    AssociacaoService associacaoService;

    @Autowired
    AssociacaoRepository associacaoRepository;

    @Autowired
    EntregadorRepository entregadorRepository;

    @Autowired
    EstabelecimentoRepository estabelecimentoRepository;

    @Autowired
    VeiculoRepository veiculoRepository;


    ObjectMapper objectMapper = new ObjectMapper();

    Entregador entregador;
    EntregadorPostPutRequestDTO entregadorPostPutRequestDTO;

    @BeforeEach
    void setup() {
        objectMapper.registerModule(new JavaTimeModule());

        entregadorRepository.deleteAll();
        veiculoRepository.deleteAll();

        Veiculo veiculo = Veiculo.builder()
                .placa("abc321")
                .tipoVeiculo(TipoVeiculo.CARRO)
                .corVeiculo("Preto")
                .build();
        veiculo = veiculoRepository.save(veiculo);

        entregador = entregadorRepository.save(Entregador.builder()
                .nome("Denner")
                .veiculo(veiculo)
                .codigo("123456")
                .build());


        entregadorPostPutRequestDTO = EntregadorPostPutRequestDTO.builder()
                .nome(entregador.getNome())
                .veiculo(VeiculoDTO.builder()
                        .placa(veiculo.getPlaca())
                        .tipoVeiculo(veiculo.getTipoVeiculo())
                        .corVeiculo(veiculo.getCorVeiculo())
                        .build())
                .codigoAcesso(entregador.getCodigo())
                .build();
    }

        @AfterEach
        void tearDown() {
                entregadorRepository.deleteAll();
                veiculoRepository.deleteAll();
        }

    @Nested
    @DisplayName("Conjunto de casos de verificação de nome")
    class EntregadorVerificacaoNome {

        @Test
        @DisplayName("Quando alteramos o nome do entregador com dados válidos")
        void quandoAlteramosNomeDoEntregadorValido() throws Exception {
            entregadorPostPutRequestDTO.setNome("Denner Alterado");

            String responseJsonString = mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", entregador.getCodigo())
                            .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
                    .andExpect(status().isOk())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            Entregador resultado = objectMapper.readValue(responseJsonString, Entregador.class);

            assertEquals("Denner Alterado", resultado.getNome());
        }

        @Test
        @DisplayName("Quando alteramos o nome do entregador para nulo")
        void quandoAlteramosNomeDoEntregadorNulo() throws Exception {
            entregadorPostPutRequestDTO.setNome(null);

            String responseJsonString = mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", entregador.getCodigo())
                            .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            assertAll(
                    () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
                    () -> assertEquals("Nome obrigatorio", resultado.getErrors().get(0))
            );
        }

        @Test
        @DisplayName("Quando alteramos o nome do entregador para vazio")
        void quandoAlteramosNomeDoEntregadorVazio() throws Exception {
            entregadorPostPutRequestDTO.setNome("");

            String responseJsonString = mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", entregador.getCodigo())
                            .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            assertAll(
                    () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
                    () -> assertEquals("Nome obrigatorio", resultado.getErrors().get(0))
            );
        }
    }



    @Nested
    @DisplayName("Conjunto de casos de verificação do veículo")
    class EntregadorVerificacaoVeiculo {

        @Test
        @DisplayName("Quando alteramos o veículo do entregador com dados válidos")
        void quandoAlteramosVeiculoDoEntregadorValido() throws Exception {
            entregadorPostPutRequestDTO.setVeiculo(VeiculoDTO.builder()
                    .placa("def456")
                    .tipoVeiculo(TipoVeiculo.MOTO)
                    .corVeiculo("Azul")
                    .build());

            String responseJsonString = mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", entregador.getCodigo())
                            .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
                    .andExpect(status().isOk())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            Entregador resultado = objectMapper.readValue(responseJsonString, Entregador.class);
            assertEquals("def456", resultado.getVeiculo().getPlaca());
            assertEquals(TipoVeiculo.MOTO, resultado.getVeiculo().getTipoVeiculo());
            assertEquals("Azul", resultado.getVeiculo().getCorVeiculo());
        }
        @Test
        @DisplayName("Quando alteramos o veículo do entregador para uma placa em branco")
        void quandoAlteramosVeiculoDoEntregadorPlacaBranco() throws Exception {
            
            entregadorPostPutRequestDTO.setVeiculo(VeiculoDTO.builder()
                    .placa("")
                    .tipoVeiculo(TipoVeiculo.CARRO)
                    .corVeiculo("Branco")
                    .build());
        
            
            String responseJsonString = mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", entregador.getCodigo())
                            .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();
        
            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
            assertEquals("Erros de validacao encontrados", resultado.getMessage());
            assertEquals("Placa obrigatoria" , resultado.getErrors().get(0));
        }
        
        
        @Test
        @DisplayName("Quando alteramos o veículo do entregador para uma cor de veículo em branco")
        void quandoAlteramosVeiculoDoEntregadorCorNula() throws Exception {
            
            entregadorPostPutRequestDTO.setVeiculo(VeiculoDTO.builder()
                    .placa("jkl012")
                    .tipoVeiculo(TipoVeiculo.MOTO)
                    .corVeiculo("")
                    .build());
        
            // Act & Assert
            String responseJsonString = mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", entregador.getCodigo())
                            .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();
        

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
            assertEquals("Erros de validacao encontrados", resultado.getMessage());
            assertEquals("cor do veiculo obrigatoria" , resultado.getErrors().get(0));
        }
      

    @Test
    @DisplayName("Quando alteramos o veículo do entregador para nulo")
    void quandoAlteramosVeiculoDoEntregadorNulo() throws Exception {
        
        entregadorPostPutRequestDTO.setVeiculo(null);

       
        String responseJsonString = mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                        .contentType(MediaType.APPLICATION_JSON)
                        .param("codigoAcesso", entregador.getCodigo())
                        .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
        assertEquals("Erros de validacao encontrados", resultado.getMessage());
        assertEquals("Veiculo obrigatorio" , resultado.getErrors().get(0));
}
    }



@Nested
@DisplayName("Conjunto de casos de verificação do código de acesso")
class EntregadorVerificacaoCodigoAcesso {

    @Test
    @DisplayName("Quando o código de acesso é inválido")
    void quandoCodigoAcessoInvalido() throws Exception {
    mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("codigoAcesso", "codigoIncorreto")
                    .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
            .andDo(print());
}


    @Test
    @DisplayName("Quando o código de acesso é nulo")
    void quandoCodigoAcessoNulo() throws Exception {
        
        mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                        .contentType(MediaType.APPLICATION_JSON)
                        .param("codigoAcesso", "")
                        .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
                .andExpect(status().isBadRequest())
                .andDo(print());
    }
    @Test
@DisplayName("Quando alteramos o código de acesso do entregador com mais de 6 dígitos")
void quandoAlteramosCodigoAcessoDoEntregadorMaisDe6Digitos() throws Exception {
    
    entregadorPostPutRequestDTO.setCodigoAcesso("1234567");

    
    String responseJsonString = mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("codigoAcesso", entregador.getCodigo())
                    .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
            .andExpect(status().isBadRequest())
            .andDo(print())
            .andReturn().getResponse().getContentAsString();

    CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

    
    assertAll(
            () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
            () -> assertEquals("Codigo de acesso deve ter exatamente 6 digitos numericos", resultado.getErrors().get(0))
    );
}

@Test
@DisplayName("Quando alteramos o código de acesso do entregador com menos de 6 dígitos")
void quandoAlteramosCodigoAcessoDoEntregadorMenosDe6Digitos() throws Exception {
    
    entregadorPostPutRequestDTO.setCodigoAcesso("12345");

    
    String responseJsonString = mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("codigoAcesso", entregador.getCodigo())
                    .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
            .andExpect(status().isBadRequest())
            .andDo(print())
            .andReturn().getResponse().getContentAsString();

    CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

    
    assertAll(
            () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
            () -> assertEquals("Codigo de acesso deve ter exatamente 6 digitos numericos", resultado.getErrors().get(0))
    );
}

@Test
@DisplayName("Quando alteramos o código de acesso do entregador com caracteres não numéricos")
void quandoAlteramosCodigoAcessoDoEntregadorComCaracteresNaoNumericos() throws Exception {
    // Arrange
    entregadorPostPutRequestDTO.setCodigoAcesso("a*c4e@");

    
    String responseJsonString = mockMvc.perform(put(URI_ENTREGADOR + "/" + entregador.getId())
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("codigoAcesso", entregador.getCodigo())
                    .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
            .andExpect(status().isBadRequest())
            .andDo(print())
            .andReturn().getResponse().getContentAsString();

    CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

    
    assertAll(
            () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
            () -> assertEquals("Codigo de acesso deve ter exatamente 6 digitos numericos", resultado.getErrors().get(0))
    );
}
}
@Nested
@DisplayName("Conjunto de casos de verificação dos fluxos básicos API Rest")
class EntregadorVerificacaoFluxosBasicosApiRest {

    @Test
    @DisplayName("Quando buscamos um entregador salvo pelo id")
    void quandoBuscamosPorUmEntregadorSalvo() throws Exception {
        

        
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_ENTREGADOR + "/" + entregador.getId())
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()) 
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        EntregadorResponseDTO resultado = objectMapper.readValue(responseJsonString, EntregadorResponseDTO.class);

        
        assertAll(
                () -> assertEquals(entregador.getId().longValue(), resultado.getId().longValue()),
                () -> assertEquals(entregador.getNome(), resultado.getNome())
        );
    }

    @Test
    @DisplayName("Quando buscamos um entregador inexistente")
    void quandoBuscamosPorUmEntregadorInexistente() throws Exception {
        

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_ENTREGADOR + "/" + 999999999)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest()) // Código 400
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        
        assertAll(
                () -> assertEquals("O entregador consultado nao existe!", resultado.getMessage())
        );
    }

    @Test
    @DisplayName("Quando criamos um novo entregador com dados válidos")
    void quandoCriarEntregadorValido() throws Exception {
        
        EntregadorPostPutRequestDTO novoEntregadorDTO = EntregadorPostPutRequestDTO.builder()
                .nome("Novo Entregador")
                .veiculo(VeiculoDTO.builder()
                        .placa("ghi789")
                        .tipoVeiculo(TipoVeiculo.CARRO)
                        .corVeiculo("Cinza")
                        .build())
                .codigoAcesso("654321")
                .build();

        
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_ENTREGADOR)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(novoEntregadorDTO)))
                .andExpect(status().isCreated()) // Código 201
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        Entregador resultado = objectMapper.readValue(responseJsonString, Entregador.class);

        
        assertAll(
                () -> assertNotNull(resultado.getId()),
                () -> assertEquals(novoEntregadorDTO.getNome(), resultado.getNome())
        );
    }

    @Test
    @DisplayName("Quando alteramos o entregador com dados válidos")
    void quandoAlteramosEntregadorValido() throws Exception {
        
        Long entregadorId = entregador.getId();
        entregadorPostPutRequestDTO.setNome("Denner Atualizado");

        
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_ENTREGADOR + "/" + entregadorId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .param("codigoAcesso", entregador.getCodigo())
                        .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
                .andExpect(status().isOk()) // Código 200
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        Entregador resultado = objectMapper.readValue(responseJsonString, Entregador.class);

        
        assertAll(
                () -> assertEquals(entregadorId, resultado.getId().longValue()),
                () -> assertEquals(entregadorPostPutRequestDTO.getNome(), resultado.getNome())
        );
    }

    @Test
    @DisplayName("Quando alteramos o entregador inexistente")
    void quandoAlteramosEntregadorInexistente() throws Exception {
        
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_ENTREGADOR + "/" + 99999L)
                        .contentType(MediaType.APPLICATION_JSON)
                        .param("codigoAcesso", entregador.getCodigo())
                        .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
                .andExpect(status().isBadRequest()) // Código 400
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        
        assertAll(
                () -> assertEquals("O entregador consultado nao existe!", resultado.getMessage())
        );
    }

    @Test
    @DisplayName("Quando alteramos o entregador passando código de acesso inválido")
    void quandoAlteramosEntregadorCodigoAcessoInvalido() throws Exception {
        
        Long entregadorId = entregador.getId();

        
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_ENTREGADOR + "/" + entregadorId)
                        .contentType(MediaType.APPLICATION_JSON)
                        .param("codigoAcesso", "invalido")
                        .content(objectMapper.writeValueAsString(entregadorPostPutRequestDTO)))
                .andExpect(status().isBadRequest()) 
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        
        assertAll(
                () -> assertEquals("Codigo de acesso invalido!", resultado.getMessage())
        );
    }

    @Test
    @DisplayName("Quando excluímos um entregador salvo")
    void quandoExcluimosEntregadorValido() throws Exception {
        
        mockMvc.perform(MockMvcRequestBuilders.delete(URI_ENTREGADOR + "/" + entregador.getId())
                        .contentType(MediaType.APPLICATION_JSON)
                        .param("codigoAcesso", entregador.getCodigo()))
                .andExpect(status().isNoContent()) 
                .andDo(print());
    }

    @Test
    @DisplayName("Quando excluímos um entregador inexistente")
    void quandoExcluimosEntregadorInexistente() throws Exception {
        
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.delete(URI_ENTREGADOR + "/" + 999999)
                        .contentType(MediaType.APPLICATION_JSON)
                        .param("codigoAcesso", entregador.getCodigo()))
                .andExpect(status().isBadRequest()) 
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        
        assertAll(
                () -> assertEquals("O entregador consultado nao existe!", resultado.getMessage())
        );
    }
}

@Nested
@DisplayName("Conjunto de casos de analise de associacao")
class EntregadorAnalisaApiRest {

        @Test
        @DisplayName("Quando analisamos uma associacao com dados válidos")
        void quandoAnalisamosAssocicao() throws Exception {

                Estabelecimento estabelecimento = estabelecimentoRepository.save(Estabelecimento.builder()
                        .codigo("654321") 
                         .build());
                
                Associacao associacao = associacaoRepository.save(Associacao.builder()
                        .estabelecimento(estabelecimento)
                        .entregador(entregador)
                        .status(StatusAssociacao.APROVADO)
                        .statusEntregador(StatusEntregador.ATIVO)
                        .tempoAtivo(LocalDateTime.of(2024, 9, 17, 9, 5))
                        .build());


                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_ENTREGADOR + "/" + entregador.getId() + "/associar/" + associacao.getId() + "/status")
        }
}
}