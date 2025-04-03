package com.ufcg.psoft.commerce.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
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
import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaResponseDTO;
import com.ufcg.psoft.commerce.exception.CustomErrorType;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
import com.ufcg.psoft.commerce.repository.SaborPizzaRepository;

import jakarta.transaction.Transactional;
import java.util.ArrayList;

@Transactional
@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("Testes do controlador de SaborPizza")
public class SaborPizzaControllerTests {
    final String URI_SABOR = "/sabores";

    @Autowired
    MockMvc mockMvc;

    @Autowired
    SaborPizzaRepository saborPizzaRepository;

    @Autowired
    EstabelecimentoRepository   estabelecimentoRepository;

    ObjectMapper objectMapper = new ObjectMapper();
    SaborPizza saborPizza;
    Estabelecimento estabelecimento;
    
    @BeforeEach
    void setup() {
        objectMapper.registerModule(new JavaTimeModule());

        estabelecimento = estabelecimentoRepository.save(Estabelecimento.builder()
        .sabores(new ArrayList<>())
        .codigo("654321") 
        .build());

        saborPizza = saborPizzaRepository.save(SaborPizza.builder()
                .nome("Mussarela")
                .disponivel(true)
                .estabelecimento(estabelecimento)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(10.0)
                .precoGrande(20.0)
                .build());
    }

@Nested
@DisplayName("Conjunto de casos de criação de sabor de pizza")
class CriacaoDePedidos {
    @Test
    @DisplayName("Quando criamos um sabor de pizza com dados válidos")
    void criarSaborPizzaComDadosValidos() throws Exception {
        SaborPizzaPostPutRequestDTO dto = SaborPizzaPostPutRequestDTO.builder()
                .nome(saborPizza.getNome())
                .disponivel(saborPizza.getDisponivel())
                .tipo(saborPizza.getTipo())
                .precoMedia(saborPizza.getPrecoMedia())
                .precoGrande(saborPizza.getPrecoGrande())
                .build();
    
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_SABOR + "/{idEstabelecimento}/sabores", estabelecimento.getId())
                        .param("codigoAcesso", "654321")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isCreated())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        
        SaborPizzaResponseDTO responseDTO = objectMapper.readValue(responseJsonString, SaborPizzaResponseDTO.class);
    
        
        assertEquals(dto.getNome(), responseDTO.getNome());
        assertEquals(dto.getDisponivel(), responseDTO.getDisponivel());
        assertEquals(dto.getTipo(), responseDTO.getTipo());
        assertEquals(dto.getPrecoMedia(), responseDTO.getPrecoMedia());
        assertEquals(dto.getPrecoGrande(), responseDTO.getPrecoGrande());
    }
    @Test
    @DisplayName("Quando criamos um sabor de pizza com um estabelecimento inexistente")
    void criarSaborPizzaComEstabelecimentoInexistente() throws Exception {
        SaborPizzaPostPutRequestDTO dto = SaborPizzaPostPutRequestDTO.builder()
                .nome(saborPizza.getNome())
                .disponivel(saborPizza.getDisponivel())
                .tipo(saborPizza.getTipo())
                .precoMedia(saborPizza.getPrecoMedia())
                .precoGrande(saborPizza.getPrecoGrande())
                .build();
    
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_SABOR + "/{idEstabelecimento}/sabores", 999L)  
                .param("codigoAcesso", "999999")  // Adiciona o código de acesso aqui
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
        assertEquals("O estabelecimento consultado nao existe!", resultado.getMessage());
    }
    
    @Test
    @DisplayName("Quando criamos um sabor de pizza com nome nulo")
    void criarSaborPizzaComNomeNulo() throws Exception {
        SaborPizzaPostPutRequestDTO dto = SaborPizzaPostPutRequestDTO.builder()
                .nome("")
                .disponivel(saborPizza.getDisponivel())
                .tipo(saborPizza.getTipo())
                .precoMedia(saborPizza.getPrecoMedia())
                .precoGrande(saborPizza.getPrecoGrande())
                .build();
    
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_SABOR + "/{idEstabelecimento}/sabores", estabelecimento.getId())
                .param("codigoAcesso", "654321") 
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
        assertEquals("Erros de validacao encontrados", resultado.getMessage());
    }
    @Test
    @DisplayName("Quando criamos um sabor de pizza sem informar o tipo")
    void criarSaborPizzaSemTipo() throws Exception {
        SaborPizzaPostPutRequestDTO dto = SaborPizzaPostPutRequestDTO.builder()
                .nome(saborPizza.getNome())
                .disponivel(saborPizza.getDisponivel())
                .tipo(null)  // Tipo não informado
                .precoMedia(saborPizza.getPrecoMedia())
                .precoGrande(saborPizza.getPrecoGrande())
                .build();
    
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_SABOR + "/{idEstabelecimento}/sabores", estabelecimento.getId())
                .param("codigoAcesso", "654321") 

                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
        assertEquals("Erros de validacao encontrados", resultado.getMessage());
    }
}

@Nested
@DisplayName("Conjunto de casos de recuperação de sabor de pizza")
class recuperarSaborPizza{

    @Test
    @DisplayName("Quando recuperamos um sabor de pizza existente")
    void recuperarSaborPizzaExistente() throws Exception {
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_SABOR + "/{idEstabelecimento}/sabores/{idSabor}", estabelecimento.getId(), saborPizza.getId())
        .param("codigoAcesso", "654321") 

                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        SaborPizzaResponseDTO responseDTO = objectMapper.readValue(responseJsonString, SaborPizzaResponseDTO.class);
    
        assertEquals(saborPizza.getNome(), responseDTO.getNome());
        assertEquals(saborPizza.getDisponivel(), responseDTO.getDisponivel());
        assertEquals(saborPizza.getTipo(), responseDTO.getTipo());
        assertEquals(saborPizza.getPrecoMedia(), responseDTO.getPrecoMedia());
        assertEquals(saborPizza.getPrecoGrande(), responseDTO.getPrecoGrande());
    }

    @Test
    @DisplayName("Quando recuperamos um sabor de pizza inexistente")
    void recuperarSaborPizzaInexistente() throws Exception {
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_SABOR + "/{idEstabelecimento}/sabores/{idSabor}", estabelecimento.getId(), 999L)
        .param("codigoAcesso", "654321") 
        
        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
        assertEquals("O sabor consultado nao existe!", resultado.getMessage());
    }
}

@Nested
@DisplayName("Conjunto de casos de listagem de sabores de pizza")
class ListagemDeSaboresPizza{
    @Test
    @DisplayName("Quando listamos os sabores de pizza de um estabelecimento")
    void listarSaborPizzaComSaboresDisponiveis() throws Exception {
        
        SaborPizza saborPizza1 = saborPizzaRepository.save(SaborPizza.builder()
                .nome("Calabresa")
                .disponivel(true)
                .estabelecimento(estabelecimento)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(12.0)
                .precoGrande(22.0)
                .build());
        
        SaborPizza saborPizza2 = saborPizzaRepository.save(SaborPizza.builder()
                .nome("Margherita")
                .disponivel(true)
                .estabelecimento(estabelecimento)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(10.0)
                .precoGrande(20.0)
                .build());
    
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_SABOR + "/{idEstabelecimento}/sabores", estabelecimento.getId())
        .param("codigoAcesso", "654321") 

                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
        SaborPizzaResponseDTO[] responseDTOs = objectMapper.readValue(responseJsonString, SaborPizzaResponseDTO[].class);

        assertEquals(3, responseDTOs.length);

    }
    @Test
    @DisplayName("Quando listamos os sabores de pizza de um estabelecimento inexistente")
    void listarSaborPizzaComEstabelecimentoInexistente() throws Exception {
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_SABOR + "/{idEstabelecimento}/sabores", 999L)
        .param("codigoAcesso", "654321") 

                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
        assertEquals("O estabelecimento consultado nao existe!", resultado.getMessage());
    }
    @Test
    @DisplayName("Quando listamos sabores de pizza e não há sabores disponíveis")
    void listarSaborPizzaSemSaboresDisponiveis() throws Exception {
        
        saborPizzaRepository.deleteAll();
    
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_SABOR + "/{idEstabelecimento}/sabores", estabelecimento.getId())
        .param("codigoAcesso", "654321") 

                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        
        SaborPizzaResponseDTO[] responseDTOs = objectMapper.readValue(responseJsonString, SaborPizzaResponseDTO[].class);
    
        assertEquals(0, responseDTOs.length); 
    }
}

@Nested
@DisplayName("Conjunto de casos de atualização de sabor de pizza")
class AtualizacaoDeSaborPizza{
    @Test
    @DisplayName("Quando atualizamos um sabor de pizza existente")
    void atualizarSaborPizzaExistente() throws Exception {
        SaborPizzaPostPutRequestDTO dto = SaborPizzaPostPutRequestDTO.builder()
                .nome("Margherita")
                .disponivel(true)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(10.0)
                .precoGrande(20.0)
                .build();
    
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_SABOR + "/{idEstabelecimento}/sabores/{idSabor}", estabelecimento.getId(), saborPizza.getId())
        .param("codigoAcesso", "654321") 

                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        SaborPizzaResponseDTO responseDTO = objectMapper.readValue(responseJsonString, SaborPizzaResponseDTO.class);
    
        assertEquals(dto.getNome(), responseDTO.getNome());
        assertEquals(dto.getDisponivel(), responseDTO.getDisponivel());
        assertEquals(dto.getTipo(), responseDTO.getTipo());
        assertEquals(dto.getPrecoMedia(), responseDTO.getPrecoMedia());
        assertEquals(dto.getPrecoGrande(), responseDTO.getPrecoGrande());
    }
    @Test
    @DisplayName("Quando atualizamos um sabor de pizza inexistente")
    void atualizarSaborPizzaInexistente() throws Exception {
        SaborPizzaPostPutRequestDTO dto = SaborPizzaPostPutRequestDTO.builder()
                .nome("Margherita")
                .disponivel(true)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(10.0)
                .precoGrande(20.0)
                .build();
    
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_SABOR + "/{idEstabelecimento}/sabores/{idSabor}", estabelecimento.getId(), 999L)
                .param("codigoAcesso", "654321") 
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
        assertEquals("O sabor consultado nao existe!", resultado.getMessage());
    }
    @Test
    @DisplayName("Quando tentamos atualizar um sabor de pizza com um estabelecimento inexistente")
    void atualizarSaborPizzaComEstabelecimentoInexistente() throws Exception {
        SaborPizzaPostPutRequestDTO dto = SaborPizzaPostPutRequestDTO.builder()
                .nome("Margherita")
                .disponivel(true)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(10.0)
                .precoGrande(20.0)
                .build();
        
        
        Long idEstabelecimentoInexistente = 999L;
    
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_SABOR + "/{idEstabelecimento}/sabores/{idSabor}", idEstabelecimentoInexistente, saborPizza.getId())
                .param("codigoAcesso", "654321") 

                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
        
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
        assertEquals("O estabelecimento consultado nao existe!", resultado.getMessage());
    }
    @Test
    @DisplayName("Quando tentamos atualizar um sabor de pizza que não pertence ao estabelecimento fornecido")
    void atualizarSaborPizzaComSaborNaoPertenceAoEstabelecimento() throws Exception {
        SaborPizzaPostPutRequestDTO dto = SaborPizzaPostPutRequestDTO.builder()
                .nome("Margherita")
                .disponivel(true)
                .tipo(TipoPizza.SALGADA)
                .precoMedia(10.0)
                .precoGrande(20.0)
                .build();
    
        Estabelecimento estabelecimento2 = estabelecimentoRepository.save(Estabelecimento.builder()
                .codigo("123456")
                .build());
    
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_SABOR + "/{idEstabelecimento}/sabores/{idSabor}", estabelecimento2.getId(), saborPizza.getId())
                .param("codigoAcesso", "123456") 
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
        assertEquals("Sabor nao encontrado nesse estabelecimento.", resultado.getMessage());
    }
    @Test
    @DisplayName("Quando tentamos atualizar um sabor de pizza com nome nulo")
    void atualizarSaborPizzaComNomeNulo() throws Exception {
        SaborPizzaPostPutRequestDTO dto = SaborPizzaPostPutRequestDTO.builder()
                .nome(null)  // Nome nulo
                .disponivel(saborPizza.getDisponivel())
                .tipo(saborPizza.getTipo())
                .precoMedia(saborPizza.getPrecoMedia())
                .precoGrande(saborPizza.getPrecoGrande())
                .build();
    
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_SABOR + "/{idEstabelecimento}/sabores/{idSabor}", estabelecimento.getId(), saborPizza.getId())
        .param("codigoAcesso", "654321") 
        .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(dto)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
        assertEquals("Erros de validacao encontrados", resultado.getMessage());
    }

}
@Nested
@DisplayName("Conjunto de casos de exclusão de sabor de pizza")
class ExclusaoDeSaborPizza{
    @Test
    @DisplayName("Quando excluímos um sabor de pizza existente")
    void excluirSaborPizzaExistente() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.delete(URI_SABOR + "/{idEstabelecimento}/sabores/{idSabor}", estabelecimento.getId(), saborPizza.getId())
        .param("codigoAcesso", "654321") 

        .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isNoContent())  
            .andDo(print());
    
        
        assertTrue(saborPizzaRepository.findById(saborPizza.getId()).isEmpty());
    }


    @Test
    @DisplayName("Quando excluímos um sabor de pizza inexistente")
    void excluirSaborPizzaInexistente() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.delete(URI_SABOR + "/{idEstabelecimento}/sabores/{idSabor}", estabelecimento.getId(), 999L)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print());
    }


    @Test
    @DisplayName("Quando excluímos um sabor de pizza com um estabelecimento inexistente")
    void excluirSaborPizzaComEstabelecimentoInexistente() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.delete(URI_SABOR + "/{idEstabelecimento}/sabores/{idSabor}", 999L, saborPizza.getId())
        .param("codigoAcesso", "654321") 

                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print());
    }


    @Test
    @DisplayName("Quando excluímos um sabor de pizza que não pertence ao estabelecimento fornecido")
    void excluirSaborPizzaComSaborNaoPertenceAoEstabelecimento() throws Exception {
        Estabelecimento estabelecimento2 = estabelecimentoRepository.save(Estabelecimento.builder()
                .codigo("123456")
                .build());
    
        mockMvc.perform(MockMvcRequestBuilders.delete(URI_SABOR + "/{idEstabelecimento}/sabores/{idSabor}", estabelecimento2.getId(), saborPizza.getId())
                .param("codigoAcesso", "654321") 
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print());
    }
}

}







