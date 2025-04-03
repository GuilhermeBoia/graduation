package com.ufcg.psoft.commerce.controller;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;
import com.ufcg.psoft.commerce.repository.EstabelecimentoRepository;
import com.ufcg.psoft.commerce.repository.SaborPizzaRepository;

@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("Testes do controlador de visualização de cardápio para clientes")
public class ClienteVisualizarCardapioTest {

    final String URI_VISUALIZAR_CARDAPIO = "/cliente/visualizar-cardapio";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private EstabelecimentoRepository estabelecimentoRepository;

    @Autowired
    private SaborPizzaRepository saborPizzaRepository;

    private Estabelecimento estabelecimento;
    private SaborPizza saborPizza;

    @BeforeEach
    void setup() {
        estabelecimento = estabelecimentoRepository.save(Estabelecimento.builder()
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
    @DisplayName("Testes para visualização de cardápio estabelecimento")
    class VisualizarCardapioEstabelecimento {
        @Test
        @DisplayName("Quando retornamos o cardápio do estabelecimento com sucesso")
        void deveRetornarCardapioEstabelecimento() throws Exception {
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_VISUALIZAR_CARDAPIO + "/{idEstabelecimento}", estabelecimento.getId()))
                    .andExpect(MockMvcResultMatchers.status().isOk())
                    .andReturn()
                    .getResponse()
                    .getContentAsString();  

            System.out.println(responseJsonString);
        }

        @Test
        @DisplayName("Quando tentamos visualizar cardápio de estabelecimento inexistente")
        void deveRetornarErroCardapioEstabelecimentoInexistente() throws Exception {
            mockMvc.perform(MockMvcRequestBuilders.get(URI_VISUALIZAR_CARDAPIO + "/{idEstabelecimento}", 0))
                    .andExpect(MockMvcResultMatchers.status().isBadRequest());
        }

        @Test
        @DisplayName("Quando tentamos visualizar cardápio de estabelecimento sem id")
        void deveRetornarErroCardapioEstabelecimentoSemId() throws Exception {
            mockMvc.perform(MockMvcRequestBuilders.get(URI_VISUALIZAR_CARDAPIO + "/{idEstabelecimento}", ""))
                    .andExpect(MockMvcResultMatchers.status().isNotFound());
        }

        @Test
        @DisplayName("Quando tentamos visualizar cardápio de estabelecimento com id inválido")
        void deveRetornarErroCardapioEstabelecimentoIdInvalido() throws Exception {
            mockMvc.perform(MockMvcRequestBuilders.get(URI_VISUALIZAR_CARDAPIO + "/{idEstabelecimento}", "abc"))
                    .andExpect(MockMvcResultMatchers.status().isBadRequest());
        }

        @Test
        @DisplayName("Quando tentamos visualizar cardápio de estabelecimento com id nulo")
        void deveRetornarErroCardapioEstabelecimentoIdNulo() throws Exception {
            mockMvc.perform(MockMvcRequestBuilders.get(URI_VISUALIZAR_CARDAPIO + "/{idEstabelecimento}", (String) null))
                    .andExpect(MockMvcResultMatchers.status().isNotFound());
        }
    }

    @Nested
    @DisplayName("Testes para visualização de cardápio Tipo")
    class VisualizarCardapioTipo {
        @Test
        @DisplayName("Quando retornamos o cardápio do estabelecimento com sucesso")
        void deveRetornarCardapioTipo() throws Exception {
            String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_VISUALIZAR_CARDAPIO + "/{idEstabelecimento}/{tipo}", estabelecimento.getId(), TipoPizza.SALGADA))
                    .andExpect(MockMvcResultMatchers.status().isOk())
                    .andReturn()
                    .getResponse()
                    .getContentAsString();  

            System.out.println(responseJsonString);
        }

        @Test
        @DisplayName("Quando tentamos visualizar cardápio de estabelecimento inexistente")
        void deveRetornarErroCardapioTipoEstabelecimentoInexistente() throws Exception {
            mockMvc.perform(MockMvcRequestBuilders.get(URI_VISUALIZAR_CARDAPIO + "/{idEstabelecimento}/{tipo}", 0, TipoPizza.SALGADA))
                    .andExpect(MockMvcResultMatchers.status().isBadRequest());
        }

        @Test
        @DisplayName("Quando tentamos visualizar cardápio de estabelecimento sem id")
        void deveRetornarErroCardapioTipoEstabelecimentoSemId() throws Exception {
            mockMvc.perform(MockMvcRequestBuilders.get(URI_VISUALIZAR_CARDAPIO + "/{idEstabelecimento}/{tipo}", "", TipoPizza.SALGADA))
                    .andExpect(MockMvcResultMatchers.status().isNotFound());
        }

        @Test
        @DisplayName("Quando tentamos visualizar cardápio de estabelecimento com id inválido")
        void deveRetornarErroCardapioTipoEstabelecimentoIdInvalido() throws Exception {
            mockMvc.perform(MockMvcRequestBuilders.get(URI_VISUALIZAR_CARDAPIO + "/{idEstabelecimento}/{tipo}", "abc", TipoPizza.SALGADA))
                    .andExpect(MockMvcResultMatchers.status().isBadRequest());
        }

        
        }
        }

