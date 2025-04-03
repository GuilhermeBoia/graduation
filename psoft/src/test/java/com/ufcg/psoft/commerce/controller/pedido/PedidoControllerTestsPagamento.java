package com.ufcg.psoft.commerce.controller.pedido;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.ufcg.psoft.commerce.dto.cliente.EnderecoDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoResponseDTO;
import com.ufcg.psoft.commerce.dto.pizza.PizzaPostPutDTO;
import com.ufcg.psoft.commerce.exception.CustomErrorType;
import com.ufcg.psoft.commerce.model.*;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;
import com.ufcg.psoft.commerce.model.enums.TamanhoPizza;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;
import com.ufcg.psoft.commerce.repository.*;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoStateEmPreparo;

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
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.List;


@Transactional
@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("Conjunto de casos de Pagamento do controlador de Pedidos")
public class PedidoControllerTestsPagamento {
    
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
    Pedido pedido;
    PedidoPostPutRequestDTO pedidoPostPutRequestDTO;
    Endereco endereco;
    Endereco enderecoCliente;
    SaborPizza saborPizza1;
    SaborPizza saborPizza2;

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
                .precoGrande(40.0)
                .build());

        pedidoRepository.deleteAll();
        List<PizzaPostPutDTO> pizzas = new ArrayList<PizzaPostPutDTO>();
        List<Long> ids = List.of(saborPizza1.getId());
        PizzaPostPutDTO pizza1 = PizzaPostPutDTO.builder()
                .saboresId(ids)
                .tamanho(TamanhoPizza.GRANDE)
                .build();

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
@DisplayName("Caso de teste de pagamento - Sucesso")
class PagamentoSucesso {

    @Test
    @DisplayName("Quando confirma o pagamento com sucesso usando cartão de crédito")
    void quandoConfirmaPagamentoComSucessoCredito() throws Exception {

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .param("tipoPagamento", "CREDITO")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        PedidoResponseDTO resultado = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);

        // Assert
        assertNotNull(resultado);
        assertEquals(pedido.getId(), resultado.getId());
        assertTrue(resultado.isPago());
        assertEquals(20.0, resultado.getTotal());
    }

    @Test
    @DisplayName("Quando confirma o pagamento com sucesso usando pix")
    void quandoConfirmaPagamentoComSucessoPix() throws Exception {

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .param("tipoPagamento", "PIX")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        PedidoResponseDTO resultado = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);

        // Assert
        assertNotNull(resultado);
        assertEquals(pedido.getId(), resultado.getId());
        assertTrue(resultado.isPago());
        assertEquals(19.0, resultado.getTotal());
    }

    @Test
    @DisplayName("Quando confirma o pagamento com sucesso usando Cartão de Débito")
    void quandoConfirmaPagamentoComSucessoDebito() throws Exception {

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .param("tipoPagamento", "DEBITO")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        PedidoResponseDTO resultado = objectMapper.readValue(responseJsonString, PedidoResponseDTO.class);

        // Assert
        assertNotNull(resultado);
        assertEquals(pedido.getId(), resultado.getId());
        assertTrue(resultado.isPago());
        assertEquals(19.5, resultado.getTotal());
    }
}

@Nested
@DisplayName("Caso de teste de pagamento - Erro")
class PagamentoErro {

    @Test
    @DisplayName("Quando confirma pagamento de pedido já pago")
    void quandoConfirmaPagamentoPedidoJaPagoPIX() throws Exception {

        pedido.setPago(true);
        pedido.setState(new PedidoStateEmPreparo(pedido));
        pedido.setStatus(StatusPedido.PEDIDO_EM_PREPARO);

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .param("tipoPagamento", "PIX")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        // Assert
        assertEquals("O pedido ja esta pago!", resultado.getMessage());
    }

    @Test
    @DisplayName("Quando confirma pagamento de pedido já pago")
    void quandoConfirmaPagamentoPedidoJaPagoDebito() throws Exception {

        pedido.setPago(true);
        pedido.setState(new PedidoStateEmPreparo(pedido));
        pedido.setStatus(StatusPedido.PEDIDO_EM_PREPARO);

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .param("tipoPagamento", "DEBITO")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
        
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        // Assert
        assertEquals("O pedido ja esta pago!", resultado.getMessage());
    }

    @Test
    @DisplayName("Quando confirma pagamento de pedido já pago")
    void quandoConfirmaPagamentoPedidoJaPagoCrebito() throws Exception {

        pedido.setPago(true);
        pedido.setState(new PedidoStateEmPreparo(pedido));
        pedido.setStatus(StatusPedido.PEDIDO_EM_PREPARO);

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .param("tipoPagamento", "CREDITO")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        // Assert
        assertEquals("O pedido ja esta pago!", resultado.getMessage());
    }

    @Test
    @DisplayName("Quando confirma pagamento com pedido que não é do cliente")
    void quandoConfirmaPagamentoPedidoNaoDoCliente() throws Exception {

        Cliente cliente2 = clienteRepository.save(Cliente.builder()
                .nome("Cliente Pedido 2")
                .endereco(enderecoCliente)
                .codigo("123456")
                .build());

        pedido.setCliente(cliente2);

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .param("tipoPagamento", "PIX")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);


        // Assert
        assertEquals("Pedido nao pertence a esse cliente.", resultado.getMessage());
    }

    @Test
    @DisplayName("Quando confirma pagamento com pedido que não existe")
    void quandoConfirmaPagamentoPedidoNaoExiste() throws Exception {

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/999999/pagar")
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .param("tipoPagamento", "PIX")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        // Assert
        assertEquals("O pedido consultado nao existe!", resultado.getMessage());
    }

    @Test
    @DisplayName("Quando confirma pagamento com codigo de acesso errado")
    void quandoConfirmaPagamentoCodigoAcessoErrado() throws Exception {

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_PEDIDOS + "/" + pedido.getId() + "/pagar")
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", "123456")
                        .param("tipoPagamento", "PIX")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        // Assert
        assertEquals("Codigo de acesso invalido!", resultado.getMessage());
    }

}

}