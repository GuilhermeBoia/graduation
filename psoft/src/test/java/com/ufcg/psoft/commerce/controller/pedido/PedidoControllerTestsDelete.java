package com.ufcg.psoft.commerce.controller.pedido;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.ufcg.psoft.commerce.dto.cliente.EnderecoDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoPostPutRequestDTO;
import com.ufcg.psoft.commerce.exception.CustomErrorType;
import com.ufcg.psoft.commerce.model.*;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;
import com.ufcg.psoft.commerce.repository.*;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoStateEmPreparo;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoStateEmRota;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoStateEntregue;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoStatePronto;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoStateRecebido;

import jakarta.transaction.Transactional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.LocalDateTime;
import java.util.ArrayList;

@Transactional
@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("Conjunto de casos de delete do controlador de Pedidos")
public class PedidoControllerTestsDelete {

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
    Cliente cliente2;
    Estabelecimento estabelecimento;
    Endereco endereco;
    Endereco enderecoCliente;
    Pedido pedido;
    Pedido pedido2;
    PedidoPostPutRequestDTO pedidoPostPutRequestDTO;
    SaborPizza saborPizza1;
    SaborPizza saborPizza2;
    SaborPizza saborPizza3;

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

        cliente2 = clienteRepository.save(Cliente.builder()
                .nome("Outro Cliente")
                .endereco(endereco)
                .codigo("654321")
                .build());

        estabelecimento = estabelecimentoRepository.save(Estabelecimento.builder()
                .codigo("654321")
                .build());

        pedido = pedidoRepository.save(Pedido.builder()
                .cliente(cliente)
                .estabelecimento(estabelecimento)
                .endereco(endereco)
                .pizzas(new ArrayList<>())  
                .total(0.0)  
                .pago(false)
                .status(StatusPedido.PEDIDO_RECEBIDO)  
                .dataHora(LocalDateTime.now())
                .build());  

        pedido2 = pedidoRepository.save(Pedido.builder()
                .cliente(cliente)
                .estabelecimento(estabelecimento)
                .endereco(endereco)
                .pizzas(new ArrayList<>())  
                .total(0.0)  
                .pago(false)
                .status(StatusPedido.PEDIDO_RECEBIDO)  
                .dataHora(LocalDateTime.now())
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
    }

    @Test
    @DisplayName("Quando excluímos um pedido existente com credenciais corretas e status inicial de PEDIDO_RECEBIDO")
    void quandoExcluimosPedidoExistenteComCredenciaisCorretas() throws Exception {
        // Act
        pedido.setState(new PedidoStateRecebido(pedido));
        mockMvc.perform(MockMvcRequestBuilders.delete(URI_PEDIDOS + "/" + pedido.getId())
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNoContent())
                .andDo(print());
    
        // Assert
        assertFalse(pedidoRepository.findById(pedido.getId()).isPresent());
    }
    
    

    @Test
    @DisplayName("Quando tentamos excluir um pedido com senha errada")
    void quandoTentamosExcluirPedidoComSenhaErrada() throws Exception {
        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.delete(URI_PEDIDOS + "/" + pedido.getId())
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", "senhaIncorreta")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        // Assert
        assertEquals("Codigo de acesso invalido!", resultado.getMessage());
    }

    @Test
    @DisplayName("Quando tentamos excluir um pedido inexistente")
    void quandoTentamosExcluirPedidoInexistente() throws Exception {
        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.delete(URI_PEDIDOS + "/999999")
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        // Assert
        assertEquals("O pedido consultado nao existe!", resultado.getMessage());
    }

    @Test
    @DisplayName("Quando tentamos excluir um pedido com ID de cliente errado")
    void quandoTentamosExcluirPedidoComIdClienteErrado() throws Exception {
        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.delete(URI_PEDIDOS + "/" + pedido.getId())
                        .param("idCliente", cliente2.getId().toString()) 
                        .param("codigoAcesso", cliente2.getCodigo())   
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest()) 
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
    
        // Assert
        assertEquals("Pedido nao pertence a esse cliente.", resultado.getMessage()); 
    }
    



    @Test
    @DisplayName("Quando tentamos excluir um pedido com um cliente que não existe")
    void quandoTentamosExcluirPedidoComClienteInexistente() throws Exception {
        // Arrange
        Long idClienteInexistente = 99L; 

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.delete(URI_PEDIDOS + "/" + pedido.getId())
                        .param("idCliente", idClienteInexistente.toString())
                        .param("codigoAcesso", "senhaQualquer") 
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest()) 
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

        // Assert
        assertEquals("O cliente consultado nao existe!", resultado.getMessage());
    }
    @Test
    @DisplayName("Quando tentamos excluir/cancelar um pedido com status PEDIDO_PRONTO")
    void quandoTentamosExcluirPedidoComStatusPedidoPronto() throws Exception {
        // Arrange
        pedido.setStatus(StatusPedido.PEDIDO_PRONTO);
        pedido.setState(new PedidoStatePronto(pedido));
        pedidoRepository.save(pedido); 
    
        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.delete(URI_PEDIDOS + "/" + pedido.getId())
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest()) 
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
    
        // Assert
        assertEquals("O pedido nao pode ser cancelado depois de pronto!", resultado.getMessage());

    }
    
    @Test
    @DisplayName("Quando tentamos excluir/cancelar um pedido com status PEDIDO_EM_ROTA")
    void quandoTentamosExcluirPedidoComStatusPedidoEmRota() throws Exception {
        // Arrange
        pedido.setStatus(StatusPedido.PEDIDO_EM_ROTA);
        pedido.setState(new PedidoStateEmRota(pedido));
        pedidoRepository.save(pedido); 
        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.delete(URI_PEDIDOS + "/" + pedido.getId())
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest()) 
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
    
        // Assert
        assertEquals("O pedido nao pode ser cancelado depois de pronto!", resultado.getMessage());

    }
    
    @Test
    @DisplayName("Quando tentamos excluir/cancelar um pedido com status PEDIDO_ENTREGUE")
    void quandoTentamosExcluirPedidoComStatusPedidoEntregue() throws Exception {
        // Arrange
        pedido.setStatus(StatusPedido.PEDIDO_ENTREGUE);
        pedido.setState(new PedidoStateEntregue(pedido));
        pedidoRepository.save(pedido); 
        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.delete(URI_PEDIDOS + "/" + pedido.getId())
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest()) 
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
    
        // Assert
        assertEquals("O pedido nao pode ser cancelado depois de pronto!", resultado.getMessage());
    }
    @Test
    @DisplayName("Para excluir/cancelar um pedido existente com credenciais corretas e em preparo")
    void quandoExcluimosPedidoExistenteEmPreparo() throws Exception {
        // Arrange
        pedido.setStatus(StatusPedido.PEDIDO_EM_PREPARO);
        pedido.setState(new PedidoStateEmPreparo(pedido));
        pedidoRepository.save(pedido); 
        // Act
        mockMvc.perform(MockMvcRequestBuilders.delete(URI_PEDIDOS + "/" + pedido.getId())
                        .param("idCliente", cliente.getId().toString())
                        .param("codigoAcesso", cliente.getCodigo())
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNoContent())
                .andDo(print());
    
        // Assert
        assertFalse(pedidoRepository.findById(pedido.getId()).isPresent());
    }
}