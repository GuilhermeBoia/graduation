package com.ufcg.psoft.commerce.controller.pedido;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.ufcg.psoft.commerce.dto.pedido.PedidoPostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.pedido.PedidoResponseDTO;
import com.ufcg.psoft.commerce.exception.CustomErrorType;
import com.ufcg.psoft.commerce.model.Cliente;
import com.ufcg.psoft.commerce.model.Endereco;
import com.ufcg.psoft.commerce.model.Estabelecimento;
import com.ufcg.psoft.commerce.model.Pedido;
import com.ufcg.psoft.commerce.model.Pizza;
import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;
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
@DisplayName("Conjunto de casos de Listagem do controlador de Pedidos")
public class PedidoControllerTestsListagem {
    
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
@DisplayName("Quando listamos os pedidos de um cliente com sucesso")
void quandoListamosPedidosDeClienteComSucesso() throws Exception {
        pedidoRepository.deleteAll();
                
        Pizza pizza1 = pizzaRepository.save(Pizza.builder()
                .sabores(List.of(saborPizza1))
                .tamanho(TamanhoPizza.GRANDE)
                .build());

        Pizza pizza2 = pizzaRepository.save(Pizza.builder()
                .sabores(List.of(saborPizza2))
                .tamanho(TamanhoPizza.GRANDE)
                .build());

        Pedido pedido1 = pedidoRepository.save(Pedido.builder()
                .cliente(cliente)
                .estabelecimento(estabelecimento)
                .pizzas(List.of(pizza1))
                .endereco(endereco)
                .total(50.0)
                .pago(false)
                .dataHora(LocalDateTime.of(2024, 9, 17, 10, 0))
                .entregador(null)
                .status(StatusPedido.PEDIDO_RECEBIDO)
                .build());

        Pedido pedido2 = pedidoRepository.save(Pedido.builder()
                .cliente(cliente)
                .estabelecimento(estabelecimento)
                .pizzas(List.of(pizza1, pizza2))
                .endereco(endereco)
                .total(60.0)
                .pago(false)
                .dataHora(LocalDateTime.of(2024, 9, 17, 9, 5))
                .entregador(null)
                .status(StatusPedido.PEDIDO_RECEBIDO)
                .build());

        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_PEDIDOS + "/" + cliente.getId())
                        .param("codigoAcesso", cliente.getCodigo())
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

    
        List<PedidoResponseDTO> resultado = objectMapper.readValue(responseJsonString, objectMapper.getTypeFactory().constructCollectionType(List.class, PedidoResponseDTO.class));

        PedidoResponseDTO pedidoRecuperado1 = resultado.get(0);
        PedidoResponseDTO pedidoRecuperado2 = resultado.get(1);

        assertNotNull(resultado);
        assertTrue(resultado.size() == 2);
        //O metodo listar ordena os pedidos por data, hora e status. Logo, o pedido 1 deve ser o primeiro a ser exibido.
        //visto que passei um horário mais recente na sua criação.
        assertEquals(pedido1.getId(), pedidoRecuperado1.getId());
        assertEquals(pedido2.getId(), pedidoRecuperado2.getId());
        assertEquals(pedido1.getTotal(), pedidoRecuperado1.getTotal());
        assertEquals(pedido2.getTotal(), pedidoRecuperado2.getTotal());
        assertEquals(pedido1.getStatus(), pedidoRecuperado1.getStatus());
        assertEquals(pedido2.getStatus(), pedidoRecuperado2.getStatus());
        assertEquals(pedido1.getDataHora(), pedidoRecuperado1.getDataHora());
        assertEquals(pedido2.getDataHora(), pedidoRecuperado2.getDataHora());
    }

    @Test
    @DisplayName("Quando listamos os pedidos de um cliente e ele possui pedidos com diferentes Status")
        void quandoListamosPedidosDeClienteComPedidosDiferentesStatus() throws Exception {
                Pizza pizza1 = pizzaRepository.save(Pizza.builder()
                        .sabores(List.of(saborPizza1))
                        .tamanho(TamanhoPizza.GRANDE)
                        .build());
        
                Pizza pizza2 = pizzaRepository.save(Pizza.builder()
                        .sabores(List.of(saborPizza2))
                        .tamanho(TamanhoPizza.GRANDE)
                        .build());
        
                Pedido pedido1 = pedidoRepository.save(Pedido.builder()
                        .cliente(cliente)
                        .estabelecimento(estabelecimento)
                        .pizzas(List.of(pizza1))
                        .endereco(endereco)
                        .total(50.0)
                        .pago(true)
                        .dataHora(LocalDateTime.of(2024, 9, 17, 10, 0))
                        .entregador(null)
                        .status(StatusPedido.PEDIDO_EM_PREPARO)
                        .build());
        
                Pedido pedido2 = pedidoRepository.save(Pedido.builder()
                        .cliente(cliente)
                        .estabelecimento(estabelecimento)
                        .pizzas(List.of(pizza1, pizza2))
                        .endereco(endereco)
                        .total(60.0)
                        .pago(false)
                        .dataHora(LocalDateTime.of(2024, 9, 17, 9, 5))
                        .entregador(null)
                        .status(StatusPedido.PEDIDO_RECEBIDO)
                        .build());
        
                
                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_PEDIDOS + "/" + cliente.getId())
                                .param("codigoAcesso", cliente.getCodigo())
                                .contentType(MediaType.APPLICATION_JSON))
                        .andExpect(status().isOk())
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();
        
                List<PedidoResponseDTO> resultado = objectMapper.readValue(responseJsonString, objectMapper.getTypeFactory().constructCollectionType(List.class, PedidoResponseDTO.class));
                

                PedidoResponseDTO pedidoRecuperado1 = resultado.get(0);
                PedidoResponseDTO pedidoRecuperado2 = resultado.get(1);

                //para testar a ordenação, um pedido com status PEDIDO_RECEBIDO deve ser exibido antes de um com status PEDIDO_EM_PREPARO

                assertNotNull(resultado);
                assertTrue(resultado.size() == 2);
                assertTrue(pedidoRecuperado1.getStatus().equals(StatusPedido.PEDIDO_RECEBIDO));
                //de fato, o pedido 2 foi criado antes do pedido 1, mas o pedido 1 foi pago e o pedido 2 não.
                //logo, o pedido 2 deve ser exibido antes do pedido 1.
                assertEquals(pedidoRecuperado1.getId(), pedido2.getId());
                assertEquals(pedidoRecuperado2.getId(), pedido1.getId());

        }

    @Test
    @DisplayName("Quando listamos os pedidos de um cliente com sucesso e sem pizzas")
    void quandoListamosPedidosDeClienteComSucessoSemPizzas() throws Exception {
        // Arrange
        Pizza pizzaVazia = pizzaRepository.save(Pizza.builder()
                .sabores(new ArrayList<>())
                .tamanho(TamanhoPizza.GRANDE)
                .build());

        Pedido pedidoSemPizzas = pedidoRepository.save(Pedido.builder()
                .cliente(cliente)
                .estabelecimento(estabelecimento)
                .pizzas(List.of(pizzaVazia))
                .endereco(endereco)
                .total(0.0)
                .pago(false)
                .dataHora(LocalDateTime.now())
                .entregador(null)
                .status(StatusPedido.PEDIDO_RECEBIDO)
                .build());
    
        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_PEDIDOS + "/" + cliente.getId())
                        .param("codigoAcesso", cliente.getCodigo())
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        List<PedidoResponseDTO> resultado = objectMapper.readValue(responseJsonString, objectMapper.getTypeFactory().constructCollectionType(List.class, PedidoResponseDTO.class));
    
        // Assert
        assertNotNull(resultado);
        assertEquals(1, resultado.size(), "O número de pedidos retornados não é o esperado");
    
        PedidoResponseDTO pedidoRecuperado = resultado.get(0);
        assertEquals(pedidoSemPizzas.getId(), pedidoRecuperado.getId());
        assertEquals(cliente.getId(), pedidoRecuperado.getIdCliente());
        assertEquals(estabelecimento.getId(), pedidoRecuperado.getIdEstabelecimento());
        assertEquals(pedidoSemPizzas.getEndereco().getCep(), pedidoRecuperado.getEndereco().getCep());
        assertTrue(pizzaVazia.getSabores().isEmpty());
    }

    @Test
    @DisplayName("Quando listamos os pedidos de um cliente por status com sucesso")
    void quandoListamosPedidosPorStatusComSucesso() throws Exception {
        // Arrange
        Pizza pizza1 = pizzaRepository.save(Pizza.builder()
                .sabores(List.of(saborPizza1))
                .tamanho(TamanhoPizza.GRANDE)
                .build());
    
        Pizza pizza2 = pizzaRepository.save(Pizza.builder()
                .sabores(List.of(saborPizza2))
                .tamanho(TamanhoPizza.GRANDE)
                .build());
    
        Pedido pedido1 = pedidoRepository.save(Pedido.builder()
                .cliente(cliente)
                .estabelecimento(estabelecimento)
                .pizzas(List.of(pizza1))
                .endereco(endereco)
                .total(pizza1.calculaPreco())
                .pago(false)
                .dataHora(LocalDateTime.now())
                .entregador(null)
                .status(StatusPedido.PEDIDO_RECEBIDO)
                .build());
    
        Pedido pedido2 = pedidoRepository.save(Pedido.builder()
                .cliente(cliente)
                .estabelecimento(estabelecimento)
                .pizzas(List.of(pizza1, pizza2))
                .endereco(endereco)
                .total(pizza1.calculaPreco() + pizza2.calculaPreco())
                .pago(true)
                .dataHora(LocalDateTime.now())
                .entregador(null)
                .status(StatusPedido.PEDIDO_EM_PREPARO) 
                .build());
    
        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_PEDIDOS + "/" + cliente.getId() + "/status/" + StatusPedido.PEDIDO_RECEBIDO)
                        .param("codigoAcesso", cliente.getCodigo())
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
    
        List<PedidoResponseDTO> resultado = objectMapper.readValue(responseJsonString, objectMapper.getTypeFactory().constructCollectionType(List.class, PedidoResponseDTO.class));
    
        // Assert
        assertNotNull(resultado);
        assertEquals(1, resultado.size(), "O número de pedidos retornados não é o esperado");
        
        //O objetivo é verificar que o teste só exibe o resultado do pedido com status PEDIDO_RECEBIDO,
        //mesmo que o cliente tenha feito dois pedidos.
        PedidoResponseDTO pedidoRecuperado = resultado.get(0);
        assertEquals("PEDIDO_RECEBIDO", pedidoRecuperado.getStatus().toString());
        assertEquals(pedido1.getId(), pedidoRecuperado.getId());
        assertEquals(cliente.getId(), pedidoRecuperado.getIdCliente());
        assertEquals(estabelecimento.getId(), pedidoRecuperado.getIdEstabelecimento());
        assertEquals(pedido1.getEndereco().getCep(), pedidoRecuperado.getEndereco().getCep());
    }

    @Test
    @DisplayName("Quando tentamos listar os pedidos de um cliente com senha errada")
    void quandoTentamosListarPedidosDeClienteComSenhaErrada() throws Exception {
        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_PEDIDOS + "/" + cliente.getId())
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
    @DisplayName("Quando tentamos listar os pedidos de um cliente que não existe")
    void quandoTentamosListarPedidosDeClienteInexistente() throws Exception {
        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_PEDIDOS + "/999999")
                        .param("codigoAcesso", "senhaQualquer") // Pode ser qualquer senha, pois o cliente não existe
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
        
        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
        
        // Assert
        assertEquals("O cliente consultado nao existe!", resultado.getMessage());
    }

    @Test
    @DisplayName("Quando listamos os pedidos de um cliente que não tem pedidos")
    void quandoListamosPedidosDeClienteSemPedidos() throws Exception {
        // Arrange
        Cliente clienteSemPedidos = clienteRepository.save(Cliente.builder()
                .nome("Cliente Sem Pedidos")
                .endereco(endereco)
                .codigo("123456")
                .build());
        
        // Act
        String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.get(URI_PEDIDOS + "/" + clienteSemPedidos.getId())
                        .param("codigoAcesso", clienteSemPedidos.getCodigo())
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();
                
        List<PedidoResponseDTO> resultado = objectMapper.readValue(responseJsonString, objectMapper.getTypeFactory().constructCollectionType(List.class, PedidoResponseDTO.class));
                
        // Assert
        assertNotNull(resultado);
        assertTrue(resultado.isEmpty());
    }

}