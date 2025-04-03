package com.ufcg.psoft.commerce.controller;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.ufcg.psoft.commerce.dto.cliente.ClientePostPutRequestDTO;
import com.ufcg.psoft.commerce.dto.cliente.ClienteResponseDTO;
import com.ufcg.psoft.commerce.dto.cliente.EnderecoDTO;
import com.ufcg.psoft.commerce.exception.CustomErrorType;
import com.ufcg.psoft.commerce.model.Cliente;
import com.ufcg.psoft.commerce.model.Endereco;
import com.ufcg.psoft.commerce.repository.ClienteRepository;
import com.ufcg.psoft.commerce.repository.EnderecoRepository;
import jakarta.transaction.Transactional;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Transactional
@SpringBootTest
@AutoConfigureMockMvc
@DisplayName("Testes do controlador de Clientes")
public class ClienteControllerTests {

    final String URI_CLIENTES = "/cliente";
    final String URI_ENDERECO = "/endereco";
    @Autowired
    MockMvc mockMvc;


    @Autowired
    MockMvc driver;

    @Autowired
    ClienteRepository clienteRepository;

    @Autowired
    EnderecoRepository enderecoRepository;

    ObjectMapper objectMapper = new ObjectMapper();

    Cliente cliente;
    Endereco endereco;

    ClientePostPutRequestDTO clientePostPutRequestDTO;

    @BeforeEach
    void setup() {
        // Object Mapper suporte para LocalDateTime
        objectMapper.registerModule(new JavaTimeModule());
         Endereco endereco = Endereco.builder()
                .rua("Rua dos Testes")
                .numero("123")
                .bairro("Bairro Teste")
                .cidade("Cidade Teste")
                .estado("Estado Teste")
                .cep("12345-678")
                .build();
                endereco = enderecoRepository.save(endereco);

        cliente = clienteRepository.save(Cliente.builder()
                .nome("Cliente Um da Silva")
                .endereco(endereco)
                .codigo("123456")
                .build()
        );
        clientePostPutRequestDTO = ClientePostPutRequestDTO.builder()
                .nome(cliente.getNome())
                .endereco(EnderecoDTO.builder()
                        .rua(endereco.getRua())
                        .numero(endereco.getNumero())
                        .bairro(endereco.getBairro())
                        .cidade(endereco.getCidade())
                        .estado(endereco.getEstado())
                        .cep(endereco.getCep())
                        .complemento(endereco.getComplemento())
                        .build())
                .codigoAcesso(cliente.getCodigo())
                .build();
    }

    

@Nested
@DisplayName("Conjunto de casos de verificação de nome")
class ClienteVerificacaoNome {

        @Test
        @DisplayName("Quando alteramos o nome do cliente com dados válidos")
        void quandoAlteramosNomeDoClienteValido() throws Exception {
            // Arrange
            clientePostPutRequestDTO.setNome("Cliente Um Alterado");

            // Act
            String responseJsonString = driver.perform(put(URI_CLIENTES + "/" + cliente.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", cliente.getCodigo())
                            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isOk())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();
                Cliente resultado = objectMapper.readValue(responseJsonString, Cliente.class);

            // Assert
            assertEquals("Cliente Um Alterado", resultado.getNome());
        }
        @Test
        @DisplayName("Quando alteramos o nome do cliente nulo")
        void quandoAlteramosNomeDoClienteNulo() throws Exception {
            // Arrange
            clientePostPutRequestDTO.setNome(null);

            // Act
            String responseJsonString = driver.perform(put(URI_CLIENTES + "/" + cliente.getId())
            .contentType(MediaType.APPLICATION_JSON)
            .param("codigoAcesso", cliente.getCodigo())
            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

                CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertAll(
                    () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
                    () -> assertEquals("Nome obrigatorio", resultado.getErrors().get(0))
            );
        }
        @Test
        @DisplayName("Quando alteramos o nome do cliente vazio")
        void quandoAlteramosNomeDoClienteVazio() throws Exception {
            // Arrange
            clientePostPutRequestDTO.setNome("");

            // Act
            String responseJsonString = driver.perform(put(URI_CLIENTES + "/" + cliente.getId())
            .contentType(MediaType.APPLICATION_JSON)
            .param("codigoAcesso", cliente.getCodigo())
            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                .andExpect(status().isBadRequest())
                .andDo(print())
                .andReturn().getResponse().getContentAsString();

        CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);


            // Assert
            assertAll(
                    () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
                    () -> assertEquals("Nome obrigatorio", resultado.getErrors().get(0))
            );
        }
}
@Nested
@DisplayName("Conjunto de casos de verificação do endereço")
class ClienteVerificacaoEndereco {

        @Test
        @DisplayName("Quando alteramos o endereço do cliente com dados válidos")
        void quandoAlteramosEnderecoDoClienteValido() throws Exception {
            // Arrange
            clientePostPutRequestDTO.setEndereco(EnderecoDTO.builder()
            .rua("Rua dos Testes")
                .numero("12")
                .bairro("Bairro Teste")
                .cidade("Cidade Teste")
                .estado("Estado Teste")
                .cep("12344-678")
                .build()
                );

            // Act
            String responseJsonString = driver.perform(put(URI_CLIENTES + "/" + cliente.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", cliente.getCodigo())
                            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isOk())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

                    Cliente resultado = objectMapper.readValue(responseJsonString, Cliente.class);
                    assertEquals("Rua dos Testes", resultado.getEndereco().getRua());
                    assertEquals("12", resultado.getEndereco().getNumero());
                    assertEquals("Bairro Teste", resultado.getEndereco().getBairro());
                    assertEquals("Cidade Teste", resultado.getEndereco().getCidade());
                    assertEquals("Estado Teste", resultado.getEndereco().getEstado());
                    assertEquals("12344-678", resultado.getEndereco().getCep());
        }

        @Test
        @DisplayName("Quando alteramos o endereço do cliente com valor nulo")
        void quandoAlteramosEnderecoDoClienteNulo() throws Exception {
            // Arrange
            clientePostPutRequestDTO.setEndereco(null); // Simulando o endereço nulo
        
            // Act
            String responseJsonString = driver.perform(put(URI_CLIENTES + "/" + cliente.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", cliente.getCodigo())
                            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isBadRequest()) // Espera-se um erro de validação
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();
        
            // Assert
            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
            assertAll(
                    () -> assertEquals("Erros de validacao encontrados", resultado.getMessage()),
                    () -> assertEquals("Endereco obrigatorio", resultado.getErrors().get(0))
            );
        }

        
@Test
@DisplayName("Quando alteramos o endereço do cliente com rua vazia")
void quandoAlteramosEnderecoDoClienteRuaVazia() throws Exception {
    // Arrange
    clientePostPutRequestDTO.setEndereco(EnderecoDTO.builder()
        .rua("")
        .numero("1111")
        .bairro("bairroso")
        .cidade("cidadania")
        .estado("estadunidense")
        .cep("12")
        .build());


    // Act
    String responseJsonString = mockMvc.perform(put(URI_CLIENTES + "/" + cliente.getId())
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("codigoAcesso", cliente.getCodigo())
                    .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

    CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
            assertEquals("Erros de validacao encontrados", resultado.getMessage());
            assertEquals("Rua obrigatoria" , resultado.getErrors().get(0));
}

@Test
@DisplayName("Quando alteramos o endereço do cliente com numero vazio")
void quandoAlteramosEnderecoDoClienteNumeroVazio() throws Exception {
    // Arrange
    clientePostPutRequestDTO.setEndereco(EnderecoDTO.builder()
        .rua("Ruanda")
        .numero("")
        .bairro("bairroso")
        .cidade("cidadania")
        .estado("estadunidense")
        .cep("12")
        .build());


    // Act
    String responseJsonString = mockMvc.perform(put(URI_CLIENTES + "/" + cliente.getId())
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("codigoAcesso", cliente.getCodigo())
                    .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

    CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
            assertEquals("Erros de validacao encontrados", resultado.getMessage());
            assertEquals("Numero obrigatorio" , resultado.getErrors().get(0));
}

@Test
@DisplayName("Quando alteramos o endereço do cliente com bairro vazio")
void quandoAlteramosEnderecoDoClienteBairroVazio() throws Exception {
    // Arrange
    clientePostPutRequestDTO.setEndereco(EnderecoDTO.builder()
        .rua("Ruanda")
        .numero("Numeroso")
        .bairro("")
        .cidade("cidadania")
        .estado("estadunidense")
        .cep("12")
        .build());


    // Act
    String responseJsonString = mockMvc.perform(put(URI_CLIENTES + "/" + cliente.getId())
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("codigoAcesso", cliente.getCodigo())
                    .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

    CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
            assertEquals("Erros de validacao encontrados", resultado.getMessage());
            assertEquals("Bairro obrigatorio" , resultado.getErrors().get(0));
}

@Test
@DisplayName("Quando alteramos o endereço do cliente com cidade vazia")
void quandoAlteramosEnderecoDoClienteCidadeVazia() throws Exception {
    // Arrange
    clientePostPutRequestDTO.setEndereco(EnderecoDTO.builder()
        .rua("Ruanda")
        .numero("Numeroso")
        .bairro("bairroso")
        .cidade("")
        .estado("estadunidense")
        .cep("12")
        .build());


    // Act
    String responseJsonString = mockMvc.perform(put(URI_CLIENTES + "/" + cliente.getId())
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("codigoAcesso", cliente.getCodigo())
                    .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

    CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
            assertEquals("Erros de validacao encontrados", resultado.getMessage());
            assertEquals("Cidade obrigatoria" , resultado.getErrors().get(0));
}

@Test
@DisplayName("Quando alteramos o endereço do cliente com estado vazio")
void quandoAlteramosEnderecoDoClienteEstadoVazio() throws Exception {
    // Arrange
    clientePostPutRequestDTO.setEndereco(EnderecoDTO.builder()
        .rua("Ruanda")
        .numero("Numeroso")
        .bairro("bairroso")
        .cidade("cidadania")
        .estado("")
        .cep("12")
        .build());


    // Act
    String responseJsonString = mockMvc.perform(put(URI_CLIENTES + "/" + cliente.getId())
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("codigoAcesso", cliente.getCodigo())
                    .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

    CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
            assertEquals("Erros de validacao encontrados", resultado.getMessage());
            assertEquals("Estado obrigatorio" , resultado.getErrors().get(0));
}

@Test
@DisplayName("Quando alteramos o endereço do cliente com cep vazio")
void quandoAlteramosEnderecoDoClienteCepVazio() throws Exception {
    // Arrange
    clientePostPutRequestDTO.setEndereco(EnderecoDTO.builder()
        .rua("Ruanda")
        .numero("Numeroso")
        .bairro("bairroso")
        .cidade("cidadania")
        .estado("estadunidense")
        .cep("")
        .build());


    // Act
    String responseJsonString = mockMvc.perform(put(URI_CLIENTES + "/" + cliente.getId())
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("codigoAcesso", cliente.getCodigo())
                    .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isBadRequest())
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

    CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);
            assertEquals("Erros de validacao encontrados", resultado.getMessage());
            assertEquals("CEP obrigatorio" , resultado.getErrors().get(0));
}

}
@Nested
@DisplayName("Conjunto de casos de verificação do código de acesso")
class ClienteVerificacaoCodigoAcesso {

        @Test
        @DisplayName("Quando alteramos o código de acesso do cliente nulo")
        void quandoAlteramosCodigoAcessoDoClienteNulo() throws Exception {
            // Arrange
            clientePostPutRequestDTO.setCodigoAcesso(null);

            // Act
            String responseJsonString = driver.perform(put(URI_CLIENTES + "/" + cliente.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", cliente.getCodigo())
                            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
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
        @DisplayName("Quando alteramos o código de acesso do cliente mais de 6 digitos")
        void quandoAlteramosCodigoAcessoDoClienteMaisDe6Digitos() throws Exception {
            // Arrange
            clientePostPutRequestDTO.setCodigoAcesso("1234567");

            // Act
            String responseJsonString = driver.perform(put(URI_CLIENTES + "/" + cliente.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", cliente.getCodigo())
                            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
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
        @DisplayName("Quando alteramos o código de acesso do cliente menos de 6 digitos")
        void quandoAlteramosCodigoAcessoDoClienteMenosDe6Digitos() throws Exception {
            // Arrange
            clientePostPutRequestDTO.setCodigoAcesso("12345");

            // Act
            String responseJsonString = driver.perform(put(URI_CLIENTES + "/" + cliente.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", cliente.getCodigo())
                            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
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
        @DisplayName("Quando alteramos o código de acesso do cliente caracteres não numéricos")
        void quandoAlteramosCodigoAcessoDoClienteCaracteresNaoNumericos() throws Exception {
            // Arrange
            clientePostPutRequestDTO.setCodigoAcesso("a*c4e@");

            // Act
            String responseJsonString = driver.perform(put(URI_CLIENTES + "/" + cliente.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", cliente.getCodigo())
                            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
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
    }

    @Nested
    @DisplayName("Conjunto de casos de verificação dos fluxos básicos API Rest")
    class ClienteVerificacaoFluxosBasicosApiRest {
        @Test
        @DisplayName("Quando buscamos um cliente salvo pelo id")
        void quandoBuscamosPorUmClienteSalvo() throws Exception {
            // Arrange
            // nenhuma necessidade além do setup()

            // Act
            String responseJsonString = driver.perform(get(URI_CLIENTES + "/" + cliente.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isOk()) // Codigo 200
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            ClienteResponseDTO resultado = objectMapper.readValue(responseJsonString, new TypeReference<>() {});

            // Assert
            assertAll(
                    () -> assertEquals(cliente.getId().longValue(), resultado.getId().longValue()),
                    () -> assertEquals(cliente.getNome(), resultado.getNome())
            );
        }

        @Test
        @DisplayName("Quando buscamos um cliente inexistente")
        void quandoBuscamosPorUmClienteInexistente() throws Exception {
            // Arrange
            // nenhuma necessidade além do setup()

            // Act
            String responseJsonString = driver.perform(get(URI_CLIENTES + "/" + 999999999)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isBadRequest()) // Codigo 400
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertAll(
                    () -> assertEquals("O cliente consultado nao existe!", resultado.getMessage())
            );
        }

        @Test
        @DisplayName("Quando criamos um novo cliente com dados válidos")
        void quandoCriarClienteValido() throws Exception {
            // Arrange
            ClientePostPutRequestDTO novoClientePostPutRequestDTO = ClientePostPutRequestDTO.builder()
                    .nome("Novo Cliente")
                    .codigoAcesso("123456")
                    .endereco(EnderecoDTO.builder()
                        .rua("Ruanda")
                        .numero("Numeroso")
                        .bairro("bairroso")
                        .cidade("cidadania")
                        .estado("estadunidense")
                        .cep("1231")
                        .build())
                    .build();
        
                    String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.post(URI_CLIENTES)
                    .contentType(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(novoClientePostPutRequestDTO)))
                        .andExpect(status().isCreated()) // Código 201
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();

                Cliente resultado = objectMapper.readValue(responseJsonString, Cliente.class);

    
        assertAll(
            () -> assertNotNull(resultado.getId()),
            () -> assertEquals(novoClientePostPutRequestDTO.getNome(), resultado.getNome())
        );
        
        
        }

        @Test
        @DisplayName("Quando alteramos o cliente com dados válidos")
        void quandoAlteramosClienteValido() throws Exception {
                Long clienteId = cliente.getId();
                cliente.setNome("Denner Atualizado");
        
                
                String responseJsonString = mockMvc.perform(MockMvcRequestBuilders.put(URI_CLIENTES + "/" + clienteId)
                                .contentType(MediaType.APPLICATION_JSON)
                                .param("codigoAcesso", cliente.getCodigo())
                                .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                        .andExpect(status().isOk()) // Código 200
                        .andDo(print())
                        .andReturn().getResponse().getContentAsString();
        
                Cliente resultado = objectMapper.readValue(responseJsonString, Cliente.class);
        
                
                assertAll(
                        () -> assertEquals(clienteId, resultado.getId().longValue()),
                        () -> assertEquals(clientePostPutRequestDTO.getNome(), resultado.getNome())
            );
        }

        @Test
        @DisplayName("Quando alteramos o cliente inexistente")
        void quandoAlteramosClienteInexistente() throws Exception {
            // Arrange
            // nenhuma necessidade além do setup()

            // Act
            String responseJsonString = driver.perform(put(URI_CLIENTES + "/" + 99999L)
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", cliente.getCodigo())
                            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isBadRequest()) // Codigo 400
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertAll(
                    () -> assertEquals("O cliente consultado nao existe!", resultado.getMessage())
            );
        }

        @Test
        @DisplayName("Quando alteramos o cliente passando código de acesso inválido")
        void quandoAlteramosClienteCodigoAcessoInvalido() throws Exception {
            // Arrange
            Long clienteId = cliente.getId();

            // Act
            String responseJsonString = driver.perform(put(URI_CLIENTES + "/" + clienteId)
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", "invalido")
                            .content(objectMapper.writeValueAsString(clientePostPutRequestDTO)))
                    .andExpect(status().isBadRequest()) // Codigo 400
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertAll(
                    () -> assertEquals("Codigo de acesso invalido!", resultado.getMessage())
            );
        }

        @Test
        @DisplayName("Quando excluímos um cliente salvo")
        void quandoExcluimosClienteValido() throws Exception {
            // Arrange
            // nenhuma necessidade além do setup()

            // Act
            String responseJsonString = driver.perform(delete(URI_CLIENTES + "/" + cliente.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", cliente.getCodigo()))
                    .andExpect(status().isNoContent()) // Codigo 204
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            // Assert
            assertTrue(responseJsonString.isBlank());
        }

        @Test
        @DisplayName("Quando excluímos um cliente inexistente")
        void quandoExcluimosClienteInexistente() throws Exception {
            // Arrange
            // nenhuma necessidade além do setup()

            // Act
            String responseJsonString = driver.perform(delete(URI_CLIENTES + "/" + 999999)
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", cliente.getCodigo()))
                    .andExpect(status().isBadRequest()) // Codigo 400
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertAll(
                    () -> assertEquals("O cliente consultado nao existe!", resultado.getMessage())
            );
        }

        @Test
        @DisplayName("Quando excluímos um cliente salvo passando código de acesso inválido")
        void quandoExcluimosClienteCodigoAcessoInvalido() throws Exception {
            // Arrange
            // nenhuma necessidade além do setup()

            // Act
            String responseJsonString = driver.perform(delete(URI_CLIENTES + "/" + cliente.getId())
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("codigoAcesso", "invalido"))
                    .andExpect(status().isBadRequest()) // Codigo 400
                    .andDo(print())
                    .andReturn().getResponse().getContentAsString();

            CustomErrorType resultado = objectMapper.readValue(responseJsonString, CustomErrorType.class);

            // Assert
            assertAll(
                    () -> assertEquals("Codigo de acesso invalido!", resultado.getMessage())
            );
        }
    }

}

 


 
