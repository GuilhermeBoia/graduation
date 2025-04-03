package com.ufcg.psoft.commerce.dto.pedido;

import com.fasterxml.jackson.annotation.JsonProperty;

import com.ufcg.psoft.commerce.dto.cliente.EnderecoDTO;
import com.ufcg.psoft.commerce.dto.pizza.PizzaPostPutDTO;
import jakarta.annotation.Nullable;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PedidoPostPutRequestDTO {

    @JsonProperty("endereco")
    @Nullable
    @Valid
    private EnderecoDTO endereco;
    
    @JsonProperty("pizzas")
    @NotEmpty(message = "pizzas obrigatorias")
    private List<PizzaPostPutDTO> pizzas = new ArrayList<>();

}
