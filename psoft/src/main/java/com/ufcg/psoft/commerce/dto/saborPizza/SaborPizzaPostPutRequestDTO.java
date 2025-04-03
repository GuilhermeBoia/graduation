package com.ufcg.psoft.commerce.dto.saborPizza;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor

public class SaborPizzaPostPutRequestDTO {

    @JsonProperty("nome")
    @NotBlank(message = "Nome obrigatorio")
    private String nome;

    @JsonProperty("tipo")
    @NotNull(message = "Doce ou salgada")
    private TipoPizza tipo;

    @JsonProperty("preco_media")
    @NotNull(message = "Valor pizza media")
    private Double precoMedia;

    @JsonProperty("preco_grande")
    @NotNull(message = "Valor pizza grande")
    private Double precoGrande;

    @JsonProperty("disponivel")
    @NotNull(message = "Disponivel ou indisponivel")
    private Boolean disponivel;
}
