package com.ufcg.psoft.commerce.dto.saborPizza;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.ufcg.psoft.commerce.model.SaborPizza;
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
public class SaborPizzaCardapioDTO {

    @JsonProperty("nome")
    @NotBlank(message = "Nome obrigatorio")
    private String nome;

    @JsonProperty("preco_media")
    @NotNull(message = "Valor pizza media")
    private Double precoMedia;

    @JsonProperty("preco_grande")
    @NotNull(message = "Valor pizza grande")
    private Double precoGrande;

    @JsonProperty("Disponivel")
    @NotNull(message = "disponibilidade" )
    private Boolean disponivel;

    public SaborPizzaCardapioDTO (SaborPizza saborPizza) {
        this.nome = saborPizza.getNome();
        this.precoMedia = saborPizza.getPrecoMedia();
        this.precoGrande = saborPizza.getPrecoGrande();
        this.disponivel = saborPizza.getDisponivel();
    }
}
