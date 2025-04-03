package com.ufcg.psoft.commerce.dto.saborPizza;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.ufcg.psoft.commerce.model.SaborPizza;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;

import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
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
public class SaborPizzaResponseDTO {

    @JsonProperty("id")
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @JsonProperty("id_estabelecimento")
    @NotBlank(message = "Estabelecimento obrigatorio")
    private Long idEstabelecimento;

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

    public SaborPizzaResponseDTO (SaborPizza saborPizza) {
        this.id = saborPizza.getId();
        this.idEstabelecimento = saborPizza.getEstabelecimento().getId();
        this.nome = saborPizza.getNome();
        this.tipo = saborPizza.getTipo();
        this.precoMedia = saborPizza.getPrecoMedia();
        this.precoGrande = saborPizza.getPrecoGrande();
        this.disponivel = saborPizza.getDisponivel();
    }
}
