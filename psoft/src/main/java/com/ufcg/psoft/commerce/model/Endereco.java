package com.ufcg.psoft.commerce.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Endereco {

    @JsonProperty("id")
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @JsonProperty("rua")
    @Column(nullable = false)
    @NotBlank(message = "Rua obrigatória")
    private String rua;

    @JsonProperty("numero")
    @Column(nullable = false)
    @NotBlank(message = "Número obrigatório")
    private String numero;

    @JsonProperty("bairro")
    @Column(nullable = false)
    @NotBlank(message = "Bairro obrigatório")
    private String bairro;

    @JsonProperty("cidade")
    @Column(nullable = false)
    @NotBlank(message = "Cidade obrigatória")
    private String cidade;

    @JsonProperty("estado")
    @Column(nullable = false)
    @NotBlank(message = "Estado obrigatório")
    private String estado;

    @JsonProperty("cep")
    @Column(nullable = false)
    @NotBlank(message = "CEP obrigatório")
    private String cep;

    @JsonProperty("complemento")
    @Column(nullable = true)
    @NotBlank(message = "Complemento obrigatório")
    private String complemento;

}