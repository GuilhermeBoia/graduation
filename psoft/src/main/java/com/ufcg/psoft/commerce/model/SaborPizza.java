package com.ufcg.psoft.commerce.model;

import com.fasterxml.jackson.annotation.*;
import com.ufcg.psoft.commerce.model.enums.TipoPizza;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SaborPizza {

    @JsonProperty("id")
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @JsonProperty("estabelecimento")
    @ManyToOne
    @JsonIgnore
    private Estabelecimento estabelecimento;
    
    @JsonProperty("nome")
    @Column(nullable = false)
    private String nome;

    @JsonProperty("tipo")
    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private  TipoPizza tipo;

    @JsonProperty("preco_media")
    @Column(nullable = false)
    private Double precoMedia;

    @JsonProperty("preco_grande")
    @Column(nullable = false)
    private Double precoGrande;

    @JsonProperty("disponivel")
    @Column(nullable = false)
    private Boolean disponivel;

}
