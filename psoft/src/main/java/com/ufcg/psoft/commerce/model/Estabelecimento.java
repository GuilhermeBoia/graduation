package com.ufcg.psoft.commerce.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashSet;
import java.util.Set;

@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Estabelecimento {

    @JsonProperty("id")
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @JsonIgnore
    @Column(nullable = false)
    @Size(min = 6, max = 6)
    private String codigo;

    @JsonProperty("sabores")
    @OneToMany(mappedBy = "estabelecimento")
    private List<SaborPizza> sabores;

    @OneToMany(mappedBy = "estabelecimento")
    private Set<Associacao> associacoes = new HashSet<>();

}
