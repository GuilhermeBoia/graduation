package com.ufcg.psoft.commerce.model;

import com.fasterxml.jackson.annotation.*;
import com.ufcg.psoft.commerce.model.enums.TamanhoPizza;

import jakarta.persistence.*;
import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Pizza {
    
    @JsonProperty("id")
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @Enumerated(EnumType.STRING)
    private TamanhoPizza tamanho;

    @ManyToOne
    @JoinColumn(name = "pedido_id")
    @JsonIgnore
    private Pedido pedido;

    @ManyToMany
    @JoinTable(name = "pizza_sabor", 
               joinColumns = @JoinColumn(name = "pizza_id"), 
               inverseJoinColumns = @JoinColumn(name = "sabor_id"))
    @Builder.Default
    private List<SaborPizza> sabores = new ArrayList<>();


    public Pizza(TamanhoPizza tamanho, List<SaborPizza> sabores) {
        this.tamanho = tamanho;
        this.sabores = sabores;
    }

    public double calculaPreco(){
        double preco = 0;
        for (SaborPizza sabor : sabores) {
            if (tamanho.equals(TamanhoPizza.GRANDE)){
                preco += sabor.getPrecoGrande();
            }
            else {
                preco += sabor.getPrecoMedia();
            }
        }
        return preco / (sabores.size() == 2 ? 2 : 1);
    }

}
