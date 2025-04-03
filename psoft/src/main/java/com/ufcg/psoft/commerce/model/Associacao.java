package com.ufcg.psoft.commerce.model;

import java.time.LocalDateTime;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.ufcg.psoft.commerce.model.enums.StatusAssociacao;
import com.ufcg.psoft.commerce.model.enums.StatusEntregador;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Associacao {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "entregador", nullable = false)
    private Entregador entregador;

    @ManyToOne
    @JoinColumn(name = "estabelecimento", nullable = false)
    private Estabelecimento estabelecimento;

    @JsonProperty("status")
    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private StatusAssociacao status;

    @JsonProperty("status_entregador")
    @Enumerated(EnumType.STRING)
    @Column(nullable = true)
    private StatusEntregador statusEntregador;

    @JsonProperty("tempoAtivo")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm")
    private LocalDateTime tempoAtivo; 

    public Associacao(Entregador entregador, Estabelecimento estabelecimento) {
        this.entregador = entregador;
        this.estabelecimento = estabelecimento;
        this.status = StatusAssociacao.SOB_ANALISE;
    }

}
