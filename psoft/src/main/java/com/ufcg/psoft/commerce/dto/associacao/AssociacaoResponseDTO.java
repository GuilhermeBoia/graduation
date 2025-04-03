package com.ufcg.psoft.commerce.dto.associacao;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.ufcg.psoft.commerce.model.Associacao;
import com.ufcg.psoft.commerce.model.enums.StatusAssociacao;
import com.ufcg.psoft.commerce.model.enums.StatusEntregador;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AssociacaoResponseDTO {

    @JsonProperty("id")
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @JsonProperty("id_entregador")
    private Long idEntregador;

    @JsonProperty("id_estabelecimento")
    private Long idEstabelecimento;

    @JsonProperty("status")
    private StatusAssociacao status;

    @JsonProperty("status_entregador")
    private StatusEntregador statusEntregador;

    public AssociacaoResponseDTO(Associacao associacao) {
        this.id = associacao.getId();
        this.idEntregador = associacao.getEntregador().getId();
        this.idEstabelecimento = associacao.getEstabelecimento().getId();
        this.status = associacao.getStatus();
        this.statusEntregador = associacao.getStatusEntregador();
    }
}
