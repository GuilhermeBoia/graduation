package com.ufcg.psoft.commerce.dto.entregador;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.ufcg.psoft.commerce.model.enums.TipoVeiculo;

import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class VeiculoDTO {

    @JsonProperty("placa")
    @NotBlank(message = "Placa obrigatoria")
    private String placa;

    @JsonProperty("tipoVeiculo")
    private TipoVeiculo tipoVeiculo;

    @JsonProperty("corVeiculo")
    @NotBlank(message = "cor do veiculo obrigatoria")
    private String corVeiculo;
}

