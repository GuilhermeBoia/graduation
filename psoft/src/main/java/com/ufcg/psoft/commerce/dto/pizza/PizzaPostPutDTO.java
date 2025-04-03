package com.ufcg.psoft.commerce.dto.pizza;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.ufcg.psoft.commerce.model.enums.TamanhoPizza;

import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PizzaPostPutDTO {

    @JsonProperty("tamanho")
    @NotBlank(message = "tamanho obrigatório")
    private TamanhoPizza tamanho;

    @JsonProperty("sabores")
    @NotBlank(message = "sabores obrigatórios")
    private List<Long> saboresId = new ArrayList<>();

}
