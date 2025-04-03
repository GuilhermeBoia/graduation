package com.ufcg.psoft.commerce.dto.InteresseSaborPizza;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.ufcg.psoft.commerce.model.InteresseSaborPizza;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class InteresseSaborPizzaResponseDTO {

    @JsonProperty("id")
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @JsonProperty("id_cliente")
    private Long idCliente;

    @JsonProperty("id_sabor_pizza")
    private Long idSaborPizza;

    @JsonProperty("notificado")
    private boolean notificado;


    public InteresseSaborPizzaResponseDTO(InteresseSaborPizza interesse) {
        this.id = interesse.getId();
        this.idCliente = interesse.getCliente().getId();
        this.idSaborPizza = interesse.getSaborPizza().getId();
        this.notificado = interesse.isNotificado();
    }

}
