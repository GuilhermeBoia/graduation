package com.ufcg.psoft.commerce.dto.pedido;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.ufcg.psoft.commerce.model.*;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PedidoResponseDTO {

    @JsonProperty("id")
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @JsonProperty("cliente")
    private Long idCliente;

    @JsonProperty("id_estabelecimento")
    @NotBlank(message = "Estabelecimento obrigatorio")
    private Long idEstabelecimento;

    @JsonProperty("pizzas")
    private List<Pizza> pizzas;

    @JsonProperty("endereco")
    private Endereco endereco;

    @JsonProperty("total")
    private Double total;

    @JsonProperty("pago")
    private boolean pago;

    @JsonProperty("dataHora")
    private LocalDateTime dataHora;
    
    @JsonProperty("status")
    private StatusPedido status;

    @JsonProperty("id_entregador")
    private Long idEntregador;

    public PedidoResponseDTO (Pedido pedido) {
        this.id = pedido.getId();
        this.idCliente = pedido.getCliente().getId();
        this.idEstabelecimento = pedido.getEstabelecimento().getId();
        this.endereco = pedido.getEndereco();
        this.pizzas = pedido.getPizzas();
        this.total = pedido.getTotal();
        this.pago = pedido.isPago();
        this.dataHora = pedido.getDataHora();
        this.status = pedido.getStatus();

        if (pedido.getEntregador() != null) {
            this.idEntregador = pedido.getEntregador().getId();
        }
        
    }

}
