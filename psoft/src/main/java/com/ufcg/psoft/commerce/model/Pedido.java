package com.ufcg.psoft.commerce.model;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoState;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoStateEmPreparo;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoStateEmRota;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoStateEntregue;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoStatePronto;
import com.ufcg.psoft.commerce.service.pedido.statePedido.PedidoStateRecebido;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Entity
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Pedido {

    @JsonProperty("id")
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    @JsonProperty("cliente")
    @ManyToOne
    private Cliente cliente;

    @JsonProperty("estabelecimento")
    @ManyToOne
    private Estabelecimento estabelecimento;

    @JsonProperty("pizzas")
    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, mappedBy = "pedido")
    private List<Pizza> pizzas = new ArrayList<>();

    @JsonProperty("endereco")
    @OneToOne
    private Endereco endereco;

    @JsonProperty("total")
    private Double total;

    @JsonProperty("pago")
    private boolean pago;

    @JsonProperty("dataHora")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm")
    private LocalDateTime dataHora;

    @JsonProperty("entregador")
    @ManyToOne
    private Entregador entregador;

    @JsonProperty("status")
    private StatusPedido status;

    @JsonIgnore
    @Transient
    private PedidoState state;

    @JsonProperty("tempoEsperaEntregador")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm")
    private LocalDateTime tempoEsperaEntregador;

    @JsonIgnore
    @Transient
    private Map<StatusPedido, PedidoState> statusMap = Map.of(
            StatusPedido.PEDIDO_RECEBIDO, new PedidoStateRecebido(this),
            StatusPedido.PEDIDO_EM_PREPARO, new PedidoStateEmPreparo(this),
            StatusPedido.PEDIDO_PRONTO, new PedidoStatePronto(this),
            StatusPedido.PEDIDO_EM_ROTA, new PedidoStateEmRota(this),
            StatusPedido.PEDIDO_ENTREGUE, new PedidoStateEntregue(this)
    );

    public Pedido (Cliente cliente, Estabelecimento estabelecimento, List<Pizza> pizzas, Endereco endereco, double total) {
        this.cliente = cliente;
        this.estabelecimento = estabelecimento;
        this.pizzas = pizzas;
        this.endereco = endereco;
        this.total = total;
        this.pago = false;
        this.dataHora = LocalDateTime.now();
        this.status = StatusPedido.PEDIDO_RECEBIDO;
        this.state = new PedidoStateRecebido(this);
    }

    @PostLoad
    private void initializePedidoStatus(){
        this.state = statusMap.get(this.status);
    }
}
