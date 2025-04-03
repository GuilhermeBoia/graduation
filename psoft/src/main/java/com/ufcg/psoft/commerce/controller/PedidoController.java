package com.ufcg.psoft.commerce.controller;

import com.ufcg.psoft.commerce.dto.pedido.PedidoPostPutRequestDTO;
import com.ufcg.psoft.commerce.model.enums.StatusPedido;
import com.ufcg.psoft.commerce.model.enums.TipoPagamento;
import com.ufcg.psoft.commerce.service.pedido.PedidoService;

import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(
        value = "/pedido",
        produces = MediaType.APPLICATION_JSON_VALUE
)
public class PedidoController {
    
    @Autowired
    PedidoService pedidoService;

    @PostMapping("")
    public ResponseEntity<?> criarPedido(
        @RequestParam Long idCliente,
        @RequestParam String codigoAcesso,
        @RequestParam Long idEstabelecimento,
        @RequestBody @Valid PedidoPostPutRequestDTO pedidoPostPutRequestDTO) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(pedidoService.criar(idCliente, codigoAcesso, idEstabelecimento, pedidoPostPutRequestDTO));
    }
    
    @PutMapping("/{idPedido}")
    public ResponseEntity<?> atualizarPedido(
                @RequestParam Long idCliente,
                @RequestParam String codigoAcesso,
                @PathVariable Long idPedido,
                @RequestBody @Valid PedidoPostPutRequestDTO pedidoPostPutRequestDTO) {
                return ResponseEntity
                        .status(HttpStatus.OK)
                        .body(pedidoService.atualizar(idPedido, idCliente, codigoAcesso, pedidoPostPutRequestDTO));
    }

     @GetMapping("/{idCliente}/{idPedido}")
    public ResponseEntity<?> recuperarPedido(
            @PathVariable Long idCliente,
            @RequestParam String codigoAcesso,
            @PathVariable Long idPedido) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(pedidoService.recuperar(idPedido, idCliente, codigoAcesso));
    }
    

    @GetMapping("/{idCliente}")
    public ResponseEntity<?> listarPedido(
        @PathVariable Long idCliente,
        @RequestParam String codigoAcesso) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(pedidoService.listar(idCliente, codigoAcesso));
    }

    @GetMapping("/{idCliente}/status/{statusPedido}")
    public ResponseEntity<?> listarPedidoStatus(
            @PathVariable Long idCliente,
            @RequestParam String codigoAcesso,
            @PathVariable StatusPedido statusPedido) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(pedidoService.listarPorStatus(idCliente, codigoAcesso, statusPedido));
    }

    @DeleteMapping("/{idPedido}")
        public ResponseEntity<?> removerPedido(
                @PathVariable Long idPedido,
                @RequestParam Long idCliente,
                @RequestParam String codigoAcesso) {
                pedidoService.remover(idPedido, idCliente, codigoAcesso);
                return ResponseEntity
                        .status(HttpStatus.NO_CONTENT)
                        .build();
    }

    @PutMapping("/{idPedido}/pagar")
    public ResponseEntity<?> confirmarPagamento(
            @PathVariable Long idPedido,
            @RequestParam Long idCliente,
            @RequestParam String codigoAcesso,
            @RequestParam TipoPagamento tipoPagamento) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(pedidoService.confirmarPagamento(idPedido, idCliente, codigoAcesso, tipoPagamento));
    }

    @PutMapping("/{idPedido}/pronto")
    public ResponseEntity<?> pedidoPronto(
                @PathVariable Long idPedido,
                @RequestParam Long idEstabelecimento,
                @RequestParam String codigoAcesso) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(pedidoService.pedidoPronto(idPedido, idEstabelecimento, codigoAcesso));
   }

    @PutMapping("/{idPedido}/confirmar-entrega")
    public ResponseEntity<?> confirmarEntrega(
                @PathVariable Long idPedido,
                @RequestParam Long idCliente,
                @RequestParam String codigoAcesso) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(pedidoService.confirmarEntrega(idPedido, idCliente, codigoAcesso));
    }
    
}
