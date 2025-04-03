package com.ufcg.psoft.commerce.controller;

import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaPostPutRequestDTO;
import com.ufcg.psoft.commerce.service.cardapio.EstabelecimentoVisualizaCardapioService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(
        value = "/estabelecimento/visualizar-cardapio",
        produces = MediaType.APPLICATION_JSON_VALUE
)
public class EstabelecimentoVisualizarCardapio {

    @Autowired
    EstabelecimentoVisualizaCardapioService estabelecimentoVisualizaCardapioService;

    @GetMapping("/{idEstabelecimento}")
    public ResponseEntity<?> visualizarCardapio(
            @PathVariable Long idEstabelecimento,
            @RequestParam String codigoAcesso) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(estabelecimentoVisualizaCardapioService.visualizarCardapio(idEstabelecimento, codigoAcesso));
    }

    @PutMapping("/{idEstabelecimento}/editar-disponibilidade/{nomePizza}")
    public ResponseEntity<?> editarDisponibilidadePizza(
            @PathVariable Long idEstabelecimento,
            @RequestParam String codigoAcesso,
            @PathVariable String nomePizza,
            @RequestBody @Valid SaborPizzaPostPutRequestDTO saborPizzaPostPutRequestDTO) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(estabelecimentoVisualizaCardapioService.editaDisponibilidadePizza(idEstabelecimento,codigoAcesso, nomePizza));
    }
}
