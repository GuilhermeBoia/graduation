package com.ufcg.psoft.commerce.controller;

import com.ufcg.psoft.commerce.model.enums.TipoPizza;
import com.ufcg.psoft.commerce.service.cardapio.ClienteVisualizaCardapioService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(
        value = "/cliente/visualizar-cardapio",
        produces = MediaType.APPLICATION_JSON_VALUE
)
public class ClienteVisualizarCardapio {

    @Autowired
    ClienteVisualizaCardapioService clienteVisualizaCardapioService;

    @GetMapping("/{idEstabelecimento}")
    public ResponseEntity<?> visualizaCardapioEstabelecimento(
            @PathVariable Long idEstabelecimento) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(clienteVisualizaCardapioService.visualizaCardapioEstabelecimento(idEstabelecimento));
    }

    @GetMapping("/{idEstabelecimento}/{tipoPizza}")
    public ResponseEntity<?> visualizaCardapioEstabelecimentoTipo(
            @PathVariable Long idEstabelecimento,
            @PathVariable TipoPizza tipoPizza) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(clienteVisualizaCardapioService.visualizaCardapioEstabelecimentoTipo(idEstabelecimento, tipoPizza));
    }
    
}
