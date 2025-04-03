package com.ufcg.psoft.commerce.controller;

import com.ufcg.psoft.commerce.dto.saborPizza.SaborPizzaPostPutRequestDTO;
import com.ufcg.psoft.commerce.service.saborPizza.SaborPizzaService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(
        value = "/sabores",
        produces = MediaType.APPLICATION_JSON_VALUE
)
public class SaborPizzaController {

    @Autowired
    SaborPizzaService saborPizzaService;

    @PostMapping("/{idEstabelecimento}/sabores")
    public ResponseEntity<?> criarSaborPizza(
        @PathVariable Long idEstabelecimento,
        @RequestParam String codigoAcesso,    
        @RequestBody @Valid SaborPizzaPostPutRequestDTO saborPizzaPostPutRequestDTO) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(saborPizzaService.criar(idEstabelecimento,codigoAcesso,saborPizzaPostPutRequestDTO));
    }

    @GetMapping("/{idEstabelecimento}/sabores/{idSabor}")
    public ResponseEntity<?> recuperarSaborPizza(
            @PathVariable Long idEstabelecimento,
            @RequestParam String codigoAcesso,
            @PathVariable Long idSabor) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(saborPizzaService.recuperar(idSabor, idEstabelecimento, codigoAcesso));
    }

    @GetMapping("/{idEstabelecimento}/sabores")
    public ResponseEntity<?> listarSaborPizza(
        @PathVariable Long idEstabelecimento,
        @RequestParam String codigoAcesso) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(saborPizzaService.listar(idEstabelecimento, codigoAcesso));
    }


    @PutMapping("/{idEstabelecimento}/sabores/{idSabor}")
    public ResponseEntity<?> atualizarSaborPizza(
            @PathVariable Long idSabor,
            @PathVariable Long idEstabelecimento,
            @RequestParam String codigoAcesso,
            @RequestBody @Valid SaborPizzaPostPutRequestDTO saborPizzaPostPutRequestDTO) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(saborPizzaService.alterar(idSabor, idEstabelecimento, codigoAcesso, saborPizzaPostPutRequestDTO));
    }

    @DeleteMapping("/{idEstabelecimento}/sabores/{idSabor}")
    public ResponseEntity<?> excluirSaborPizza(
        @PathVariable Long idSabor,
        @PathVariable Long idEstabelecimento,
        @RequestParam String codigoAcesso) {
        saborPizzaService.remover(idSabor, idEstabelecimento, codigoAcesso);
        return ResponseEntity
                .status(HttpStatus.NO_CONTENT)
                .body("");
    }
}