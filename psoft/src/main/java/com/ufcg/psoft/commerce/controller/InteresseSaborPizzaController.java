package com.ufcg.psoft.commerce.controller;

import com.ufcg.psoft.commerce.service.InteresseSaborPizza.InteresseSaborPizzaService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/interesse")
public class InteresseSaborPizzaController {

    @Autowired
    private InteresseSaborPizzaService interesseSaborPizzaService;

    @PostMapping("/registrar")
    public ResponseEntity<?> registrarInteresse(
            @RequestParam Long clienteId,
            @RequestParam Long saborPizzaId) {
        interesseSaborPizzaService.registrarInteresse(clienteId, saborPizzaId);
        return ResponseEntity.ok("Interesse registrado com sucesso!");
    }

    @PutMapping("/notificar")
    public ResponseEntity<?> notificarClientes(
            @RequestParam Long saborPizzaId) {
        String resposta = interesseSaborPizzaService.notificarClientes(saborPizzaId);
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(resposta);
    }
}
