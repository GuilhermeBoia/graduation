package com.ufcg.psoft.commerce.controller;

import com.ufcg.psoft.commerce.dto.estabelecimento.EstabelecimentoPostPutDTO;
import com.ufcg.psoft.commerce.model.enums.StatusAssociacao;
import com.ufcg.psoft.commerce.service.associacao.AssociacaoService;
import com.ufcg.psoft.commerce.service.estabelecimento.EstabelecimentoService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(
        value = "/estabelecimento",
        produces = MediaType.APPLICATION_JSON_VALUE
)
public class EstabelecimentoController {

    @Autowired
    EstabelecimentoService estabelecimentoService;

    @Autowired
    AssociacaoService associacaoService;

    @PostMapping()
    public ResponseEntity<?> criarEstabelecimento(
            @RequestBody @Valid EstabelecimentoPostPutDTO estabelecimentoPostPutDTO) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(estabelecimentoService.criar(estabelecimentoPostPutDTO));
    }

    @PutMapping("/{id}")
    public ResponseEntity<?> atualizarEstabelecimento(
            @PathVariable Long id,
            @RequestParam String codigoAcesso,
            @RequestBody @Valid EstabelecimentoPostPutDTO estabelecimentoPostPutDTO) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(estabelecimentoService.alterar(id, codigoAcesso, estabelecimentoPostPutDTO));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<?> excluirEstabelecimento(
            @PathVariable Long id,
            @RequestParam String codigoAcesso) {
        estabelecimentoService.remover(id, codigoAcesso);
        return ResponseEntity
                .status(HttpStatus.NO_CONTENT)
                .body("");
    }

    @PutMapping("/{id}/associar")
    public ResponseEntity<?> analisaAssociacao(
            @PathVariable Long id,
            @RequestParam String codigoAcesso,
            @RequestParam Long associacaoId,
            @RequestParam StatusAssociacao statusAssociacao) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(associacaoService.analisaAssociacao(id, codigoAcesso, associacaoId,statusAssociacao));
    }
}
