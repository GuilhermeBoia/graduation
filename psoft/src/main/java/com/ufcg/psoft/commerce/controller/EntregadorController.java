package com.ufcg.psoft.commerce.controller;

import com.ufcg.psoft.commerce.dto.entregador.EntregadorPostPutRequestDTO;
import com.ufcg.psoft.commerce.model.enums.StatusEntregador;
import com.ufcg.psoft.commerce.service.associacao.AssociacaoService;
import com.ufcg.psoft.commerce.service.entregador.EntregadorService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(
    value = "/entregador",
    produces = MediaType.APPLICATION_JSON_VALUE
)
public class EntregadorController {

    @Autowired
    AssociacaoService associacaoService;

    @Autowired
    EntregadorService entregadorService;

    @GetMapping("/{id}")
    public ResponseEntity<?> recuperarEntregador(
            @PathVariable Long id) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(entregadorService.recuperar(id));
    }

    @GetMapping("")
    public ResponseEntity<?> listarEntregador() {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(entregadorService.listar());
    }

    @PostMapping()
    public ResponseEntity<?> criarEntregador(
            @RequestBody @Valid EntregadorPostPutRequestDTO entregadorPostPutRequestDTO) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(entregadorService.criar(entregadorPostPutRequestDTO));
    }

    @PutMapping("/{id}")
    public ResponseEntity<?> atualizarEntregador(
            @PathVariable Long id,
            @RequestParam String codigoAcesso,
            @RequestBody @Valid EntregadorPostPutRequestDTO entregadorPostPutRequestDTO) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(entregadorService.alterar(id, codigoAcesso, entregadorPostPutRequestDTO));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<?> excluirEntregador(
            @PathVariable Long id,
            @RequestParam String codigoAcesso) {
        entregadorService.remover(id, codigoAcesso);
        return ResponseEntity
                .status(HttpStatus.NO_CONTENT)
                .body("");
    }

    @PostMapping("/{id}/associar")
    public ResponseEntity<?> associaEstabelecimento(
            @PathVariable Long id,
            @RequestParam String codigoAcesso,
            @RequestParam Long estabelecimentoId) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(associacaoService.associarEntregador(id, codigoAcesso, estabelecimentoId));
    }

    @PutMapping("/{id}/associar/{associacaoId}/status")
        public ResponseEntity<?> alteraStatusAssociacao(
                @PathVariable Long id,
                @RequestParam String codigoAcesso,
                @RequestParam Long associacaoId,
                @RequestParam StatusEntregador status) {
                return ResponseEntity
                        .status(HttpStatus.OK)
                        .body(associacaoService.alteraStatusEntregador(id, codigoAcesso, associacaoId, status));
        }
}
