package com.ufcg.psoft.commerce.dto.cliente;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EnderecoDTO {

    @JsonProperty("rua")
    @NotBlank(message = "Rua obrigatoria")
    private String rua;

    @JsonProperty("numero")
    @NotBlank(message = "Numero obrigatorio")
    private String numero;

    @JsonProperty("bairro")
    @NotBlank(message = "Bairro obrigatorio")
    private String bairro;

    @JsonProperty("cidade")
    @NotBlank(message = "Cidade obrigatoria")
    private String cidade;

    @JsonProperty("estado")
    @NotBlank(message = "Estado obrigatorio")
    private String estado;

    @JsonProperty("cep")
    @NotBlank(message = "CEP obrigatorio")
    private String cep;

    @JsonProperty("complemento")
    private String complemento;

}