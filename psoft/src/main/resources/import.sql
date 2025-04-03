--endereco
INSERT INTO endereco (id, bairro, cep, cidade, complemento, estado, numero, rua)
VALUES (1, 'Centro', '58400-100', 'Campina Grande', 'Apartamento 101', 'PB', '100', 'Rua Almeida');

INSERT INTO endereco (id, bairro, cep, cidade, complemento, estado, numero, rua)
VALUES (2, 'Catole', '58410-200', 'Campina Grande', 'Casa', 'PB', '200', 'Avenida Brasilia');

INSERT INTO endereco (id, bairro, cep, cidade, complemento, estado, numero, rua)
VALUES (3, 'Bodocongo', '58430-300', 'Campina Grande', 'Predio Comercial', 'PB', '300', 'Rua Bodocongo');


--cliente
INSERT INTO cliente (id, codigo, nome, endereco_id)
VALUES (1, '111111', 'Gabriela', 1);

INSERT INTO cliente (id, codigo, nome, endereco_id)
VALUES (2,'111111' ,'Douglas', 2);

INSERT INTO cliente (id,codigo, nome, endereco_id)
VALUES (3,'222222', 'Virginia', 3);

--estabelecimento
INSERT INTO estabelecimento (id, codigo)
VALUES (1, '111111');

INSERT INTO estabelecimento(id, codigo)
VALUES (2, '111111');

INSERT INTO estabelecimento (id, codigo)
VALUES (3, '111111');

--veiculo
INSERT INTO veiculo (id, cor_veiculo, placa, tipo_veiculo)
VALUES (1, 'Preto', 'ABC1234', 'CARRO');

INSERT INTO veiculo (id, cor_veiculo, placa, tipo_veiculo)
VALUES (2, 'Vermelho', 'DEF5678', 'CARRO');

INSERT INTO veiculo (id, cor_veiculo, placa, tipo_veiculo)
VALUES (3, 'Azul', 'GHI9012', 'MOTO');

--entregador 
INSERT INTO entregador (id, codigo, nome, veiculo_id)
VALUES (1, '111111', 'Carlos', 1);

INSERT INTO entregador (id, codigo, nome, veiculo_id)
VALUES (2, '111111', 'Ana', 2);

INSERT INTO entregador (id, codigo, nome, veiculo_id)
VALUES (3, '111111', 'Marcos', 3);


--associacao
INSERT INTO associacao (id, entregador_id, estabelecimento_id, status)
VALUES (1, 1, 1, 'SOB_ANALISE');

INSERT INTO associacao(id, entregador_id, estabelecimento_id, status)
VALUES (2, 2, 2, 'APROVADO');

INSERT INTO associacao (id, entregador_id, estabelecimento_id, status)
VALUES (3, 3, 3, 'REJEITADO');

--sabores
INSERT INTO sabor_pizza (id, disponivel, nome, preco_grande, preco_media, tipo, estabelecimento_id)
VALUES (1, true, 'Margherita', 45.00, 30.00, 'SALGADA', 1);

INSERT INTO sabor_pizza (id, disponivel, nome, preco_grande, preco_media, tipo, estabelecimento_id)
VALUES (2, true, 'Pepperoni', 50.00, 35.00, 'SALGADA', 1);

INSERT INTO sabor_pizza (id, disponivel, nome, preco_grande, preco_media, tipo, estabelecimento_id)
VALUES (3, true, 'Chocolate', 55.00, 38.00, 'DOCE', 1);


--pedido
INSERT INTO pedido (id, pago, total, cliente_id, endereco_id, estabelecimento_id)
VALUES (1, true, 85.00, 1, 1, 1);

INSERT INTO pedido (id, pago, total, cliente_id, endereco_id, estabelecimento_id)
VALUES (2, false, 120.00, 2, 2, 2);

INSERT INTO pedido (id, pago, total, cliente_id, endereco_id, estabelecimento_id)
VALUES (3, true, 95.00, 3, 3, 3);


--pizza
INSERT INTO pizza (id, tamanho, pedido_id)
VALUES (1, 'MEDIA', 1);

INSERT INTO pizza (id, tamanho, pedido_id)
VALUES (2, 'GRANDE', 1);

INSERT INTO pizza (id, tamanho, pedido_id)
VALUES (3, 'MEDIA', 2);
