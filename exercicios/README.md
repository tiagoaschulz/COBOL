# Bootcamp Hackathon COBOL (BHC)

Coleção de exercícios em COBOL desenvolvidos durante o bootcamp, evoluindo de conceitos básicos até processamento de arquivos e subprogramas.

## Exercícios

| Programa | Objetivo |
|---|---|
| `BHCP0000_MODELO` | Modelo/esqueleto padrão usado como base para os demais programas (cabeçalho, divisões, seções). |
| `BHCP0001` | Lê um número digitado pelo usuário, validando que a entrada seja numérica antes de exibi-la. |
| `BHCP0002` | Soma simples de dois números fixos e exibe o resultado. |
| `BHCP0003` | Recebe nome, curso e mensagem do usuário e exibe as informações (movimentação simples de variáveis). |
| `BHCP0004` | Cadastro simples de um cliente (código, nome, agência, conta, saldo inicial) com valores fixos. |
| `BHCP0005` | Cálculo de movimentação bancária: soma depósito e saldo inicial e subtrai um saque, exibindo o saldo final. |
| `BHCP0006` | Classifica um saldo informado como POSITIVO, ZERADO ou NEGATIVO usando `IF`. |
| `BHCP0007` | Recebe o tipo de conta (1 a 4) e retorna a descrição correspondente usando `EVALUATE` e uma tabela em memória. |
| `BHCP0008` | Simula processamento de vendas: recebe 5 valores de venda e uma meta, calcula total e média, e avalia se a meta foi atingida. |
| `BHCP0009` | Cadastro de até 3 funcionários (código, nome, salário) em tabela, classificando o salário em ALTO/LIMITE/NORMAL via condição de nível 88 e montando um relatório com `STRING`. |
| `BHCP0010` | Cadastro de produtos em tabela de tamanho variável (`OCCURS ... DEPENDING ON`), classificando o preço em BARATO/NORMAL/CARO e gerando relatório. |
| `BHCP0011` | Sistema bancário: cadastro de contas (corrente/poupança) em tabela dinâmica, classificação do saldo (VIP/ESPECIAL/PADRÃO) e chamada ao subprograma `BHCS0001` para emitir o relatório de cada cliente. |
| `BHCP0012` | Gera o arquivo `BHCF012S.txt` (sequencial) com dados de participantes (nome, UF, trilha, situação), incluindo data de geração no rodapé. |
| `BHCP0013` | Lê o arquivo `BHCF012S.txt` gerado no exercício anterior e grava um arquivo de log (`BHCF013L.txt`) com as rejeições/processamento de cada registro. |
| `BHCP0014` | Lê `BHCF012S.txt`, chama um subprograma (`BHCS0014`) para processar cada registro e grava dois arquivos de saída: `BHCF014S.txt` (sucesso) e `BHCF014L.txt` (log). |
| `BHCP0015` | Processa lançamentos de conta a partir de um arquivo de entrada (`BHCF015E.txt`), classificando cada lançamento por tipo (D-Débito, S-Saque/Saída, T-Transferência) e gravando arquivo de saída (`BHCF015S.txt`) e de log (`BHCF015L.txt`). |
| `BHCP0016` | Lê `BHCF012S.txt` e gera um arquivo `BHCF016J.json`, convertendo os registros de participantes para o formato JSON (cabeçalho, objetos e rodapé). |
| `BHCS0001` | Subprograma (chamado via `CALL`) responsável por emitir o relatório de um cliente, recebendo nome, tipo de conta, saldo e status via `LINKAGE SECTION`. Usado pelo `BHCP0011`. |

## Progressão didática

1. **Fundamentos (0001–0006):** entrada/saída, variáveis, operações aritméticas e estruturas condicionais (`IF`).
2. **Estruturas de decisão e tabelas (0007–0011):** `EVALUATE`, tabelas (`OCCURS`), condições de nível 88, `STRING` e chamada de subprogramas (`CALL`).
3. **Processamento de arquivos (0012–0016):** leitura e gravação de arquivos sequenciais, geração de logs, integração entre programas via arquivo e geração de saída em JSON.

## Observações

- Todos os programas usam `DECIMAL-POINT IS COMMA` (padrão brasileiro para valores decimais).
- Os programas `BHCP0012` a `BHCP0016` trabalham em conjunto, compartilhando o arquivo `BHCF012S.txt` como base de dados de participantes.
- `BHCP0014` referencia o subprograma `BHCS0014`, que não está incluído neste conjunto de arquivos.
