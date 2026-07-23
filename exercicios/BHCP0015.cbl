      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0015
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 02/07/2026
      * OBJETIVO..: PROCESSAMENTO DE LANCAMENTOS DE CONTA
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 02.07.26 TIAGO A. SCHULZ        IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0015.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BHCF015E ASSIGN TO "BHCF015E.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCF015E.

           SELECT BHCF015S ASSIGN TO "BHCF015S.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCF015S.

           SELECT BHCF015L ASSIGN TO "BHCF015L.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCF015L.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************

      *----------------------------------------
       FILE SECTION.
      *----------------------------------------

      *---- ARQUIVO DE ENTRADA
       FD  BHCF015E
           RECORDING MODE IS F.
       01  REG-BHCF015E.
           05 IN-COD-CONTA          PIC 9(005).
           05 IN-DATA-LANC          PIC 9(008).
           05 IN-TIPO-LANC          PIC X(001).
           05 IN-VALOR-LANC         PIC 9(007)V99.
           05 IN-HISTORICO          PIC X(030).

      *---- ARQUIVO DE SAIDA DE VALIDOS
       FD  BHCF015S
           RECORDING MODE IS F.
       01  REG-BHCF015S.
           05 OUT-COD-CONTA         PIC 9(005).
           05 OUT-DATA-LANC         PIC 9(008).
           05 OUT-TIPO-LANC         PIC X(001).
           05 OUT-VALOR-LANC        PIC 9(007)V99.
           05 OUT-HISTORICO         PIC X(030).
           05 OUT-SALDO-APOS        PIC 9(007)V99.

      *---- ARQUIVO DE LOG DE REJEICOES
       FD  BHCF015L
           RECORDING MODE IS F
           RECORD CONTAINS 67 CHARACTERS.
       01  REG-BHCF015L.
           05 LOG-COD-CONTA         PIC 9(005).
           05 LOG-DATA-LANC         PIC 9(008).
           05 LOG-TIPO-LANC         PIC X(001).
           05 LOG-VALOR-LANC        PIC 9(007)V99.
           05 LOG-COD-ERRO          PIC X(004).
           05 LOG-MENSAGEM          PIC X(040).

      *----------------------------------------
       WORKING-STORAGE SECTION.
      *----------------------------------------

      *---- FILE STATUS DOS ARQUIVOS
       01  WS-FILE-STATUS.
           05 WS-FS-BHCF015E        PIC X(002) VALUE SPACES.
           05 WS-FS-BHCF015S        PIC X(002) VALUE SPACES.
           05 WS-FS-BHCF015L        PIC X(002) VALUE SPACES.

      *---- CONSTANTES DE FILE STATUS
       01  WS-CONSTANTES.
           05 WS-FS-OK              PIC X(002) VALUE '00'.
           05 WS-FS-EOF             PIC X(002) VALUE '10'.

      *----------------------------------------
       LOCAL-STORAGE SECTION.
      *----------------------------------------

      *---- CONTROLE DE FIM DE ARQUIVO
       01  GDA-CONTROLES.
           05 GDA-FIM-ARQ           PIC X(001) VALUE 'N'.
               88 GDA-FIM-SIM           VALUE 'S'.
               88 GDA-FIM-NAO           VALUE 'N'.
           05 GDA-STATUS-REG        PIC X(001) VALUE SPACES.
               88 GDA-REG-VALIDO        VALUE 'V'.
               88 GDA-REG-REJEITADO     VALUE 'R'.
           05 GDA-COD-ERRO          PIC X(004) VALUE SPACES.
           05 GDA-MENSAGEM          PIC X(040) VALUE SPACES.

      *---- SALDO DA CONTA
       01  GDA-SALDOS.
           05 GDA-SALDO-INICIAL     PIC 9(007)V99 VALUE 1000.00.
           05 GDA-SALDO-ATUAL       PIC 9(007)V99 VALUE ZEROS.

      *---- TOTALIZADORES DE QUANTIDADE
       01  GDA-TOTALIZADORES.
           05 GDA-TOT-LIDOS         PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-VALIDOS       PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-REJEITADOS    PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-DEPOSITOS     PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-SAQUES        PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-TRANSF        PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-ERROS         PIC 9(005) VALUE ZEROS.

      *---- ACUMULADORES DE VALORES
       01  GDA-ACUMULADORES.
           05 GDA-VLR-DEPOSITOS     PIC 9(009)V99 VALUE ZEROS.
           05 GDA-VLR-SAQUES        PIC 9(009)V99 VALUE ZEROS.
           05 GDA-VLR-TRANSF        PIC 9(009)V99 VALUE ZEROS.

      *---- DATA E HORA
       01 GDA-DATA-HORA.
           05 GDA-CURRENT-DATE      PIC X(021).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR.
           PERFORM 2000-ABRIR-ARQUIVOS.
           PERFORM 3000-PROCESSAR-ARQUIVO.
           PERFORM 8000-FECHAR-ARQUIVOS.
           PERFORM 9000-EXIBIR-RESUMO.
           PERFORM 9999-FINALIZAR.

      ******************************************************************
      *....INICIALIZA CONTROLES E EXIBE INICIO
      ******************************************************************
       1000-INICIALIZAR.
           MOVE ZEROS TO GDA-TOT-LIDOS
                        GDA-TOT-VALIDOS
                        GDA-TOT-REJEITADOS
                        GDA-TOT-DEPOSITOS
                        GDA-TOT-SAQUES
                        GDA-TOT-TRANSF
                        GDA-TOT-ERROS
                        GDA-VLR-DEPOSITOS
                        GDA-VLR-SAQUES
                        GDA-VLR-TRANSF.

           MOVE GDA-SALDO-INICIAL TO GDA-SALDO-ATUAL.

           SET GDA-FIM-NAO TO TRUE.

           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.

           DISPLAY 'BHCP0015 - PROCESSAMENTO DE LANCAMENTOS'.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.

      ******************************************************************
      *....ABRE OS TRES ARQUIVOS COM VALIDACAO DE FILE STATUS
      ******************************************************************
       2000-ABRIR-ARQUIVOS.
           OPEN INPUT BHCF015E.

           IF WS-FS-BHCF015E NOT = WS-FS-OK
                DISPLAY 'ERRO OPEN INPUT BHCF015E FS=' WS-FS-BHCF015E
                ADD 1 TO GDA-TOT-ERROS
                SET GDA-FIM-SIM TO TRUE
           END-IF.

           OPEN OUTPUT BHCF015S.

           IF WS-FS-BHCF015S NOT = WS-FS-OK
                DISPLAY 'ERRO OPEN OUTPUT BHCF015S FS=' WS-FS-BHCF015S
                ADD 1 TO GDA-TOT-ERROS
                SET GDA-FIM-SIM TO TRUE
           END-IF.

           OPEN OUTPUT BHCF015L.

           IF WS-FS-BHCF015L NOT = WS-FS-OK
                DISPLAY 'ERRO OPEN OUTPUT BHCF015L FS=' WS-FS-BHCF015L
                ADD 1 TO GDA-TOT-ERROS
                SET GDA-FIM-SIM TO TRUE
           END-IF.

      ******************************************************************
      *....LOOP PRINCIPAL DE LEITURA E PROCESSAMENTO
      ******************************************************************
       3000-PROCESSAR-ARQUIVO.
           PERFORM UNTIL GDA-FIM-SIM
                PERFORM 3100-LER-ENTRADA

                IF GDA-FIM-NAO
                    PERFORM 3200-VALIDAR-LANCAMENTO

                    IF GDA-REG-VALIDO
                        PERFORM 3300-PROCESSAR-LANCAMENTO
                        PERFORM 3400-GRAVAR-SAIDA
                    ELSE
                        PERFORM 3500-GRAVAR-LOG
                    END-IF
                END-IF
           END-PERFORM.

      ******************************************************************
      *....LE UM REGISTRO DO ARQUIVO DE ENTRADA
      ******************************************************************
       3100-LER-ENTRADA.
           READ BHCF015E
                AT END
                    SET GDA-FIM-SIM TO TRUE
                NOT AT END
                    ADD 1 TO GDA-TOT-LIDOS
           END-READ.

           IF WS-FS-BHCF015E NOT = WS-FS-OK
                AND WS-FS-BHCF015E NOT = WS-FS-EOF
                DISPLAY 'ERRO READ BHCF015E FS=' WS-FS-BHCF015E
                ADD 1 TO GDA-TOT-ERROS
                SET GDA-FIM-SIM TO TRUE
           END-IF.

      ******************************************************************
      *....VALIDA OS CAMPOS OBRIGATORIOS DO LANCAMENTO
      ******************************************************************
       3200-VALIDAR-LANCAMENTO.
           SET GDA-REG-VALIDO TO TRUE.
           MOVE SPACES TO GDA-COD-ERRO
                            GDA-MENSAGEM.

           IF IN-COD-CONTA = ZEROS
                SET GDA-REG-REJEITADO TO TRUE
                MOVE 'E001' TO GDA-COD-ERRO
                MOVE 'CODIGO DA CONTA INVALIDO' TO GDA-MENSAGEM
            ELSE
                IF IN-DATA-LANC = ZEROS
                    SET GDA-REG-REJEITADO TO TRUE
                    MOVE 'E002' TO GDA-COD-ERRO
                    MOVE 'DATA DO LANCAMENTO INVALIDA' TO GDA-MENSAGEM
                ELSE
                    IF IN-TIPO-LANC NOT = 'D'
                        AND IN-TIPO-LANC NOT = 'S'
                        AND IN-TIPO-LANC NOT = 'T'
                        SET GDA-REG-REJEITADO TO TRUE
                        MOVE 'E003' TO GDA-COD-ERRO
                        MOVE 'TIPO DE OPERACAO INVALIDO' TO GDA-MENSAGEM
                    ELSE
                        IF IN-VALOR-LANC = ZEROS
                            SET GDA-REG-REJEITADO TO TRUE
                            MOVE 'E004' TO GDA-COD-ERRO
                            MOVE 'VALOR DO LANCAMENTO INVALIDO'
                                TO GDA-MENSAGEM
                        ELSE
                            IF IN-HISTORICO = SPACES
                                SET GDA-REG-REJEITADO TO TRUE
                                MOVE 'E005' TO GDA-COD-ERRO
                                MOVE 'HISTORICO NAO INFORMADO'
                                    TO GDA-MENSAGEM
                            ELSE
                                IF (IN-TIPO-LANC = 'S'
                                    OR IN-TIPO-LANC = 'T')
                                    AND IN-VALOR-LANC > GDA-SALDO-ATUAL
                                    SET GDA-REG-REJEITADO TO TRUE
                                    MOVE 'E006' TO GDA-COD-ERRO
                                    MOVE 'SALDO INSUFICIENTE'
                                        TO GDA-MENSAGEM
                                END-IF
                            END-IF
                        END-IF
                    END-IF
                END-IF
           END-IF.

      ******************************************************************
      *....PROCESSA O LANCAMENTO E ATUALIZA SALDO E TOTALIZADORES
      ******************************************************************
       3300-PROCESSAR-LANCAMENTO.
           EVALUATE IN-TIPO-LANC
                WHEN 'D'
                    ADD IN-VALOR-LANC TO GDA-SALDO-ATUAL
                    ADD 1              TO GDA-TOT-DEPOSITOS
                    ADD IN-VALOR-LANC  TO GDA-VLR-DEPOSITOS
                WHEN 'S'
                    SUBTRACT IN-VALOR-LANC FROM GDA-SALDO-ATUAL
                    ADD 1               TO GDA-TOT-SAQUES
                    ADD IN-VALOR-LANC   TO GDA-VLR-SAQUES
                WHEN 'T'
                    SUBTRACT IN-VALOR-LANC FROM GDA-SALDO-ATUAL
                    ADD 1               TO GDA-TOT-TRANSF
                    ADD IN-VALOR-LANC   TO GDA-VLR-TRANSF
           END-EVALUATE.

           ADD 1 TO GDA-TOT-VALIDOS.

      ******************************************************************
      *....GRAVA O LANCAMENTO VALIDO NO ARQUIVO DE SAIDA
      ******************************************************************
       3400-GRAVAR-SAIDA.
           MOVE IN-COD-CONTA    TO OUT-COD-CONTA.
           MOVE IN-DATA-LANC    TO OUT-DATA-LANC.
           MOVE IN-TIPO-LANC    TO OUT-TIPO-LANC.
           MOVE IN-VALOR-LANC   TO OUT-VALOR-LANC.
           MOVE IN-HISTORICO    TO OUT-HISTORICO.
           MOVE GDA-SALDO-ATUAL TO OUT-SALDO-APOS.

           WRITE REG-BHCF015S.

           IF WS-FS-BHCF015S NOT = WS-FS-OK
                DISPLAY 'ERRO WRITE BHCF015S FS=' WS-FS-BHCF015S
                ADD 1 TO GDA-TOT-ERROS
           END-IF.

      ******************************************************************
      *....GRAVA O LANCAMENTO REJEITADO NO ARQUIVO DE LOG
      ******************************************************************
       3500-GRAVAR-LOG.
           MOVE IN-COD-CONTA  TO LOG-COD-CONTA.
           MOVE IN-DATA-LANC  TO LOG-DATA-LANC.
           MOVE IN-TIPO-LANC  TO LOG-TIPO-LANC.
           MOVE IN-VALOR-LANC TO LOG-VALOR-LANC.
           MOVE GDA-COD-ERRO  TO LOG-COD-ERRO.
           MOVE GDA-MENSAGEM  TO LOG-MENSAGEM.

           WRITE REG-BHCF015L.

           IF WS-FS-BHCF015L NOT = WS-FS-OK
                DISPLAY 'ERRO WRITE BHCF015L FS=' WS-FS-BHCF015L
                ADD 1 TO GDA-TOT-ERROS
           ELSE
                ADD 1 TO GDA-TOT-REJEITADOS
           END-IF.

      ******************************************************************
      *....FECHA OS TRES ARQUIVOS
      ******************************************************************
       8000-FECHAR-ARQUIVOS.
           CLOSE BHCF015E.
           CLOSE BHCF015S.
           CLOSE BHCF015L.

      ******************************************************************
      *....EXIBE O RESUMO COMPLETO NA SYSOUT
      ******************************************************************
       9000-EXIBIR-RESUMO.
           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.

           DISPLAY '---------------------------------------'.
           DISPLAY 'TOTAL DE REGISTROS LIDOS........: '
                    GDA-TOT-LIDOS.
           DISPLAY 'TOTAL DE REGISTROS VALIDOS......: '
                    GDA-TOT-VALIDOS.
           DISPLAY 'TOTAL DE REGISTROS REJEITADOS...: '
                    GDA-TOT-REJEITADOS.
           DISPLAY 'TOTAL DE DEPOSITOS..............: '
                    GDA-TOT-DEPOSITOS.
           DISPLAY 'TOTAL DE SAQUES.................: '
                    GDA-TOT-SAQUES.
           DISPLAY 'TOTAL DE TRANSFERENCIAS.........: '
                    GDA-TOT-TRANSF.
           DISPLAY 'VALOR TOTAL DEPOSITOS...........: '
                    GDA-VLR-DEPOSITOS.
           DISPLAY 'VALOR TOTAL SAQUES..............: '
                    GDA-VLR-SAQUES.
           DISPLAY 'VALOR TOTAL TRANSFERENCIAS......: '
                    GDA-VLR-TRANSF.
           DISPLAY 'SALDO INICIAL...................: '
                    GDA-SALDO-INICIAL.
           DISPLAY 'SALDO FINAL.....................: '
                    GDA-SALDO-ATUAL.
           DISPLAY 'TOTAL DE ERROS DE ARQUIVO.......: '
                    GDA-TOT-ERROS.
           DISPLAY '---------------------------------------'.

      ******************************************************************
      *....FINALIZA O PROGRAMA
      ******************************************************************
       9999-FINALIZAR.
           DISPLAY 'BHCP0015 - FIM DO PROCESSAMENTO'.
           GOBACK.
