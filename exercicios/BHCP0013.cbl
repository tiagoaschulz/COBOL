      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0013
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 01/07/2026
      * OBJETIVO..: LER ARQUIVO E GERAR LOG DE REJEICOES
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 01.07.26 TIAGO A. SCHULZ        IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0013.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BHCF012S ASSIGN TO "BHCF012S.txt"
               ORGANIZATION IS SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCF012S.

           SELECT BHCF013L ASSIGN TO "BHCF013L.txt"
                ORGANIZATION IS SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCF013L.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************

      *----------------------------------------
       FILE SECTION.
      *----------------------------------------

      *---- ARQUIVO DE ENTRADA
       FD  BHCF012S
           RECORDING MODE IS F
           RECORD CONTAINS 65 CHARACTERS.
       01 REG-BHCF012S.
           05 IN-CODIGO             PIC 9(005).
           05 IN-NOME               PIC X(030).
           05 IN-UF                 PIC X(002).
           05 IN-TRILHA             PIC X(010).
           05 IN-SITUACAO           PIC X(010).
           05 IN-DATA               PIC 9(008).

      *---- ARQUIVO DE LOG DE REJEICOES
       FD  BHCF013L
           RECORDING MODE IS F
           RECORD CONTAINS 79 CHARACTERS.
       01 REG-BHCF013L.
           05 LOG-CODIGO            PIC 9(005).
           05 LOG-NOME              PIC X(030).
           05 LOG-COD-ERRO          PIC X(004).
           05 LOG-MENSAGEM          PIC X(040).

      *----------------------------------------
       WORKING-STORAGE SECTION.
      *----------------------------------------

      *---- FILE STATUS DOS ARQUIVOS
       01  WS-FILE-STATUS.
           05 WS-FS-BHCF012S        PIC X(002) VALUE SPACES.
           05 WS-FS-BHCF013L        PIC X(002) VALUE SPACES.

      *---- CONSTANTES DE FILE STATUS
       01  WS-CONSTANTES.
           05 WS-FS-OK              PIC X(002) VALUE '00'.
           05 WS-FS-EOF             PIC X(002) VALUE '10'.
           05 WS-STATUS-VALIDO      PIC X(001) VALUE 'V'.
           05 WS-STATUS-REJEITADO   PIC X(001) VALUE 'R'.

      *---- AREA DE LEITURA (READ INTO)
       01  WS-AREA-LEITURA.
           05 WS-CODIGO             PIC 9(005).
           05 WS-NOME               PIC X(030).
           05 WS-UF                 PIC X(002).
           05 WS-TRILHA             PIC X(010).
           05 WS-SITUACAO           PIC X(010).
           05 WS-DATA               PIC 9(008).

      *----------------------------------------
       LOCAL-STORAGE SECTION.
      *----------------------------------------

      *---- CONTROLES DE FIM DE ARQUIVO E STATUS DO REGISTRO
       01  GDA-CONTROLES.
           05 GDA-FIM-ARQ           PIC X(001) VALUE 'N'.
               88 GDA-FIM-SIM           VALUE 'S'.
               88 GDA-FIM-NAO           VALUE 'N'.
           05 GDA-STATUS-REG        PIC X(001) VALUE SPACES.
               88 GDA-REG-VALIDO        VALUE 'V'.
               88 GDA-REG-REJEITADO     VALUE 'R'.
           05 GDA-COD-ERRO          PIC X(004) VALUE SPACES.
           05 GDA-MENSAGEM          PIC X(040) VALUE SPACES.

      *---- TOTALIZADORES
       01  GDA-TOTALIZADORES.
           05 GDA-TOT-LIDOS         PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-VALIDOS       PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-REJEITADOS    PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-ERROS         PIC 9(005) VALUE ZEROS.

      *---- DATA E HORA
       01  GDA-DATA-HORA.
           05 GDA-CURRENT-DATE      PIC X(021).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

       000000-ROTINA-PRINCIPAL.
           PERFORM 100000-INICIALIZAR.
           PERFORM 200000-ABRIR-ARQUIVOS.
           PERFORM 300000-PROCESSAR-ARQUIVO.
           PERFORM 900000-FECHAR-ARQUIVOS.
           PERFORM 910000-EXIBIR-TOTALIZADORES.
           GOBACK.

      ******************************************************************
      *....INICIALIZA CONTROLES E EXIBE INICIO
      ******************************************************************
       100000-INICIALIZAR.
           MOVE ZEROS TO GDA-TOT-LIDOS
                        GDA-TOT-VALIDOS
                        GDA-TOT-REJEITADOS
                        GDA-TOT-ERROS.

           SET GDA-FIM-NAO TO TRUE.

           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.

           DISPLAY 'BHCP0013 - INICIO DO PROCESSAMENTO'.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.

      ******************************************************************
      *....ABRE ARQUIVOS DE ENTRADA E LOG
      ******************************************************************
       200000-ABRIR-ARQUIVOS.
           OPEN INPUT BHCF012S.

           IF WS-FS-BHCF012S NOT = WS-FS-OK
               DISPLAY 'ERRO OPEN INPUT BHCF012S FS=' WS-FS-BHCF012S
               ADD 1 TO GDA-TOT-ERROS
               SET GDA-FIM-SIM TO TRUE
           END-IF.

           OPEN OUTPUT BHCF013L.

           IF WS-FS-BHCF013L NOT = WS-FS-OK
               DISPLAY 'ERRO OPEN OUTPUT BHCF013L FS=' WS-FS-BHCF013L
               ADD 1 TO GDA-TOT-ERROS
               SET GDA-FIM-SIM TO TRUE
           END-IF.

      ******************************************************************
      *....LOOP PRINCIPAL DE LEITURA E PROCESSAMENTO
      ******************************************************************
       300000-PROCESSAR-ARQUIVO.
           PERFORM UNTIL GDA-FIM-SIM
                PERFORM 310000-LER-PARTICIPANTE

                IF GDA-FIM-NAO
                    PERFORM 400000-VALIDAR-PARTICIPANTE

                    IF GDA-REG-VALIDO
                        ADD 1 TO GDA-TOT-VALIDOS
                    ELSE
                        ADD 1 TO GDA-TOT-REJEITADOS
                        PERFORM 500000-GRAVAR-LOG
                    END-IF
                END-IF
           END-PERFORM.

      ******************************************************************
      *....LE UM REGISTRO COM READ INTO E CONTROLA FIM DE ARQUIVO
      ******************************************************************
       310000-LER-PARTICIPANTE.
           READ BHCF012S INTO WS-AREA-LEITURA
                AT END
                    SET GDA-FIM-SIM TO TRUE
                NOT AT END
                    ADD 1 TO GDA-TOT-LIDOS
           END-READ.

           IF WS-FS-BHCF012S NOT = WS-FS-OK
                AND WS-FS-BHCF012S NOT = WS-FS-EOF
                DISPLAY 'ERRO READ BHCF012S FS=' WS-FS-BHCF012S
                ADD 1 TO GDA-TOT-ERROS
                SET GDA-FIM-SIM TO TRUE
           END-IF.

      ******************************************************************
      *....VALIDA CAMPOS OBRIGATORIOS NA ORDEM DAS REGRAS R01 A R06
      ******************************************************************
       400000-VALIDAR-PARTICIPANTE.
           SET GDA-REG-VALIDO TO TRUE.
           MOVE SPACES TO GDA-COD-ERRO
                            GDA-MENSAGEM.

           IF WS-CODIGO = ZEROS
                SET GDA-REG-REJEITADO TO TRUE
                MOVE 'E001' TO GDA-COD-ERRO
                MOVE 'CODIGO NAO INFORMADO' TO GDA-MENSAGEM
           ELSE
                IF WS-NOME = SPACES
                    SET GDA-REG-REJEITADO TO TRUE
                    MOVE 'E002' TO GDA-COD-ERRO
                    MOVE 'NOME NAO INFORMADO' TO GDA-MENSAGEM
                ELSE
                    IF WS-UF = SPACES
                        SET GDA-REG-REJEITADO TO TRUE
                        MOVE 'E003' TO GDA-COD-ERRO
                        MOVE 'UF NAO INFORMADA' TO GDA-MENSAGEM
                    ELSE
                        IF WS-TRILHA = SPACES
                            SET GDA-REG-REJEITADO TO TRUE
                            MOVE 'E004' TO GDA-COD-ERRO
                            MOVE 'TRILHA NAO INFORMADA' TO GDA-MENSAGEM
                        ELSE
                            IF WS-SITUACAO = SPACES
                                SET GDA-REG-REJEITADO TO TRUE
                                MOVE 'E005' TO GDA-COD-ERRO
                                MOVE 'SITUACAO NAO INFORMADA'
                                    TO GDA-MENSAGEM
                            ELSE
                                IF WS-DATA = ZEROS
                                    SET GDA-REG-REJEITADO TO TRUE
                                    MOVE 'E006' TO GDA-COD-ERRO
                                    MOVE 'DATA NAO INFORMADA'
                                        TO GDA-MENSAGEM
                                END-IF
                            END-IF
                        END-IF
                    END-IF
                END-IF
           END-IF.

      ******************************************************************
      *....GRAVA REGISTRO REJEITADO NO ARQUIVO DE LOG
      ******************************************************************
       500000-GRAVAR-LOG.
           MOVE WS-CODIGO     TO LOG-CODIGO.
           MOVE WS-NOME       TO LOG-NOME.
           MOVE GDA-COD-ERRO  TO LOG-COD-ERRO.
           MOVE GDA-MENSAGEM  TO LOG-MENSAGEM.

           WRITE REG-BHCF013L.

           IF WS-FS-BHCF013L NOT = WS-FS-OK
               DISPLAY 'ERRO WRITE BHCF013L FS=' WS-FS-BHCF013L
               ADD 1 TO GDA-TOT-ERROS
           END-IF.

      ******************************************************************
      *....FECHA ARQUIVOS E EXIBE DATA/HORA DE FIM
      ******************************************************************
       900000-FECHAR-ARQUIVOS.
           CLOSE BHCF012S.
           CLOSE BHCF013L.

           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.

           DISPLAY 'BHCP0013 - FIM DO PROCESSAMENTO'.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.

      ******************************************************************
      *....EXIBE TOTALIZADORES NO SYSOUT
      ******************************************************************
       910000-EXIBIR-TOTALIZADORES.
           DISPLAY 'TOTAL DE REGISTROS LIDOS......: ' GDA-TOT-LIDOS.
           DISPLAY 'TOTAL DE REGISTROS VALIDOS....: ' GDA-TOT-VALIDOS.
           DISPLAY 'TOTAL DE REGISTROS REJEITADOS.: ' GDA-TOT-REJEITADOS.

           DISPLAY 'TOTAL DE ERROS DE ARQUIVO.....: ' GDA-TOT-ERROS.
