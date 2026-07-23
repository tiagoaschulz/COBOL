      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0016
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 03/07/2026
      * OBJETIVO..: GERACAO DE ARQUIVO JSON A PARTIR DO BHCF012S
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 03.07.26 TIAGO A. SCHULZ        IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0016.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BHCF012S ASSIGN TO "BHCF012S.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS GDA-FS-BHCF012S.

           SELECT BHCF016J ASSIGN TO "BHCF016J.json"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS GDA-FS-BHCF016J.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************

      *----------------------------------------
       FILE SECTION.
      *----------------------------------------

      *---- ARQUIVO DE ENTRADA
       FD  BHCF012S
           RECORDING MODE IS F.
       01  REG-BHCF012S              PIC X(065).

      *---- ARQUIVO DE SAIDA JSON
       FD  BHCF016J.
       01  REG-BHCF016J              PIC X(200).

      *----------------------------------------
       LOCAL-STORAGE SECTION.
      *----------------------------------------

      *---- FILE STATUS
       01  GDA-FILE-STATUS.
           03 GDA-FS-BHCF012S       PIC X(002) VALUE SPACES.
           03 GDA-FS-BHCF016J       PIC X(002) VALUE SPACES.
           03 GDA-FS-OK             PIC X(002) VALUE '00'.

      *---- CONTROLE DE FIM DE ARQUIVO
       01  WS-FIM-ARQUIVO            PIC X(001) VALUE 'N'.
           88 WS-FIM-SIM                VALUE 'S'.
           88 WS-FIM-NAO                VALUE 'N'.

      *---- TOTALIZADORES
       01  GDA-TOTALIZADORES.
           03 GDA-TOT-LIDOS         PIC 9(005) VALUE ZEROS.
           03 GDA-TOT-JSON          PIC 9(005) VALUE ZEROS.
           03 GDA-TOT-ERROS         PIC 9(005) VALUE ZEROS.

      *---- DATA E HORA
       01  GDA-DATA-HORA.
           03 GDA-CURRENT-DATE      PIC X(021).

      *---- AREA DE TRABALHO DO REGISTRO LIDO
       01  GDA-GR-BHCF012S.
           03 FD-CODIGO             PIC X(005).
           03 FD-NOME               PIC X(030).
           03 FD-UF                 PIC X(002).
           03 FD-TRILHA             PIC X(010).
           03 FD-SITUACAO           PIC X(010).
           03 FD-DATA               PIC X(008).

      *---- AREA DE MONTAGEM DAS LINHAS JSON
       01  GDA-TX-LINHA             PIC X(200).

      *---- AREAS AUXILIARES PARA TRIM DOS CAMPOS
       01  GDA-TX-NOME-TRIM         PIC X(030).
       01  GDA-TX-UF-TRIM           PIC X(002).
       01  GDA-TX-TRILHA-TRIM       PIC X(010).
       01  GDA-TX-SITUACAO-TRIM     PIC X(010).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

       000000-ROTINA-PRINCIPAL.
           PERFORM 100000-INICIALIZAR.
           PERFORM 200000-ABRIR-ARQUIVOS.
           PERFORM 300000-GRAVAR-CABECALHO-JSON.
           PERFORM 400000-LER-ENTRADA.
           PERFORM 500000-PROCESSAR-ARQUIVO.
           PERFORM 800000-GRAVAR-RODAPE-JSON.
           PERFORM 900000-FECHAR-ARQUIVOS.
           PERFORM 910000-EXIBIR-TOTALIZADORES.
           PERFORM 999999-FINALIZAR.

      ******************************************************************
      *....INICIALIZA CONTROLES E EXIBE INICIO
      ******************************************************************
       100000-INICIALIZAR.
           MOVE ZEROS TO GDA-TOT-LIDOS
                        GDA-TOT-JSON
                        GDA-TOT-ERROS.

           SET WS-FIM-NAO TO TRUE.

           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.

           DISPLAY 'BHCP0016 - INICIO DO PROCESSAMENTO'.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.

      ******************************************************************
      *....ABRE OS DOIS ARQUIVOS COM VALIDACAO DE FILE STATUS
      ******************************************************************
       200000-ABRIR-ARQUIVOS.
           OPEN INPUT BHCF012S.

           IF GDA-FS-BHCF012S = GDA-FS-OK
                DISPLAY 'ARQUIVO BHCF012S ABERTO COM SUCESSO'
           ELSE
                DISPLAY 'ERRO OPEN INPUT BHCF012S FS=' GDA-FS-BHCF012S
                ADD 1 TO GDA-TOT-ERROS
                SET WS-FIM-SIM TO TRUE
           END-IF.

           OPEN OUTPUT BHCF016J.

           IF GDA-FS-BHCF016J = GDA-FS-OK
                DISPLAY 'ARQUIVO BHCF016J ABERTO COM SUCESSO'
           ELSE
                DISPLAY 'ERRO OPEN OUTPUT BHCF016J FS=' GDA-FS-BHCF016J
                ADD 1 TO GDA-TOT-ERROS
                SET WS-FIM-SIM TO TRUE
           END-IF.

      ******************************************************************
      *....GRAVA O CABECALHO DO JSON
      ******************************************************************
       300000-GRAVAR-CABECALHO-JSON.
           IF WS-FIM-NAO
                MOVE '{'                  TO REG-BHCF016J
                WRITE REG-BHCF016J

                MOVE '  "participantes": [' TO REG-BHCF016J
                WRITE REG-BHCF016J
           END-IF.

      ******************************************************************
      *....LE O PRIMEIRO REGISTRO ANTES DO LOOP
      ******************************************************************
       400000-LER-ENTRADA.
           IF WS-FIM-NAO
               READ BHCF012S INTO GDA-GR-BHCF012S
                   AT END
                       SET WS-FIM-SIM TO TRUE
                   NOT AT END
                       ADD 1 TO GDA-TOT-LIDOS
               END-READ

               IF GDA-FS-BHCF012S NOT = GDA-FS-OK
                   AND GDA-FS-BHCF012S NOT = '10'
                   DISPLAY 'ERRO READ BHCF012S FS=' GDA-FS-BHCF012S
                   ADD 1 TO GDA-TOT-ERROS
                   SET WS-FIM-SIM TO TRUE
               END-IF
           END-IF.

      ******************************************************************
      *....LOOP DE PROCESSAMENTO - LE E GRAVA ATE FIM DO ARQUIVO
      ******************************************************************
       500000-PROCESSAR-ARQUIVO.
           PERFORM UNTIL WS-FIM-SIM
                PERFORM 510000-GRAVAR-OBJETO-JSON

                READ BHCF012S INTO GDA-GR-BHCF012S
                    AT END
                        SET WS-FIM-SIM TO TRUE
                    NOT AT END
                        ADD 1 TO GDA-TOT-LIDOS
                END-READ

                IF GDA-FS-BHCF012S NOT = GDA-FS-OK
                    AND GDA-FS-BHCF012S NOT = '10'
                    DISPLAY 'ERRO READ BHCF012S FS=' GDA-FS-BHCF012S
                    ADD 1 TO GDA-TOT-ERROS
                    SET WS-FIM-SIM TO TRUE
                END-IF
           END-PERFORM.

      ******************************************************************
      *....MONTA E GRAVA UM OBJETO JSON DO PARTICIPANTE
      ******************************************************************
       510000-GRAVAR-OBJETO-JSON.
      *---- CONTROLE DE VIRGULA ENTRE OBJETOS
           IF GDA-TOT-JSON > 0
                MOVE '    ,'              TO REG-BHCF016J
                WRITE REG-BHCF016J
           END-IF.

      *---- APLICA TRIM NOS CAMPOS ALFANUMERICOS
           MOVE FUNCTION TRIM(FD-NOME    LEADING)
                TO GDA-TX-NOME-TRIM.
           MOVE FUNCTION TRIM(FD-UF      LEADING)
                TO GDA-TX-UF-TRIM.
           MOVE FUNCTION TRIM(FD-TRILHA  LEADING)
                TO GDA-TX-TRILHA-TRIM.
           MOVE FUNCTION TRIM(FD-SITUACAO LEADING)
                TO GDA-TX-SITUACAO-TRIM.

      *---- ABRE O OBJETO
           MOVE '    {'                  TO REG-BHCF016J.
           WRITE REG-BHCF016J.

      *---- CAMPO CODIGO
           MOVE SPACES TO GDA-TX-LINHA.
           STRING '      "codigo": "' DELIMITED BY SIZE
                    FD-CODIGO             DELIMITED BY SIZE
                    '"'                   DELIMITED BY SIZE
                INTO GDA-TX-LINHA.
           MOVE GDA-TX-LINHA TO REG-BHCF016J.
           WRITE REG-BHCF016J.

      *---- CAMPO NOME (COM TRIM E TRATAMENTO DE BRANCO)
           MOVE SPACES TO GDA-TX-LINHA.
           IF FD-NOME = SPACES
                STRING '      "nome": ""' DELIMITED BY SIZE
                    INTO GDA-TX-LINHA
           ELSE
                STRING '      "nome": "' DELIMITED BY SIZE
                        FUNCTION TRIM(FD-NOME) DELIMITED BY SIZE
                        '"'                    DELIMITED BY SIZE
                    INTO GDA-TX-LINHA
           END-IF.
           MOVE GDA-TX-LINHA TO REG-BHCF016J.
           WRITE REG-BHCF016J.

      *---- CAMPO UF (COM TRIM E TRATAMENTO DE BRANCO)
           MOVE SPACES TO GDA-TX-LINHA.
           IF FD-UF = SPACES
                STRING '      "uf": ""' DELIMITED BY SIZE
                    INTO GDA-TX-LINHA
           ELSE
                STRING '      "uf": "' DELIMITED BY SIZE
                        FUNCTION TRIM(FD-UF) DELIMITED BY SIZE
                        '"'                  DELIMITED BY SIZE
                    INTO GDA-TX-LINHA
           END-IF.
           MOVE GDA-TX-LINHA TO REG-BHCF016J.
           WRITE REG-BHCF016J.

      *---- CAMPO TRILHA
           MOVE SPACES TO GDA-TX-LINHA.
           STRING '      "trilha": "' DELIMITED BY SIZE
                    FUNCTION TRIM(FD-TRILHA) DELIMITED BY SIZE
                    '"'                      DELIMITED BY SIZE
                INTO GDA-TX-LINHA.
           MOVE GDA-TX-LINHA TO REG-BHCF016J.
           WRITE REG-BHCF016J.

      *---- CAMPO SITUACAO
           MOVE SPACES TO GDA-TX-LINHA.
           STRING '      "situacao": "' DELIMITED BY SIZE
                    FUNCTION TRIM(FD-SITUACAO) DELIMITED BY SIZE
                    '"'                        DELIMITED BY SIZE
                INTO GDA-TX-LINHA.
           MOVE GDA-TX-LINHA TO REG-BHCF016J.
           WRITE REG-BHCF016J.

      *---- CAMPO DATA
           MOVE SPACES TO GDA-TX-LINHA.
           STRING '      "data": "' DELIMITED BY SIZE
                    FD-DATA              DELIMITED BY SIZE
                    '"'                  DELIMITED BY SIZE
                INTO GDA-TX-LINHA.
           MOVE GDA-TX-LINHA TO REG-BHCF016J.
           WRITE REG-BHCF016J.

      *---- FECHA O OBJETO
           MOVE '    }'                  TO REG-BHCF016J.
           WRITE REG-BHCF016J.

           ADD 1 TO GDA-TOT-JSON.

      ******************************************************************
      *....GRAVA O RODAPE DO JSON
      ******************************************************************
       800000-GRAVAR-RODAPE-JSON.
           MOVE '  ]'                    TO REG-BHCF016J.
           WRITE REG-BHCF016J.

           MOVE '}'                      TO REG-BHCF016J.
           WRITE REG-BHCF016J.

      ******************************************************************
      *....FECHA OS DOIS ARQUIVOS
      ******************************************************************
       900000-FECHAR-ARQUIVOS.
           CLOSE BHCF012S.
           CLOSE BHCF016J.

           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.

      ******************************************************************
      *....EXIBE TOTALIZADORES NO SYSOUT
      ******************************************************************
       910000-EXIBIR-TOTALIZADORES.
           DISPLAY 'TOTAL DE REGISTROS LIDOS......: ' GDA-TOT-LIDOS.
           DISPLAY 'TOTAL DE OBJETOS JSON GERADOS.: ' GDA-TOT-JSON.
           DISPLAY 'TOTAL DE ERROS DE ARQUIVO.....: ' GDA-TOT-ERROS.

      ******************************************************************
      *....FINALIZA O PROGRAMA
      ******************************************************************
       999999-FINALIZAR.
           DISPLAY 'BHCP0016 - FIM DO PROCESSAMENTO'.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.
           GOBACK.
