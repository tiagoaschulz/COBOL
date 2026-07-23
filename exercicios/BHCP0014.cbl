      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0014
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 01/07/2026
      * OBJETIVO..: PROCESSAMENTO COMPLETO COM SUBPROGRAMA
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

       PROGRAM-ID. BHCP0014.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BHCF012S ASSIGN TO "BHCF012S.txt"
                ORGANIZATION IS SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCF012S.

           SELECT BHCF014S ASSIGN TO "BHCF014S.txt"
                ORGANIZATION IS SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCF014S.

           SELECT BHCF014L ASSIGN TO "BHCF014L.txt"
                ORGANIZATION IS SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCF014L.

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
       01  REG-BHCF012S.
           05 IN-CODIGO             PIC 9(005).
           05 IN-NOME               PIC X(030).
           05 IN-UF                 PIC X(002).
           05 IN-TRILHA             PIC X(010).
           05 IN-SITUACAO           PIC X(010).
           05 IN-DATA               PIC 9(008).

      *---- ARQUIVO DE SAIDA DE VALIDOS
       FD  BHCF014S
           RECORDING MODE IS F
           RECORD CONTAINS 65 CHARACTERS.
       01  REG-BHCF014S.
           05 OUT-CODIGO            PIC 9(005).
           05 OUT-NOME              PIC X(030).
           05 OUT-UF                PIC X(002).
           05 OUT-TRILHA            PIC X(010).
           05 OUT-SITUACAO          PIC X(010).
           05 OUT-DATA              PIC 9(008).

      *---- ARQUIVO DE LOG DE REJEICOES
       FD  BHCF014L
           RECORDING MODE IS F
           RECORD CONTAINS 79 CHARACTERS.
       01  REG-BHCF014L.
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
           05 WS-FS-BHCF014S        PIC X(002) VALUE SPACES.
           05 WS-FS-BHCF014L        PIC X(002) VALUE SPACES.

      *---- CONSTANTES
       01  WS-CONSTANTES.
           05 WS-FS-OK              PIC X(002) VALUE '00'.
           05 WS-FS-EOF             PIC X(002) VALUE '10'.
           05 WS-SUBPROGRAMA        PIC X(008) VALUE 'BHCS0014'.

      *----------------------------------------
       LOCAL-STORAGE SECTION.
      *----------------------------------------

      *---- CONTROLE DE FIM DE ARQUIVO
       01  GDA-CONTROLES.
           05 GDA-FIM-ARQ           PIC X(001) VALUE 'N'.
               88 GDA-FIM-SIM           VALUE 'S'.
               88 GDA-FIM-NAO           VALUE 'N'.

      *---- TOTALIZADORES
       01  GDA-TOTALIZADORES.
           05 GDA-TOT-LIDOS         PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-VALIDOS       PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-REJEITADOS    PIC 9(005) VALUE ZEROS.
           05 GDA-TOT-ERROS         PIC 9(005) VALUE ZEROS.

      *---- AREA DE COMUNICACAO COM O SUBPROGRAMA
       01  GDA-AREA-COMUNICACAO.
           05 GDA-COM-CODIGO        PIC 9(005).
           05 GDA-COM-NOME          PIC X(030).
           05 GDA-COM-UF            PIC X(002).
           05 GDA-COM-TRILHA        PIC X(010).
           05 GDA-COM-SITUACAO      PIC X(010).
           05 GDA-COM-DATA          PIC 9(008).
           05 GDA-COM-STATUS        PIC X(001).
           05 GDA-COM-COD-ERRO      PIC X(004).
           05 GDA-COM-MENSAGEM      PIC X(040).

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

           DISPLAY 'BHCP0014 - INICIO DO PROCESSAMENTO'.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.

      ******************************************************************
      *....ABRE OS TRES ARQUIVOS
      ******************************************************************
       200000-ABRIR-ARQUIVOS.
           OPEN INPUT BHCF012S.

           IF WS-FS-BHCF012S NOT = WS-FS-OK
                DISPLAY 'ERRO OPEN INPUT BHCF012S FS=' WS-FS-BHCF012S
                ADD 1 TO GDA-TOT-ERROS
                SET GDA-FIM-SIM TO TRUE
           END-IF.

           OPEN OUTPUT BHCF014S.

           IF WS-FS-BHCF014S NOT = WS-FS-OK
                DISPLAY 'ERRO OPEN OUTPUT BHCF014S FS=' WS-FS-BHCF014S
                ADD 1 TO GDA-TOT-ERROS
                SET GDA-FIM-SIM TO TRUE
           END-IF.

           OPEN OUTPUT BHCF014L.

           IF WS-FS-BHCF014L NOT = WS-FS-OK
                DISPLAY 'ERRO OPEN OUTPUT BHCF014L FS=' WS-FS-BHCF014L
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
                    PERFORM 400000-PREPARAR-CALL
                    PERFORM 410000-CHAMAR-SUBPROGRAMA
                    PERFORM 500000-TRATAR-RETORNO
                END-IF
           END-PERFORM.

      ******************************************************************
      *....LE UM REGISTRO DO ARQUIVO DE ENTRADA
      ******************************************************************
       310000-LER-PARTICIPANTE.
           READ BHCF012S
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
      *....CARREGA A AREA DE COMUNICACAO COM OS DADOS DO REGISTRO
      ******************************************************************
       400000-PREPARAR-CALL.
           MOVE IN-CODIGO    TO GDA-COM-CODIGO.
           MOVE IN-NOME      TO GDA-COM-NOME.
           MOVE IN-UF        TO GDA-COM-UF.
           MOVE IN-TRILHA    TO GDA-COM-TRILHA.
           MOVE IN-SITUACAO  TO GDA-COM-SITUACAO.
           MOVE IN-DATA      TO GDA-COM-DATA.
           MOVE SPACES       TO GDA-COM-STATUS
                                GDA-COM-COD-ERRO
                                GDA-COM-MENSAGEM.

      ******************************************************************
      *....CHAMA O SUBPROGRAMA DE VALIDACAO
      ******************************************************************
       410000-CHAMAR-SUBPROGRAMA.
           CALL WS-SUBPROGRAMA USING GDA-AREA-COMUNICACAO.

      ******************************************************************
      *....TRATA O RETORNO DO SUBPROGRAMA E DIRECIONA A GRAVACAO
     ******************************************************************
       500000-TRATAR-RETORNO.
           IF GDA-COM-STATUS = 'V'
               PERFORM 510000-GRAVAR-VALIDO
           ELSE
               PERFORM 520000-GRAVAR-LOG
           END-IF.

      ******************************************************************
      *....GRAVA O REGISTRO NO ARQUIVO DE VALIDOS
      ******************************************************************
       510000-GRAVAR-VALIDO.
           MOVE IN-CODIGO   TO OUT-CODIGO.
           MOVE IN-NOME     TO OUT-NOME.
           MOVE IN-UF       TO OUT-UF.
           MOVE IN-TRILHA   TO OUT-TRILHA.
           MOVE IN-SITUACAO TO OUT-SITUACAO.
           MOVE IN-DATA     TO OUT-DATA.

           WRITE REG-BHCF014S.

           IF WS-FS-BHCF014S = WS-FS-OK
               ADD 1 TO GDA-TOT-VALIDOS
           ELSE
               DISPLAY 'ERRO WRITE BHCF014S FS=' WS-FS-BHCF014S
               ADD 1 TO GDA-TOT-ERROS
           END-IF.

      ******************************************************************
      *....GRAVA O REGISTRO NO LOG DE REJEICOES
      ******************************************************************
       520000-GRAVAR-LOG.
           MOVE IN-CODIGO          TO LOG-CODIGO.
           MOVE IN-NOME            TO LOG-NOME.
           MOVE GDA-COM-COD-ERRO   TO LOG-COD-ERRO.
           MOVE GDA-COM-MENSAGEM   TO LOG-MENSAGEM.

           WRITE REG-BHCF014L.

           IF WS-FS-BHCF014L = WS-FS-OK
                ADD 1 TO GDA-TOT-REJEITADOS
           ELSE
                DISPLAY 'ERRO WRITE BHCF014L FS=' WS-FS-BHCF014L
                ADD 1 TO GDA-TOT-ERROS
           END-IF.

      ******************************************************************
      *....FECHA ARQUIVOS E EXIBE DATA/HORA DE FIM
      ******************************************************************
       900000-FECHAR-ARQUIVOS.
           CLOSE BHCF012S.
           CLOSE BHCF014S.
           CLOSE BHCF014L.

            MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.

            DISPLAY 'BHCP0014 - FIM DO PROCESSAMENTO'.
            DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.

      ******************************************************************
      *....EXIBE TOTALIZADORES NO SYSOUT
      ******************************************************************
       910000-EXIBIR-TOTALIZADORES.
           DISPLAY 'TOTAL DE REGISTROS LIDOS......: ' GDA-TOT-LIDOS.
           DISPLAY 'TOTAL DE REGISTROS VALIDOS....: ' GDA-TOT-VALIDOS.
           DISPLAY 'TOTAL DE REGISTROS REJEITADOS.: ' GDA-TOT-REJEITADOS.
           DISPLAY 'TOTAL DE ERROS DE ARQUIVO.....: ' GDA-TOT-ERROS.
