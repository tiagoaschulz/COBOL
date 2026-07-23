      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0008
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 28/06/2026
      * OBJETIVO..: SIMULACAO DE PROCESSAMENTO COM PERFORM
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 28.06.26 TIAGO A. SCHULZ        IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0008.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************

      *----------------------------------------
       CONFIGURATION                   SECTION.
      *----------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************

      *----------------------------------------
       WORKING-STORAGE                 SECTION.
      *----------------------------------------
       01  GDA-WS-VENDAS.
           05 GDA-WS-NR-VDA        PIC 9(5)V99 OCCURS 5 TIMES.

       01  GDA-WS-NR-TOT-VENDAS    PIC 9(7)V99.
       01  GDA-WS-NR-CONTADOR      PIC 9(2).
       01  GDA-WS-NR-MEDIA         PIC 9(7)V99.
       01  GDA-WS-NR-META          PIC 9(5)V99.
       01  GDA-WS-TX-STATUS        PIC X(20).

       01  GDA-WS-NR-IDX           PIC 9(2).

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01 GDA-LS-NR-VENDA-INPUT    PIC 9(5)V99.
       01 GDA-LS-NR-META-INPUT     PIC 9(5)V99.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
           PERFORM 1000-GET-DATA.
           PERFORM 2000-PROCESS-DATA.
           PERFORM 3000-CALCULATE-DATA.
           PERFORM 4000-EVALUATE-DATA.
           PERFORM 5000-DISPLAY-DATA.
           PERFORM 6000-END-PROGRAM.

      ******************************************************************
      *....RECEBE O VALOR DA META
      ******************************************************************
       1000-GET-DATA.
           MOVE 1 TO GDA-WS-NR-IDX.

           PERFORM 1100-ACCEPT-DATA
             UNTIL GDA-WS-NR-IDX > 5.

           DISPLAY "QUAL O VALOR DA META ESTABELECIDA?".
           ACCEPT GDA-LS-NR-META-INPUT.
           MOVE GDA-LS-NR-META-INPUT TO GDA-WS-NR-META.

      ******************************************************************
      *....RECEBE OS VALORES DAS VENDAS
      ******************************************************************
       1100-ACCEPT-DATA.
           DISPLAY "QUAL FOI O VALOR DA VENDA " GDA-WS-NR-IDX "? "
           ACCEPT GDA-LS-NR-VENDA-INPUT.

           MOVE GDA-LS-NR-VENDA-INPUT TO GDA-WS-NR-VDA(GDA-WS-NR-IDX).
           ADD 1 TO GDA-WS-NR-IDX.

      ******************************************************************
      *....SOMA A QUANTIDADE DE VENDAS
      ******************************************************************
       2000-PROCESS-DATA.
           MOVE 1 TO GDA-WS-NR-IDX.

           PERFORM 2100-ACCUMULATE-VENDAS
             UNTIL GDA-WS-NR-IDX > 5.

      ******************************************************************
      *....GUARDA A CONTAGEM DAS VENDAS
      ******************************************************************
       2100-ACCUMULATE-VENDAS.
           ADD GDA-WS-NR-VDA(GDA-WS-NR-IDX) TO GDA-WS-NR-TOT-VENDAS.
           ADD 1 TO GDA-WS-NR-CONTADOR.
           ADD 1 TO GDA-WS-NR-IDX.

      ******************************************************************
      *....CALCULA A MEDIA DAS VENDAS
      ******************************************************************
       3000-CALCULATE-DATA.
           COMPUTE GDA-WS-NR-MEDIA  =
             GDA-WS-NR-TOT-VENDAS / GDA-WS-NR-CONTADOR.

      ******************************************************************
      *....AVALIA SE A META FOI ATINGIDA
      ******************************************************************
       4000-EVALUATE-DATA.
           IF GDA-WS-NR-TOT-VENDAS >= GDA-WS-NR-META
             MOVE "META ATINGIDA" TO GDA-WS-TX-STATUS
           ELSE
             MOVE "META NAO ATINGIDA" TO GDA-WS-TX-STATUS
           END-IF.

      ******************************************************************
      *....EXIBICAO DAS INFORMACOES
      ******************************************************************
       5000-DISPLAY-DATA.
           DISPLAY SPACES.
           DISPLAY "RESUMO DE VENDAS".
           DISPLAY "QUANTIDADE: " GDA-WS-NR-CONTADOR.
           DISPLAY "TOTAL: " GDA-WS-NR-TOT-VENDAS.
           DISPLAY "MEDIA: " GDA-WS-NR-MEDIA.
           DISPLAY "SITUACAO: " GDA-WS-TX-STATUS.
           DISPLAY SPACES.

      ******************************************************************
      *....FINALIZAÇÃO DO PROGRAMA
      ******************************************************************
       6000-END-PROGRAM.
           GOBACK.
      ******************************************************************
