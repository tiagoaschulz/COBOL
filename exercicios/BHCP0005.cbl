      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0005
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 27/06/2026
      * OBJETIVO..: CALCULO DE MOVIMENTACAO BANCARIA
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 27.06.26 TIAGO A. SCHULZ        IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0005.

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
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01  GDA-LS-NR-SDI                PIC 9(5)V99 VALUE 5000,00.
       01  GDA-LS-NR-DP                PIC 9(5)V99 VALUE 300,00.
       01  GDA-LS-NR-SQ                PIC 9(5)V99 VALUE 200,00.
       01  GDA-LS-NR-SDF                PIC 9(5)V99 VALUE ZEROS.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
           PERFORM 1000-CALCULATE-DATA.
           PERFORM 2000-DISPLAY-DATA.
           PERFORM 3000-END-PROGRAM.

      ******************************************************************
      *....PROCESSA O CALCULO
      ******************************************************************
       1000-CALCULATE-DATA.
           ADD GDA-LS-NR-SDI GDA-LS-NR-DP,
               GIVING GDA-LS-NR-SDF.
           SUBTRACT GDA-LS-NR-SQ FROM GDA-LS-NR-SDF.

      ******************************************************************
      *....EXIBICAO DAS INFORMACOES
      ******************************************************************
       2000-DISPLAY-DATA.
           DISPLAY "SALDO INICIAL: R$ " GDA-LS-NR-SDI.
           DISPLAY "DEPOSITO: R$ " GDA-LS-NR-DP.
           DISPLAY "SAQUE: R$ " GDA-LS-NR-SQ.
           DISPLAY "SALDO FINAL: R$ " GDA-LS-NR-SDF.

      ******************************************************************
      *....FINALIZAÇÃO DO PROGRAMA
      ******************************************************************
       3000-END-PROGRAM.
           GOBACK.
      ******************************************************************
