******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0001
      * ANALISTA..: Tiago Assis Schulz
      * AUTOR.....: Tiago A. Schulz
      * DATA......: 25/06/2026
      * OBJETIVO..: DISPLAY HELLO WORLD
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 25.06.26 Tiago A. Schulz        IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0001.

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
       01  WS-ENTRADA                  PIC X(4).
      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
           PERFORM UNTIL WS-ENTRADA IS NUMERIC
             DISPLAY "Digite um numero: "
             ACCEPT WS-ENTRADA

           IF WS-ENTRADA IS NOT NUMERIC
              DISPLAY "Entrada invalida. Digite apenas numeros."
           END-IF
           END-PERFORM

           DISPLAY "Valor informado: " WS-ENTRADA.
           GOBACK.
      ******************************************************************
