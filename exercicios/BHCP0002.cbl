******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0002
      * ANALISTA..: Tiago Assis Schulz
      * AUTOR.....: Tiago A. Schulz
      * DATA......: 25/06/2026
      * OBJETIVO..: REALIZAR UMA SOMA SIMPLES
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 25.06.26 Tiago A. Schulz       IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0002.

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

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
           01 GDA-NR-1          PIC 9(3) VALUE ZEROS.
           01 GDA-NR-2          PIC 9(3) VALUE ZEROS.
           01 GDA-NR-RESULTADO  PIC 9(4) VALUE ZEROS.
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

           MOVE 10 TO GDA-NR-1.
           MOVE 25 TO GDA-NR-2.

           ADD GDA-NR-1 TO GDA-NR-2 GIVING GDA-NR-RESULTADO.

           DISPLAY "Primeiro numero: " GDA-NR-1.
           DISPLAY "Segundo numero: " GDA-NR-2.
           DISPLAY "Resultado: " GDA-NR-RESULTADO.

           GOBACK.
      ******************************************************************
