      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0003
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 27/06/2026
      * OBJETIVO..: MOVIMENTACAO SIMPLES
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

       PROGRAM-ID. BHCP0003.

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

       01  GDA-WS-TX-NM                     PIC  X(50).
       01  GDA-WS-TX-CR                     PIC  X(50).
       01  GDA-WS-TX-MSG                    PIC  X(100).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
           PERFORM 1000-GET-INFO.
           PERFORM 2000-DISPLAY-INFO.
           PERFORM 3000-END-PROGRAM.

      ******************************************************************
      *....DECLARACAO DINAMICA DAS VARIAVEIS
      ******************************************************************
       1000-GET-INFO.
           DISPLAY "QUAL O NOME DO ALUNO?".
           ACCEPT GDA-WS-TX-NM.

           DISPLAY "QUAL É O CURSO?".
           ACCEPT GDA-WS-TX-CR.

           DISPLAY "QUAL A MENSAGEM A SER PASSADA?".
           ACCEPT GDA-WS-TX-MSG.

      ******************************************************************
      *....EXIBICAO DAS INFORMAÇÕES
      ******************************************************************
       2000-DISPLAY-INFO.
           DISPLAY "ALUNO: " GDA-WS-TX-NM.
           DISPLAY "CURSO: " GDA-WS-TX-CR.
           DISPLAY "MENSAGEM: " GDA-WS-TX-MSG.

      ******************************************************************
      *....FINALIZAÇÃO DO PROGRAMA
      ******************************************************************
       3000-END-PROGRAM.
           GOBACK.
      ******************************************************************
