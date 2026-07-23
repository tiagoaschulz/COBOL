      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0006
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 27/06/2026
      * OBJETIVO..: CLASSIFICACAO DE SALDO COM IF
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

       PROGRAM-ID. BHCP0006.

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

       01  GDA-LS-TX-CT               PIC X(50) VALUE 'Tiago Schulz'.
       01  GDA-LS-TX-ST                PIC X(10).
       01  GDA-LS-NR-SD                PIC S9(5)V99.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
           PERFORM 1000-VALIDATE-DATA.
           PERFORM 2000-DISPLAY-DATA.
           PERFORM 3000-END-PROGRAM.

      ******************************************************************
      *....VALIDAÇÃO DO SALDO
      ******************************************************************
       1000-VALIDATE-DATA.
           DISPLAY "QUAL O VALOR DO SALDO?"
           ACCEPT GDA-LS-NR-SD.

      *....USO DE GREATER THAN OU > E EQUALS TO OU = NÃO MUDA A SAIDA
           IF GDA-LS-NR-SD GREATER THAN 0
               MOVE "POSITIVO" TO GDA-LS-TX-ST
           ELSE IF GDA-LS-NR-SD EQUALS TO 0
               MOVE "ZERADO" TO GDA-LS-TX-ST
           ELSE
               MOVE "NEGATIVO" TO GDA-LS-TX-ST
           END-IF.

      ******************************************************************
      *....EXIBICAO DAS INFORMACOES
      ******************************************************************
       2000-DISPLAY-DATA.
           DISPLAY "CLIENTE: " GDA-LS-TX-CT.
           DISPLAY "SALDO: R$ " GDA-LS-NR-SD.
           DISPLAY "SITUACAO: " GDA-LS-TX-ST.

      ******************************************************************
      *....FINALIZAÇÃO DO PROGRAMA
      ******************************************************************
       3000-END-PROGRAM.
           GOBACK.
      ******************************************************************
