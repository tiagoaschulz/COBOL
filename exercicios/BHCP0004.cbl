      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0004
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 27/06/2026
      * OBJETIVO..: CADASTRO SIMPLES DE CLIENTE
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

       PROGRAM-ID. BHCP0004.

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
       01  GDA-LS-NR-CD              PIC 9(4).
       01  GDA-LS-NR-AG                PIC X(6).
       01  GDA-LS-NR-ACCT              PIC X(5).
       01  GDA-LS-NR-SDI               PIC 9(5)V99.
       01  GDA-LS-TX-NM              PIC X(30).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
           PERFORM 1000-SET-DATA.
           PERFORM 2000-DISPLAY-DATA.
           PERFORM 3000-END-PROGRAM.

      ******************************************************************
      *....DECLARANDO O VALOR DAS VARIAVEIS
      ******************************************************************
       1000-SET-DATA.
           MOVE 'Tiago Assis Schulz' TO GDA-LS-TX-NM.
           MOVE '9876-5' TO GDA-LS-NR-AG.
           MOVE '999-8' TO GDA-LS-NR-ACCT.
           MOVE 1111 TO GDA-LS-NR-CD.
           MOVE 8000 TO GDA-LS-NR-SDI.

      ******************************************************************
      *....EXIBICAO DAS INFORMACOES
      ******************************************************************
       2000-DISPLAY-DATA.
           DISPLAY "Cadastro do cliente:".
           DISPLAY "Codigo: " GDA-LS-NR-CD.
           DISPLAY "Nome: " GDA-LS-TX-NM.
           DISPLAY "Agencia: " GDA-LS-NR-AG.
           DISPLAY "Conta: " GDA-LS-NR-ACCT.
           DISPLAY "Saldo Inicial: R$ " GDA-LS-NR-SDI.

      ******************************************************************
      *....FINALIZAÇÃO DO PROGRAMA
      ******************************************************************
       3000-END-PROGRAM.
           GOBACK.
      ******************************************************************
