      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0011
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 30/06/2026
      * OBJETIVO..: SISTEMA BANCARIO COM SUBPROGRAMA
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 30.06.26 TIAGO A. SCHULZ        IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0011.

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
       01 GDA-WS-NR-QTD-CONTAS   PIC 9(2) VALUE ZEROS.

       01 GDA-WS-TB-CONTAS.
           05 GDA-WS-TB-CTA OCCURS 1 TO 50 TIMES
                       DEPENDING ON GDA-WS-NR-QTD-CONTAS.
             10 GDA-WS-NR-CONTA              PIC 9(3).
             10 GDA-WS-TX-CLIENTE            PIC X(15).
             10 GDA-WS-TX-TIPO               PIC X(01).
               88 GDA-WS-TX-TIPO-CORRENTE    VALUE "C".
               88 GDA-WS-TX-TIPO-POUPANCA    VALUE "P".
             10 GDA-WS-NR-SALDO              PIC 9(7)V99.
             10 FILLER                       PIC X(05).

       01 GDA-WS-NR-IDX            PIC 9(2).
       01 GDA-WS-TX-STATUS         PIC X(10).
       01 GDA-WS-TIPO-DESC         PIC X(10).

       01 GDA-WS-TX-CONTINUAR      PIC X(01).
           88 GDA-WS-CONTINUAR-SIM     VALUES "S", "s".
           88 GDA-WS-CONTINUAR-NAO     VALUES "N", "n".

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01 GDA-LS-NR-CONTA-INPUT    PIC 9(5).
       01 GDA-LS-TX-CLIENTE-INPUT  PIC X(15).
       01 GDA-LS-TX-TIPO-INPUT     PIC X(01).
       01 GDA-LS-NR-SALDO-INPUT    PIC 9(7)V99.
       01 GDA-LS-TX-RESP-INPUT     PIC X(01).


      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
       000000-INICIO.
           PERFORM 1000-CADASTRAR-CONTAS.
           PERFORM 2000-PROCESSAR-RELATORIO.
           PERFORM 3000-END-PROGRAM.
       000000-FIM.
           EXIT.

      *....SECTION PARA CADASTRO DA CONTA
       1000-CADASTRAR-CONTAS SECTION.
       1000-INICIO.
           MOVE "S" TO GDA-WS-TX-CONTINUAR.

           PERFORM 1100-ACCEPT-CONTA
             UNTIL GDA-WS-CONTINUAR-NAO
             OR GDA-WS-NR-QTD-CONTAS = 50.
       1000-FIM.
           EXIT.

      *....SECTION PARA INSERIR DADOS DA CONTA
       1100-ACCEPT-CONTA  SECTION.
       1100-INICIO.

           ADD 1 TO GDA-WS-NR-QTD-CONTAS.

           DISPLAY ' '.
           DISPLAY "CADASTRO DA CONTA " GDA-WS-NR-QTD-CONTAS.

           DISPLAY "NUMERO DA CONTA: ".
           ACCEPT GDA-LS-NR-CONTA-INPUT.

           MOVE GDA-LS-NR-CONTA-INPUT
             TO GDA-WS-NR-CONTA(GDA-WS-NR-QTD-CONTAS).

           DISPLAY "CLIENTE: ".
           ACCEPT GDA-LS-TX-CLIENTE-INPUT.

           MOVE GDA-LS-TX-CLIENTE-INPUT
             TO GDA-WS-TX-CLIENTE(GDA-WS-NR-QTD-CONTAS).

           DISPLAY "TIPO (C-CORRENTE/ P-POUPANCA): ".
           ACCEPT GDA-LS-TX-TIPO-INPUT.

           MOVE GDA-LS-TX-TIPO-INPUT
             TO GDA-WS-TX-TIPO(GDA-WS-NR-QTD-CONTAS).

           DISPLAY "SALDO: ".
           ACCEPT GDA-LS-NR-SALDO-INPUT.

           MOVE GDA-LS-NR-SALDO-INPUT
             TO GDA-WS-NR-SALDO(GDA-WS-NR-QTD-CONTAS).

           DISPLAY "CADASTRAR OUTRA CONTA? (S/N)".
           ACCEPT GDA-LS-TX-RESP-INPUT.
           MOVE GDA-LS-TX-RESP-INPUT TO GDA-WS-TX-CONTINUAR.
       1100-FIM.
           EXIT.


      ******************************************************************
      *....SECTION PARA GERAR O RELATORIO
      ******************************************************************
       2000-PROCESSAR-RELATORIO  SECTION.
       2000-INICIO.
           MOVE 1 TO GDA-WS-NR-IDX.

           PERFORM 2100-PROCESSAR-CONTA
             UNTIL GDA-WS-NR-IDX > GDA-WS-NR-QTD-CONTAS.
       2000-FIM.
           EXIT.

      *....SECTION PARA PROCESSAR AS LINHAS DO RELATORIO
       2100-PROCESSAR-CONTA  SECTION.
       2100-INICIO.
           PERFORM 2200-CLASSIFICAR-SALDO.
           PERFORM 2300-TRADUZIR-TIPO.

           CALL "BHCS0001" USING
             GDA-WS-TX-CLIENTE(GDA-WS-NR-IDX)
             GDA-WS-TIPO-DESC
             GDA-WS-NR-SALDO(GDA-WS-NR-IDX)
             GDA-WS-TX-STATUS.

           ADD 1 TO GDA-WS-NR-IDX.
       2100-FIM.
           EXIT.

      *....SECTION PARA CLASSIFICAR OS PRECOS
       2200-CLASSIFICAR-SALDO  SECTION.
       2200-INICIO.
            EVALUATE TRUE
           WHEN GDA-WS-NR-SALDO(GDA-WS-NR-IDX) GREATER THAN 10000
              MOVE "VIP"      TO GDA-WS-TX-STATUS
           WHEN GDA-WS-NR-SALDO(GDA-WS-NR-IDX) EQUAL TO 10000
              MOVE "ESPECIAL" TO GDA-WS-TX-STATUS
           WHEN OTHER
              MOVE "PADRAO"   TO GDA-WS-TX-STATUS
           END-EVALUATE.
       2200-FIM.
           EXIT.


      *....SECTION PARA MONTAR AS LINHAS DO RELATORIO
       2300-TRADUZIR-TIPO  SECTION.
       2300-INICIO.
           EVALUATE TRUE
           WHEN GDA-WS-TX-TIPO-CORRENTE(GDA-WS-NR-IDX)
             MOVE "CORRENTE" TO GDA-WS-TIPO-DESC
           WHEN GDA-WS-TX-TIPO-POUPANCA(GDA-WS-NR-IDX)
              MOVE "POUPANCA" TO GDA-WS-TIPO-DESC
           WHEN OTHER
             MOVE "INVALIDO" TO GDA-WS-TIPO-DESC
           END-EVALUATE.
       2300-FIM.
           EXIT.

      ******************************************************************
      *....FINALIZAÇÃO DO PROGRAMA
      ******************************************************************
       3000-END-PROGRAM  SECTION.
           GOBACK.
      ******************************************************************
