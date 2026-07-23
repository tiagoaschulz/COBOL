      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0007
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 28/06/2026
      * OBJETIVO..: TIPO DE CONTA COM EVALUATE
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

       PROGRAM-ID. BHCP0007.

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

       01  GDA-WS-TB-CONTA.
         05 GDA-WS-TB-ITEM OCCURS 4 TIMES.
            10 GDA-WS-NR-TP   PIC 9(1).
            10 GDA-WS-TX-DSC   PIC X(20).

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01  GDA-LS-NR-TP-INPUT                PIC 9(2).
       01  GDA-LS-TX-DSC-OUTPUT               PIC X(20).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
           PERFORM 1000-LOAD-TABLE-DATA.
           PERFORM 2000-GET-DATA.
           PERFORM 3000-VERIFY-DATA.
           PERFORM 4000-DISPLAY-DATA.
           PERFORM 5000-END-PROGRAM.

      ******************************************************************
      *....ATRIBUI OS VALORES DA TABELA REFERENTE A CADA TIPO DE CONTA
      ******************************************************************
       1000-LOAD-TABLE-DATA.
           MOVE 1 TO GDA-WS-NR-TP(1).
           MOVE "CONTA CORRENTE" TO GDA-WS-TX-DSC(1).

           MOVE 2 TO GDA-WS-NR-TP(2).
           MOVE "CONTA POUPANCA" TO GDA-WS-TX-DSC(2).

           MOVE 3 TO GDA-WS-NR-TP(3).
           MOVE "CONTA SALARIO" TO GDA-WS-TX-DSC(3).

           MOVE 4 TO GDA-WS-NR-TP(4).
           MOVE "CONTA EMPRESARIAL" TO GDA-WS-TX-DSC(4).

      ******************************************************************
      *....RECEBE O VALOR DO TIPO DA CONTA
      ******************************************************************
       2000-GET-DATA.
           DISPLAY "QUAL O TIPO DA SUA CONTA? (1 A 4)".
           DISPLAY "1 - CONTA CORRENTE".
           DISPLAY "2 - CONTA POUPANCA".
           DISPLAY "3 - CONTA SALARIO".
           DISPLAY "4 - CONTA EMPRESARIAL".
           ACCEPT GDA-LS-NR-TP-INPUT.

      ******************************************************************
      *....VERIFICA O TIPO DA CONTA
      ******************************************************************
       3000-VERIFY-DATA.
           EVALUATE GDA-LS-NR-TP-INPUT
             WHEN 1
               MOVE GDA-WS-TX-DSC(1) TO GDA-LS-TX-DSC-OUTPUT
             WHEN 2
               MOVE GDA-WS-TX-DSC(2) TO GDA-LS-TX-DSC-OUTPUT
             WHEN 3
               MOVE GDA-WS-TX-DSC(3) TO GDA-LS-TX-DSC-OUTPUT
             WHEN 4
               MOVE GDA-WS-TX-DSC(4) TO GDA-LS-TX-DSC-OUTPUT
             WHEN OTHER
               MOVE "TIPO INVALIDO" TO GDA-LS-TX-DSC-OUTPUT
           END-EVALUATE.

      ******************************************************************
      *....EXIBICAO DAS INFORMACOES
      ******************************************************************
       4000-DISPLAY-DATA.
           DISPLAY SPACES.
           DISPLAY "CODIGO INFORMADO: " GDA-LS-NR-TP-INPUT.
           DISPLAY "DESCRICAO: " GDA-LS-TX-DSC-OUTPUT.
           DISPLAY SPACES.

      ******************************************************************
      *....FINALIZAÇÃO DO PROGRAMA
      ******************************************************************
       5000-END-PROGRAM.
           GOBACK.
      ******************************************************************
