      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0009
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 29/06/2026
      * OBJETIVO..: CADASTRO DE FUNCIONARIOS
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 29.06.26 TIAGO A. SCHULZ        IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0009.

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
       01  GDA-WS-TB-FUNCIONARIOS.
          05 GDA-WS-TB-FUNC OCCURS 10 TIMES.
             10 GDA-WS-NR-CD               PIC 9(4).
             10 GDA-WS-TX-NM               PIC X(15).
             10 GDA-WS-NR-SALARIO          PIC 9(5)V99.
                88 GDA-WS-SAL-ALTO         VALUE 5000,01 THRU 99999,99.
                88 GDA-WS-SAL-LIMITE       VALUE 5000,00.
                88 GDA-WS-SAL-NORMAL       VALUE 0 THRU 4999,99.
             10 GDA-WS-SALARIO-EDIT REDEFINES GDA-WS-NR-SALARIO
                                                        PIC X(07).
             10 FILLER                     PIC X(05).


       01  GDA-WS-NR-SALARIO-FMT    PIC ZZZZ9,99.
       01  GDA-WS-NR-IDX            PIC 9(2).
       01  GDA-WS-TX-SITUACAO       PIC X(10).
       01  GDA-WS-TX-LINHA          PIC X(80).

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01  GDA-LS-NR-CD-INPUT          PIC 9(4).
       01  GDA-LS-TX-NM-INPUT          PIC X(20).
       01  GDA-LS-NR-SALARIO-INPUT     PIC 9(5)V99.


      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
       000000-INICIO.
           DISPLAY "OBS: SO 3 FUNC. SERA CADASTRADO P/FACILITAR".
           DISPLAY "NO EXERC. FOI LIMITADO ATE 10".
           DISPLAY "MUDAR NO INDEX SE DESEJAR NA LINHA 87 E 132".
           PERFORM 1000-CADASTRAR-FUNC.
           PERFORM 2000-GERAR-RELATORIO.
           PERFORM 3000-END-PROGRAM.
       000000-FIM.
           EXIT.

      ******************************************************************
      *....SECTION DE CADASTRO DO FUNCIONARIO
      ******************************************************************
       1000-CADASTRAR-FUNC SECTION.
       1000-INICIO.
           MOVE 1 TO GDA-WS-NR-IDX.

           PERFORM 1100-ACCEPT-FUNC
             UNTIL GDA-WS-NR-IDX > 3.
       1000-FIM.
           EXIT.

      *....SECTION PARA INSERIR DADOS DO CADASTRO
       1100-ACCEPT-FUNC  SECTION.
       1100-INICIO.
           DISPLAY ' '.
           DISPLAY "CADASTRO DO FUNCIONARIO " GDA-WS-NR-IDX.

           DISPLAY "CODIGO: ".
           ACCEPT GDA-LS-NR-CD-INPUT.

           MOVE GDA-LS-NR-CD-INPUT
             TO GDA-WS-NR-CD(GDA-WS-NR-IDX).

           DISPLAY "NOME: ".
           ACCEPT GDA-LS-TX-NM-INPUT.

           MOVE GDA-LS-TX-NM-INPUT
             TO GDA-WS-TX-NM(GDA-WS-NR-IDX).

           DISPLAY "SALARIO: ".
           ACCEPT GDA-LS-NR-SALARIO-INPUT.

           MOVE GDA-LS-NR-SALARIO-INPUT
             TO GDA-WS-NR-SALARIO(GDA-WS-NR-IDX).

           ADD 1 TO GDA-WS-NR-IDX.
       1100-FIM.
           EXIT.


      ******************************************************************
      *....SECTION PARA GERAR O RELATORIO
      ******************************************************************
       2000-GERAR-RELATORIO  SECTION.
       2000-INICIO.
           DISPLAY ' '.
           DISPLAY "RELATORIO DOS FUNCIONARIOS".
           DISPLAY ' '.

           MOVE 1 TO GDA-WS-NR-IDX.

           PERFORM 2100-PROCESSAR-STR
             UNTIL GDA-WS-NR-IDX > 3.
       2000-FIM.
           EXIT.

      *....SECTION PARA PROCESSAR AS LINHAS DO RELATORIO
       2100-PROCESSAR-STR  SECTION.
       2100-INICIO.
           PERFORM 2200-CLASSIFICAR-SALARIO.
           PERFORM 2300-MONTAR-LINHA.

           DISPLAY GDA-WS-TX-LINHA.
           ADD 1 TO GDA-WS-NR-IDX.
       2100-FIM.
           EXIT.

      *....SECTION PARA CLASSIFICAR OS SALARIOS
       2200-CLASSIFICAR-SALARIO  SECTION.
       2200-INICIO.
           IF GDA-WS-SAL-ALTO(GDA-WS-NR-IDX)
             MOVE "ALTO" TO  GDA-WS-TX-SITUACAO
           END-IF.

           IF GDA-WS-SAL-LIMITE(GDA-WS-NR-IDX)
             MOVE "LIMITE" TO  GDA-WS-TX-SITUACAO
           END-IF.

           IF GDA-WS-SAL-NORMAL(GDA-WS-NR-IDX)
             MOVE "NORMAL" TO  GDA-WS-TX-SITUACAO
           END-IF.
       2200-FIM.
           EXIT.


      *....SECTION PARA MONTAR AS LINHAS DO RELATORIO
       2300-MONTAR-LINHA  SECTION.
       2300-INICIO.
           MOVE GDA-WS-NR-SALARIO(GDA-WS-NR-IDX)
             TO GDA-WS-NR-SALARIO-FMT.

           MOVE SPACES TO GDA-WS-TX-LINHA.

           STRING GDA-WS-NR-CD(GDA-WS-NR-IDX)  DELIMITED BY SIZE
                  " "                          DELIMITED BY SIZE
                  GDA-WS-TX-NM(GDA-WS-NR-IDX)  DELIMITED BY SIZE
                  " "                          DELIMITED BY SIZE
                  GDA-WS-NR-SALARIO-FMT        DELIMITED BY SIZE
                  " "                          DELIMITED BY SIZE
                  GDA-WS-TX-SITUACAO           DELIMITED BY SIZE
                INTO GDA-WS-TX-LINHA.
       2300-FIM.
           EXIT.

      ******************************************************************
      *....FINALIZAÇÃO DO PROGRAMA
      ******************************************************************
       3000-END-PROGRAM  SECTION.
           GOBACK.
      ******************************************************************
