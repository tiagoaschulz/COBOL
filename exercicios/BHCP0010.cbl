      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0010
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 30/06/2026
      * OBJETIVO..: CADASTRO DE PRODUTOS
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

       PROGRAM-ID. BHCP0010.

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
       01 GDA-WS-NR-QTD-PRODUTOS   PIC 9(3) VALUE ZEROS.

       01 GDA-WS-TB-PRODUTOS.
           05 GDA-WS-TB-PROD OCCURS 1 TO 100 TIMES
                       DEPENDING ON GDA-WS-NR-QTD-PRODUTOS.
             10 GDA-WS-NR-CD          PIC 9(3).
             10 GDA-WS-TX-DESC        PIC X(15).
             10 GDA-WS-NR-PRECO       PIC 9(5)V99.
             10 FILLER                PIC X(05).

       01 GDA-WS-NR-PRECO-FMT      PIC ZZZZ9,99.
       01 GDA-WS-NR-IDX            PIC 9(3).
       01 GDA-WS-TX-CATEGORIA      PIC X(10).
       01 GDA-WS-TX-LINHA          PIC X(80).

       01 GDA-WS-TX-CONTINUAR      PIC X(01).
           88 GDA-WS-CONTINUAR-SIM     VALUES "S", "s".
           88 GDA-WS-CONTINUAR-NAO     VALUES "N", "n".

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01 GDA-LS-NR-CD-INPUT       PIC 9(3).
       01 GDA-LS-TX-DESC-INPUT     PIC X(15).
       01 GDA-LS-NR-PRECO-INPUT    PIC 9(5)V99.
       01 GDA-LS-TX-RESP-INPUT     PIC X(01).


      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
       000000-INICIO.
           PERFORM 1000-CADASTRAR-PROD.
           PERFORM 2000-GERAR-RELATORIO.
           PERFORM 3000-END-PROGRAM.
       000000-FIM.
           EXIT.

      *....SECTION PARA CADASTRO DO PRODUTO
       1000-CADASTRAR-PROD SECTION.
       1000-INICIO.
           MOVE "S" TO GDA-WS-TX-CONTINUAR.

           PERFORM 1100-ACCEPT-PROD
             UNTIL GDA-WS-CONTINUAR-NAO
             OR GDA-WS-NR-QTD-PRODUTOS = 100.
       1000-FIM.
           EXIT.

      *....SECTION PARA INSERIR DADOS DO PRODUTO
       1100-ACCEPT-PROD  SECTION.
       1100-INICIO.

           ADD 1 TO GDA-WS-NR-QTD-PRODUTOS.

           DISPLAY ' '.
           DISPLAY "CADASTRO DO PRODUTO " GDA-WS-NR-QTD-PRODUTOS.

           DISPLAY "CODIGO: ".
           ACCEPT GDA-LS-NR-CD-INPUT.

           MOVE GDA-LS-NR-CD-INPUT
             TO GDA-WS-NR-CD(GDA-WS-NR-QTD-PRODUTOS).

           DISPLAY "DESCRICAO: ".
           ACCEPT GDA-LS-TX-DESC-INPUT.

           MOVE GDA-LS-TX-DESC-INPUT
             TO GDA-WS-TX-DESC(GDA-WS-NR-QTD-PRODUTOS).

           DISPLAY "PRECO: ".
           ACCEPT GDA-LS-NR-PRECO-INPUT.

           MOVE GDA-LS-NR-PRECO-INPUT
             TO GDA-WS-NR-PRECO(GDA-WS-NR-QTD-PRODUTOS).

           DISPLAY "CADASTRAR OUTRO PRODUTO? (S/N)".
           ACCEPT GDA-LS-TX-RESP-INPUT.
           MOVE GDA-LS-TX-RESP-INPUT TO GDA-WS-TX-CONTINUAR.
       1100-FIM.
           EXIT.


      ******************************************************************
      *....SECTION PARA GERAR O RELATORIO
      ******************************************************************
       2000-GERAR-RELATORIO  SECTION.
       2000-INICIO.
           DISPLAY ' '.
           DISPLAY "RELATORIO DE PRODUTOS".
           DISPLAY ' '.

           MOVE 1 TO GDA-WS-NR-IDX.

           PERFORM 2100-PROCESSAR-STR
             UNTIL GDA-WS-NR-IDX > GDA-WS-NR-QTD-PRODUTOS.
       2000-FIM.
           EXIT.

      *....SECTION PARA PROCESSAR AS LINHAS DO RELATORIO
       2100-PROCESSAR-STR  SECTION.
       2100-INICIO.
           PERFORM 2200-CLASSIFICAR-PRECO.
           PERFORM 2300-MONTAR-LINHA.

           DISPLAY GDA-WS-TX-LINHA.
           ADD 1 TO GDA-WS-NR-IDX.
       2100-FIM.
           EXIT.

      *....SECTION PARA CLASSIFICAR OS PRECOS
       2200-CLASSIFICAR-PRECO  SECTION.
       2200-INICIO.
           EVALUATE TRUE
              WHEN GDA-WS-NR-PRECO(GDA-WS-NR-IDX) NOT GREATER THAN 99,99
                 MOVE "BARATO" TO GDA-WS-TX-CATEGORIA
              WHEN GDA-WS-NR-PRECO(GDA-WS-NR-IDX) NOT LESS THAN 100
                AND GDA-WS-NR-PRECO(GDA-WS-NR-IDX) NOT GREATER THAN 500
                 MOVE "NORMAL" TO GDA-WS-TX-CATEGORIA
              WHEN GDA-WS-NR-PRECO(GDA-WS-NR-IDX) NOT EQUAL TO 0
                AND GDA-WS-NR-PRECO(GDA-WS-NR-IDX) GREATER THAN 500
                 MOVE "CARO"   TO GDA-WS-TX-CATEGORIA
              WHEN OTHER
                 MOVE "INVALIDO" TO GDA-WS-TX-CATEGORIA
           END-EVALUATE.
       2200-FIM.
           EXIT.


      *....SECTION PARA MONTAR AS LINHAS DO RELATORIO
       2300-MONTAR-LINHA  SECTION.
       2300-INICIO.
           MOVE GDA-WS-NR-PRECO(GDA-WS-NR-IDX)
             TO GDA-WS-NR-PRECO-FMT.

           MOVE SPACES TO GDA-WS-TX-LINHA.

           STRING GDA-WS-NR-CD(GDA-WS-NR-IDX)    DELIMITED BY SIZE
                  " "                            DELIMITED BY SIZE
                  GDA-WS-TX-DESC(GDA-WS-NR-IDX)  DELIMITED BY SIZE
                  " "                            DELIMITED BY SIZE
                  GDA-WS-NR-PRECO-FMT            DELIMITED BY SIZE
                  " "                            DELIMITED BY SIZE
                  GDA-WS-TX-CATEGORIA            DELIMITED BY SIZE
                INTO GDA-WS-TX-LINHA.
       2300-FIM.
           EXIT.

      ******************************************************************
      *....FINALIZAÇÃO DO PROGRAMA
      ******************************************************************
       3000-END-PROGRAM  SECTION.
           GOBACK.
      ******************************************************************
