      ******************************************************************
      * SIGLA.....: BHC � BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCS0001
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 30/06/2026
      * OBJETIVO..: SUBPROGRAMA DE EMISSAO DO RELATORIO DE CLIENTE
      * EXECUCAO..: COBOL - SUBPROGRAMA (CALLED)
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 30.06.26 TIAGO A. SCHULZ        IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCS0001.

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

       01  GDA-WS-TX-LINHA         PIC X(80).
       01  GDA-WS-NR-SALDO-FMT     PIC ZZZZZZ9,99.

       01  WS-CTE-CABECALHO.
           05 WS-CB-SIGLA             PIC X(025) VALUE
              'BHC - BOOTCAMP HACKATHON COBOL'.
           05 WS-CB-PROGRAMA          PIC X(008) VALUE 'BHCS0001'.
           05 WS-CB-ANALISTA          PIC X(020) VALUE 'JOSE HILARIO'.
           05 WS-CB-AUTOR             PIC X(020) VALUE
              'TIAGO ASSIS SCHULZ'.
           05 WS-CB-DATA-HORA         PIC X(021).
           05 WS-CB-OBJETIVO          PIC X(060) VALUE
              'SUBPROGRAMA DE EMISSAO DO RELATORIO DE CLIENTE'.
           05 WS-CB-EXECUCAO          PIC X(030) VALUE
              'COBOL - SUBPROGRAMA (CALLED)'.

      *----------------------------------------
       LINKAGE                         SECTION.
      *----------------------------------------
       01  LK-TX-CLIENTE           PIC X(15).
       01  LK-TX-TIPO-DESC         PIC X(10).
       01  LK-NR-SALDO             PIC 9(7)V99.
       01  LK-TX-STATUS            PIC X(10).

      ******************************************************************
       PROCEDURE DIVISION USING LK-TX-CLIENTE
                                LK-TX-TIPO-DESC
                                LK-NR-SALDO
                                LK-TX-STATUS.
      ******************************************************************
       000000-ROTINA-PRINCIPAL SECTION.
       000000-INICIO.
           MOVE FUNCTION CURRENT-DATE TO WS-CB-DATA-HORA
           PERFORM 1100-EXIBIR-CABECALHO
           PERFORM 1000-EXIBIR-RELATORIO.
       000000-FIM.
           EXIT PROGRAM.

      ******************************************************************
      *....SECTION PARA EXIBIR O CABECALHO DO PROGRAMA
      ******************************************************************
       1100-EXIBIR-CABECALHO SECTION.
       1100-INICIO.
           DISPLAY '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*'
           DISPLAY '* SIGLA.....: ' WS-CB-SIGLA
           DISPLAY '* PROGRAMA..: ' WS-CB-PROGRAMA
           DISPLAY '* ANALISTA...: ' WS-CB-ANALISTA
           DISPLAY '* AUTOR......: ' WS-CB-AUTOR
           DISPLAY '* DATA/HORA..: ' WS-CB-DATA-HORA
           DISPLAY '* OBJETIVO...: ' WS-CB-OBJETIVO
           DISPLAY '* EXECUCAO...: ' WS-CB-EXECUCAO
           DISPLAY '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*'
       1100-FIM.
           EXIT.

      ******************************************************************
      *....SECTION PARA EXIBIR O RELATORIO FORMATADO
      ******************************************************************
       1000-EXIBIR-RELATORIO SECTION.
       1000-INICIO.
           MOVE LK-NR-SALDO TO GDA-WS-NR-SALDO-FMT.

           DISPLAY"************************************".
           DISPLAY "RELATORIO DOS CLIENTES".

           MOVE SPACES TO GDA-WS-TX-LINHA.
           STRING "CLIENTE : " DELIMITED BY SIZE
             LK-TX-CLIENTE DELIMITED BY SIZE
               INTO GDA-WS-TX-LINHA.
           DISPLAY GDA-WS-TX-LINHA.

           MOVE SPACES TO GDA-WS-TX-LINHA.
           STRING "TIPO    : " DELIMITED BY SIZE
             LK-TX-TIPO-DESC DELIMITED BY SIZE
               INTO GDA-WS-TX-LINHA.
           DISPLAY GDA-WS-TX-LINHA.

           MOVE SPACES TO GDA-WS-TX-LINHA.
           STRING "SALDO   : " DELIMITED BY SIZE
             GDA-WS-NR-SALDO-FMT DELIMITED BY SIZE
               INTO GDA-WS-TX-LINHA.
           DISPLAY GDA-WS-TX-LINHA.

           MOVE SPACES TO GDA-WS-TX-LINHA.
           STRING "STATUS  : " DELIMITED BY SIZE
              LK-TX-STATUS DELIMITED BY SIZE
               INTO GDA-WS-TX-LINHA.
           DISPLAY GDA-WS-TX-LINHA.

           DISPLAY "************************************".
       1000-FIM.
           EXIT.
