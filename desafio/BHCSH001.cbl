      ******************************************************************
      * SIGLA.....: BHC - BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCSH001
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 04/07/2026
      * OBJETIVO..: VALIDAR TRANSACAO FINANCEIRA
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 04.07.26 TIAGO A. SCHULZ        IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCSH001.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------

       01  WS-CTE-CABECALHO.
           05 WS-CB-SIGLA             PIC X(030) VALUE
              'BHC - BOOTCAMP HACKATHON COBOL'.
           05 WS-CB-PROGRAMA          PIC X(008) VALUE 'BHCSH001'.
           05 WS-CB-ANALISTA          PIC X(020) VALUE 'JOSE HILARIO'.
           05 WS-CB-AUTOR             PIC X(020) VALUE
              'TIAGO ASSIS SCHULZ'.
           05 WS-CB-DATA-HORA         PIC X(021).
           05 WS-CB-OBJETIVO          PIC X(035) VALUE
              'VALIDAR TRANSACAO FINANCEIRA'.
           05 WS-CB-EXECUCAO          PIC X(015) VALUE
              'COBOL - BATCH'.

       01  WS-CTE-ERRO-TECNICO.
           05 WS-ERR-PROGRAMA         PIC X(008) VALUE SPACES.
           05 WS-ERR-ARQUIVO          PIC X(008) VALUE SPACES.
           05 WS-ERR-OPERACAO         PIC X(011) VALUE SPACES.
           05 WS-ERR-FS               PIC X(002) VALUE SPACES.
           05 WS-ERR-MENSAGEM         PIC X(060) VALUE SPACES.

       01  WS-GDA-DATA-HORA.
           05 WS-CURRENT-DATE        PIC X(021).

       01  WS-DATA-HORA-FMT.
           05 WS-DH-DIA              PIC X(002).
           05 FILLER                 PIC X(001) VALUE '/'.
           05 WS-DH-MES              PIC X(002).
           05 FILLER                 PIC X(001) VALUE '/'.
           05 WS-DH-ANO              PIC X(004).
           05 FILLER                 PIC X(002) VALUE '  '.
           05 WS-DH-HORA             PIC X(002).
           05 FILLER                 PIC X(001) VALUE ':'.
           05 WS-DH-MIN              PIC X(002).
           05 FILLER                 PIC X(001) VALUE ':'.
           05 WS-DH-SEG              PIC X(002).

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------

       01  LK-DADOS.
           05 LK-CLI-FOUND           PIC X(001).
           05 LK-CLI-ST              PIC X(001).
           05 LK-TRA-TP              PIC X(001).
           05 LK-TRA-VLR             PIC 9(009)V99.
           05 LK-CLI-SDO             PIC 9(009)V99.
           05 LK-RET-COD             PIC 9(002).
           05 LK-RET-MSG             PIC X(060).

      ******************************************************************
       PROCEDURE DIVISION USING LK-DADOS.
      ******************************************************************

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL SECTION.
      *-----------------------------------------------------------------

           PERFORM 110000-EXIBIR-CABECALHO
           PERFORM 100000-VALIDAR-CLIENTE
           PERFORM 900000-FINALIZAR

           .
       000000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       100000-VALIDAR-CLIENTE SECTION.
      *-----------------------------------------------------------------

              MOVE 00 TO LK-RET-COD.
           MOVE SPACES TO LK-RET-MSG.

           IF LK-CLI-FOUND NOT = 'S'
              MOVE 01 TO LK-RET-COD
              MOVE 'CLIENTE NAO ENCONTRADO' TO LK-RET-MSG
           ELSE
              IF LK-CLI-ST = 'I'
                 MOVE 02 TO LK-RET-COD
                 MOVE 'CLIENTE INATIVO' TO LK-RET-MSG
              ELSE
                 IF LK-CLI-ST = 'B'
                    MOVE 03 TO LK-RET-COD
                    MOVE 'CLIENTE BLOQUEADO' TO LK-RET-MSG
                 ELSE
                    IF LK-TRA-TP NOT = 'D'
                       AND LK-TRA-TP NOT = 'S'
                       AND LK-TRA-TP NOT = 'P'
                       MOVE 04 TO LK-RET-COD
                       MOVE 'TIPO DE TRANSACAO INVALIDO'
                            TO LK-RET-MSG
                    ELSE
                       IF LK-TRA-VLR = ZEROS
                          MOVE 05 TO LK-RET-COD
                          MOVE 'VALOR INVALIDO' TO LK-RET-MSG
                       ELSE
                          IF (LK-TRA-TP = 'S'
                              OR LK-TRA-TP = 'P')
                             AND LK-TRA-VLR > LK-CLI-SDO
                             MOVE 06 TO LK-RET-COD
                             MOVE 'SALDO INSUFICIENTE'
                                  TO LK-RET-MSG
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF

           .
       000000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXIBIR-CABECALHO SECTION.
      *-----------------------------------------------------------------

           MOVE FUNCTION CURRENT-DATE TO WS-CB-DATA-HORA
           MOVE WS-CB-DATA-HORA(7:2)  TO WS-DH-DIA
           MOVE WS-CB-DATA-HORA(5:2)  TO WS-DH-MES
           MOVE WS-CB-DATA-HORA(1:4)  TO WS-DH-ANO
           MOVE WS-CB-DATA-HORA(9:2)  TO WS-DH-HORA
           MOVE WS-CB-DATA-HORA(11:2) TO WS-DH-MIN
           MOVE WS-CB-DATA-HORA(13:2) TO WS-DH-SEG

           DISPLAY '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*'
           DISPLAY '* SIGLA......: ' WS-CB-SIGLA
           DISPLAY '* PROGRAMA...: ' WS-CB-PROGRAMA
           DISPLAY '* ANALISTA...: ' WS-CB-ANALISTA
           DISPLAY '* AUTOR......: ' WS-CB-AUTOR
           DISPLAY '* DATA/HORA..: ' WS-DATA-HORA-FMT
           DISPLAY '* OBJETIVO...: ' WS-CB-OBJETIVO
           DISPLAY '* EXECUCAO...: ' WS-CB-EXECUCAO
           DISPLAY '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*' &
                   '*' & '*' & '*' & '*' & '*' & '*' & '*'

           .
       110000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       850000-TRATAR-ERRO-TECNICO SECTION.
      *-----------------------------------------------------------------

           DISPLAY '=================================================='
           DISPLAY '* ERRO TECNICO DETECTADO'
           DISPLAY '* PROGRAMA...: ' WS-ERR-PROGRAMA
           DISPLAY '* ARQUIVO....: ' WS-ERR-ARQUIVO
           DISPLAY '* OPERACAO...: ' WS-ERR-OPERACAO
           DISPLAY '* FILE STATUS: ' WS-ERR-FS
           DISPLAY '* MENSAGEM...: ' WS-ERR-MENSAGEM
           DISPLAY '=================================================='

           MOVE SPACES TO WS-ERR-PROGRAMA
                          WS-ERR-ARQUIVO
                          WS-ERR-OPERACAO
                          WS-ERR-FS
                          WS-ERR-MENSAGEM

           .
       850000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       900000-FINALIZAR        SECTION.
      *-----------------------------------------------------------------

           GOBACK

           .
       900000-FIM.
           EXIT.
