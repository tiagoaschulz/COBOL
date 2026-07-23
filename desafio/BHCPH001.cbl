      ******************************************************************
      * SIGLA.....: BHC - BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCPH001
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 04/07/2026
      * OBJETIVO..: SISTEMA FINANCEIRO - FINANCE CORE
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

       PROGRAM-ID. BHCPH001.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************

      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
      *-----------------------------------------------------------------

       FILE-CONTROL.
           SELECT BHCFHCLI ASSIGN TO "BHCFHCLI.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCFHCLI.

           SELECT BHCFHTRA ASSIGN TO "BHCFHTRA.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCFHTRA.

           SELECT BHCFHEXT ASSIGN TO "BHCFHEXT.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCFHEXT.

           SELECT BHCFHLOG ASSIGN TO "BHCFHLOG.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCFHLOG.

           SELECT BHCFHJSN ASSIGN TO "BHCFHJSN.json"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCFHJSN.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************

      *----------------------------------------
       FILE SECTION.
      *----------------------------------------

       FD  BHCFHCLI.
       01  BHCFHCLI-REG              PIC X(058).

       FD  BHCFHTRA.
       01  BHCFHTRA-REG              PIC X(031).

       FD  BHCFHEXT.
       01  BHCFHEXT-REG              PIC X(059).

       FD  BHCFHLOG.
       01  BHCFHLOG-REG              PIC X(091).

       FD  BHCFHJSN.
       01  BHCFHJSN-REG              PIC X(200).

      *----------------------------------------
       WORKING-STORAGE SECTION.
      *----------------------------------------

       01  WS-CTE-CABECALHO.
           05 WS-CB-SIGLA             PIC X(030) VALUE
              'BHC - BOOTCAMP HACKATHON COBOL'.
           05 WS-CB-PROGRAMA          PIC X(008) VALUE 'BHCPH001'.
           05 WS-CB-ANALISTA          PIC X(020) VALUE 'JOSE HILARIO'.
           05 WS-CB-AUTOR             PIC X(020) VALUE
              'TIAGO ASSIS SCHULZ'.
           05 WS-CB-DATA-HORA         PIC X(021).
           05 WS-CB-OBJETIVO          PIC X(035) VALUE
              'SISTEMA FINANCEIRO - FINANCE CORE'.
           05 WS-CB-EXECUCAO          PIC X(015) VALUE
              'COBOL - BATCH'.

       01  WS-CTE-FILE-STATUS.
           05 WS-FS-BHCFHCLI         PIC X(002) VALUE SPACES.
           05 WS-FS-BHCFHTRA         PIC X(002) VALUE SPACES.
           05 WS-FS-BHCFHEXT         PIC X(002) VALUE SPACES.
           05 WS-FS-BHCFHLOG         PIC X(002) VALUE SPACES.
           05 WS-FS-BHCFHJSN         PIC X(002) VALUE SPACES.
           05 WS-FS-OK               PIC X(002) VALUE '00'.
           05 WS-FS-EOF              PIC X(002) VALUE '10'.

       01  WS-CTE-CONTROLES.
           05 WS-PROCESSAR           PIC X(001) VALUE 'S'.
              88 WS-PROCESSAR-SIM        VALUE 'S'.
              88 WS-PROCESSAR-NAO        VALUE 'N'.
           05 WS-FIM-CLI             PIC X(001) VALUE 'N'.
              88 WS-FIM-CLI-SIM          VALUE 'S'.
              88 WS-FIM-CLI-NAO          VALUE 'N'.
           05 WS-FIM-TRA             PIC X(001) VALUE 'N'.
              88 WS-FIM-TRA-SIM          VALUE 'S'.
              88 WS-FIM-TRA-NAO          VALUE 'N'.
           05 WS-ABRIU-CLI           PIC X(001) VALUE 'N'.
              88 WS-ABRIU-CLI-SIM        VALUE 'S'.
              88 WS-ABRIU-CLI-NAO        VALUE 'N'.
           05 WS-ABRIU-TRA           PIC X(001) VALUE 'N'.
              88 WS-ABRIU-TRA-SIM        VALUE 'S'.
              88 WS-ABRIU-TRA-NAO        VALUE 'N'.
           05 WS-ABRIU-EXT           PIC X(001) VALUE 'N'.
              88 WS-ABRIU-EXT-SIM        VALUE 'S'.
              88 WS-ABRIU-EXT-NAO        VALUE 'N'.
           05 WS-ABRIU-LOG           PIC X(001) VALUE 'N'.
              88 WS-ABRIU-LOG-SIM        VALUE 'S'.
              88 WS-ABRIU-LOG-NAO        VALUE 'N'.
           05 WS-ABRIU-JSN           PIC X(001) VALUE 'N'.
              88 WS-ABRIU-JSN-SIM        VALUE 'S'.
              88 WS-ABRIU-JSN-NAO        VALUE 'N'.
           05 WS-CLI-FOUND           PIC X(001) VALUE 'N'.
              88 WS-CLI-FOUND-SIM       VALUE 'S'.
              88 WS-CLI-FOUND-NAO       VALUE 'N'.

       01  WS-CTE-ERRO-TECNICO.
           05 WS-ERR-PROGRAMA         PIC X(008) VALUE SPACES.
           05 WS-ERR-ARQUIVO          PIC X(008) VALUE SPACES.
           05 WS-ERR-OPERACAO         PIC X(011) VALUE SPACES.
           05 WS-ERR-FS               PIC X(002) VALUE SPACES.
           05 WS-ERR-MENSAGEM         PIC X(060) VALUE SPACES.

       01  WS-GDA-TOTALIZADORES.
           05 WS-TOT-CLI-LIDOS       PIC 9(005) VALUE ZEROS.
           05 WS-TOT-TRA-LIDAS       PIC 9(005) VALUE ZEROS.
           05 WS-TOT-TRA-VALIDAS     PIC 9(005) VALUE ZEROS.
           05 WS-TOT-TRA-REJ         PIC 9(005) VALUE ZEROS.
           05 WS-TOT-EXT-GRV         PIC 9(005) VALUE ZEROS.
           05 WS-TOT-LOG-GRV         PIC 9(005) VALUE ZEROS.
           05 WS-TOT-JSN-GRV         PIC 9(005) VALUE ZEROS.
           05 WS-TOT-ERROS           PIC 9(005) VALUE ZEROS.

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

       01  WS-IDX-INDICES.
           05 WS-INDICE-CLI          PIC 9(003) VALUE ZEROS.
           05 WS-INDICE-TRA          PIC 9(003) VALUE ZEROS.
           05 WS-INDICE-PESQ         PIC 9(003) VALUE ZEROS.

       01  WS-GDA-LK-DADOS.
           05 WS-LK-CLI-FOUND        PIC X(001).
           05 WS-LK-CLI-ST           PIC X(001).
           05 WS-LK-TRA-TP           PIC X(001).
           05 WS-LK-TRA-VLR          PIC 9(009)V99.
           05 WS-LK-CLI-SDO          PIC 9(009)V99.
           05 WS-LK-RET-COD          PIC 9(002).
           05 WS-LK-RET-MSG          PIC X(060).

       01  WS-GDA-TB-CLI.
           05 WS-CLI OCCURS 100 TIMES.
              10 WS-CLI-CD           PIC 9(005).
              10 WS-CLI-NM           PIC X(030).
              10 WS-CLI-CPF          PIC 9(011).
              10 WS-CLI-SDO          PIC 9(009)V99.
              10 WS-CLI-ST           PIC X(001).

       01  WS-GDA-JSON-AUX.
           05 WS-JSON-CLI-LIDOS      PIC ZZZZ9.
           05 WS-JSON-TRA-LIDAS      PIC ZZZZ9.
           05 WS-JSON-TRA-VALIDAS    PIC ZZZZ9.
           05 WS-JSON-TRA-REJ        PIC ZZZZ9.
           05 WS-JSON-EXT-GRV        PIC ZZZZ9.
           05 WS-JSON-LOG-GRV        PIC ZZZZ9.

      *----------------------------------------
       LOCAL-STORAGE SECTION.
      *----------------------------------------

       01  FD-REG-CLI.
           05 FD-CLI-CD              PIC 9(005).
           05 FD-CLI-NM              PIC X(030).
           05 FD-CLI-CPF             PIC 9(011).
           05 FD-CLI-SDO             PIC 9(009)V99.
           05 FD-CLI-ST              PIC X(001).

       01  FD-REG-TRA.
           05 FD-TRA-ID              PIC 9(006).
           05 FD-TRA-CD-CLI          PIC 9(005).
           05 FD-TRA-TP              PIC X(001).
           05 FD-TRA-VLR             PIC 9(009)V99.
           05 FD-TRA-DT              PIC 9(008).

       01  FD-REG-EXT.
           05 FD-EXT-ID-TRA          PIC 9(006).
           05 FILLER                 PIC X(001) VALUE ';'.
           05 FD-EXT-CD-CLI          PIC 9(005).
           05 FILLER                 PIC X(001) VALUE ';'.
           05 FD-EXT-TP-TRA          PIC X(001).
           05 FILLER                 PIC X(001) VALUE ';'.
           05 FD-EXT-VLR-TRA         PIC 9(009)V99.
           05 FILLER                 PIC X(001) VALUE ';'.
           05 FD-EXT-SDO-ANT         PIC 9(009)V99.
           05 FILLER                 PIC X(001) VALUE ';'.
           05 FD-EXT-SDO-ATU         PIC 9(009)V99.
           05 FILLER                 PIC X(001) VALUE ';'.
           05 FD-EXT-DT-TRA          PIC 9(008).

       01  FD-REG-LOG.
           05 FD-LOG-ID-TRA          PIC 9(006).
           05 FILLER                 PIC X(001) VALUE ';'.
           05 FD-LOG-CD-CLI          PIC 9(005).
           05 FILLER                 PIC X(001) VALUE ';'.
           05 FD-LOG-TP-TRA          PIC X(001).
           05 FILLER                 PIC X(001) VALUE ';'.
           05 FD-LOG-VLR-TRA         PIC 9(009)V99.
           05 FILLER                 PIC X(001) VALUE ';'.
           05 FD-LOG-CD-ERR          PIC X(002).
           05 FILLER                 PIC X(001) VALUE ';'.
           05 FD-LOG-MSG-ERR         PIC X(060).

       01  FD-REG-JSN                PIC X(200).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

      *-----------------------------------------------------------------
       000000-ROTINA-PRINCIPAL SECTION.
      *-----------------------------------------------------------------

           PERFORM 100000-INICIALIZAR
           PERFORM 200000-ABRIR-ARQUIVOS
           IF WS-PROCESSAR-SIM
              PERFORM 300000-CARREGAR-CLIENTES
              PERFORM 400000-PROCESSAR-TRANSACOES
              PERFORM 500000-GERAR-JSON
           END-IF
           PERFORM 800000-FECHAR-ARQUIVOS
           PERFORM 900000-EXIBIR-TOTALIZADORES
           PERFORM 999999-FINALIZAR

           .
       000000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       100000-INICIALIZAR SECTION.
      *-----------------------------------------------------------------

           MOVE ZEROS TO WS-TOT-CLI-LIDOS
                         WS-TOT-TRA-LIDAS
                         WS-TOT-TRA-VALIDAS
                         WS-TOT-TRA-REJ
                         WS-TOT-EXT-GRV
                         WS-TOT-LOG-GRV
                         WS-TOT-JSN-GRV
                         WS-TOT-ERROS
                         WS-INDICE-CLI
                         WS-INDICE-TRA
                         WS-INDICE-PESQ

           SET WS-PROCESSAR-SIM TO TRUE
           SET WS-FIM-CLI-NAO TO TRUE
           SET WS-FIM-TRA-NAO TO TRUE

           MOVE FUNCTION CURRENT-DATE TO WS-CB-DATA-HORA
           MOVE WS-CB-DATA-HORA(7:2)  TO WS-DH-DIA
           MOVE WS-CB-DATA-HORA(5:2)  TO WS-DH-MES
           MOVE WS-CB-DATA-HORA(1:4)  TO WS-DH-ANO
           MOVE WS-CB-DATA-HORA(9:2)  TO WS-DH-HORA
           MOVE WS-CB-DATA-HORA(11:2) TO WS-DH-MIN
           MOVE WS-CB-DATA-HORA(13:2) TO WS-DH-SEG

           PERFORM 110000-EXIBIR-CABECALHO

           .
       100000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       110000-EXIBIR-CABECALHO SECTION.
      *-----------------------------------------------------------------

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
       200000-ABRIR-ARQUIVOS SECTION.
      *-----------------------------------------------------------------

           OPEN INPUT BHCFHCLI

           IF WS-FS-BHCFHCLI = WS-FS-OK
              SET WS-ABRIU-CLI-SIM TO TRUE
           ELSE
              MOVE 'BHCPH001'       TO WS-ERR-PROGRAMA
              MOVE 'BHCFHCLI'       TO WS-ERR-ARQUIVO
              MOVE 'OPEN INPUT'     TO WS-ERR-OPERACAO
              MOVE WS-FS-BHCFHCLI   TO WS-ERR-FS
              MOVE 'FALHA AO ABRIR ARQUIVO DE CLIENTES'
                                    TO WS-ERR-MENSAGEM
              PERFORM 850000-TRATAR-ERRO-TECNICO
              SET WS-PROCESSAR-NAO  TO TRUE
           END-IF

           OPEN INPUT BHCFHTRA

           IF WS-FS-BHCFHTRA = WS-FS-OK
              SET WS-ABRIU-TRA-SIM TO TRUE
           ELSE
              MOVE 'BHCPH001'       TO WS-ERR-PROGRAMA
              MOVE 'BHCFHTRA'       TO WS-ERR-ARQUIVO
              MOVE 'OPEN INPUT'     TO WS-ERR-OPERACAO
              MOVE WS-FS-BHCFHTRA   TO WS-ERR-FS
              MOVE 'FALHA AO ABRIR ARQUIVO DE TRANSACOES'
                                    TO WS-ERR-MENSAGEM
              PERFORM 850000-TRATAR-ERRO-TECNICO
              SET WS-PROCESSAR-NAO  TO TRUE
           END-IF

           OPEN OUTPUT BHCFHEXT

           IF WS-FS-BHCFHEXT = WS-FS-OK
              SET WS-ABRIU-EXT-SIM TO TRUE
           ELSE
              MOVE 'BHCPH001'       TO WS-ERR-PROGRAMA
              MOVE 'BHCFHEXT'       TO WS-ERR-ARQUIVO
              MOVE 'OPEN OUTPUT'    TO WS-ERR-OPERACAO
              MOVE WS-FS-BHCFHEXT   TO WS-ERR-FS
              MOVE 'FALHA AO ABRIR ARQUIVO DE EXTRATO'
                                    TO WS-ERR-MENSAGEM
              PERFORM 850000-TRATAR-ERRO-TECNICO
              SET WS-PROCESSAR-NAO  TO TRUE
           END-IF

           OPEN OUTPUT BHCFHLOG

           IF WS-FS-BHCFHLOG = WS-FS-OK
              SET WS-ABRIU-LOG-SIM TO TRUE
           ELSE
              MOVE 'BHCPH001'       TO WS-ERR-PROGRAMA
              MOVE 'BHCFHLOG'       TO WS-ERR-ARQUIVO
              MOVE 'OPEN OUTPUT'    TO WS-ERR-OPERACAO
              MOVE WS-FS-BHCFHLOG   TO WS-ERR-FS
              MOVE 'FALHA AO ABRIR ARQUIVO DE LOG'
                                    TO WS-ERR-MENSAGEM
              PERFORM 850000-TRATAR-ERRO-TECNICO
              SET WS-PROCESSAR-NAO  TO TRUE
           END-IF

           OPEN OUTPUT BHCFHJSN

           IF WS-FS-BHCFHJSN = WS-FS-OK
              SET WS-ABRIU-JSN-SIM TO TRUE
           ELSE
              MOVE 'BHCPH001'       TO WS-ERR-PROGRAMA
              MOVE 'BHCFHJSN'       TO WS-ERR-ARQUIVO
              MOVE 'OPEN OUTPUT'    TO WS-ERR-OPERACAO
              MOVE WS-FS-BHCFHJSN   TO WS-ERR-FS
              MOVE 'FALHA AO ABRIR ARQUIVO JSON'
                                    TO WS-ERR-MENSAGEM
              PERFORM 850000-TRATAR-ERRO-TECNICO
              SET WS-PROCESSAR-NAO  TO TRUE
           END-IF

           .
       200000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       300000-CARREGAR-CLIENTES SECTION.
      *-----------------------------------------------------------------

           PERFORM 310000-LER-CLIENTE UNTIL WS-FIM-CLI-SIM

           .
       300000-FIM.
            EXIT.

      *-----------------------------------------------------------------
       310000-LER-CLIENTE SECTION.
      *-----------------------------------------------------------------

           READ BHCFHCLI INTO FD-REG-CLI
                AT END
                    SET WS-FIM-CLI-SIM TO TRUE
                NOT AT END
                    ADD 1 TO WS-TOT-CLI-LIDOS
                    ADD 1 TO WS-INDICE-CLI
                    MOVE FD-CLI-CD  TO WS-CLI-CD(WS-INDICE-CLI)
                    MOVE FD-CLI-NM  TO WS-CLI-NM(WS-INDICE-CLI)
                    MOVE FD-CLI-CPF TO WS-CLI-CPF(WS-INDICE-CLI)
                    MOVE FD-CLI-SDO TO WS-CLI-SDO(WS-INDICE-CLI)
                    MOVE FD-CLI-ST  TO WS-CLI-ST(WS-INDICE-CLI)
           END-READ

           IF WS-FS-BHCFHCLI NOT = WS-FS-OK
              AND WS-FS-BHCFHCLI NOT = WS-FS-EOF
              MOVE 'BHCPH001'       TO WS-ERR-PROGRAMA
              MOVE 'BHCFHCLI'       TO WS-ERR-ARQUIVO
              MOVE 'READ'           TO WS-ERR-OPERACAO
              MOVE WS-FS-BHCFHCLI   TO WS-ERR-FS
              MOVE 'FALHA AO LER ARQUIVO DE CLIENTES'
                                    TO WS-ERR-MENSAGEM
              PERFORM 850000-TRATAR-ERRO-TECNICO
              SET WS-FIM-CLI-SIM TO TRUE
              SET WS-PROCESSAR-NAO TO TRUE
           END-IF

           .
       310000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       400000-PROCESSAR-TRANSACOES SECTION.
      *-----------------------------------------------------------------

           PERFORM 410000-LER-TRANSACAO UNTIL WS-FIM-TRA-SIM

           .
       400000-FIM.
            EXIT.

      *-----------------------------------------------------------------
       410000-LER-TRANSACAO SECTION.
      *-----------------------------------------------------------------

           READ BHCFHTRA INTO FD-REG-TRA
                AT END
                    SET WS-FIM-TRA-SIM TO TRUE
                NOT AT END
                    ADD 1 TO WS-TOT-TRA-LIDAS
                    PERFORM 420000-TRATAR-TRANSACAO
           END-READ

           IF WS-FS-BHCFHTRA NOT = WS-FS-OK
              AND WS-FS-BHCFHTRA NOT = WS-FS-EOF
              MOVE 'BHCPH001'       TO WS-ERR-PROGRAMA
              MOVE 'BHCFHTRA'       TO WS-ERR-ARQUIVO
              MOVE 'READ'           TO WS-ERR-OPERACAO
              MOVE WS-FS-BHCFHTRA   TO WS-ERR-FS
              MOVE 'FALHA AO LER ARQUIVO DE TRANSACOES'
                                    TO WS-ERR-MENSAGEM
              PERFORM 850000-TRATAR-ERRO-TECNICO
              SET WS-FIM-TRA-SIM TO TRUE
              SET WS-PROCESSAR-NAO TO TRUE
           END-IF

           .
       410000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       420000-TRATAR-TRANSACAO SECTION.
      *-----------------------------------------------------------------

           SET WS-CLI-FOUND-NAO TO TRUE
           MOVE ZEROS TO WS-INDICE-PESQ

           PERFORM VARYING WS-INDICE-PESQ FROM 1 BY 1
              UNTIL WS-INDICE-PESQ > WS-TOT-CLI-LIDOS
                 OR WS-CLI-CD(WS-INDICE-PESQ) = FD-TRA-CD-CLI
           END-PERFORM

           IF WS-INDICE-PESQ <= WS-TOT-CLI-LIDOS
              AND WS-CLI-CD(WS-INDICE-PESQ) = FD-TRA-CD-CLI
              SET WS-CLI-FOUND-SIM TO TRUE
           END-IF

           MOVE WS-CLI-FOUND TO WS-LK-CLI-FOUND

           IF WS-CLI-FOUND-SIM
              MOVE WS-CLI-ST(WS-INDICE-PESQ)  TO WS-LK-CLI-ST
              MOVE WS-CLI-SDO(WS-INDICE-PESQ) TO WS-LK-CLI-SDO
           ELSE
              MOVE SPACES TO WS-LK-CLI-ST
              MOVE ZEROS  TO WS-LK-CLI-SDO
           END-IF

           MOVE FD-TRA-TP TO WS-LK-TRA-TP
           MOVE FD-TRA-VLR TO WS-LK-TRA-VLR

           CALL 'BHCSH001' USING WS-GDA-LK-DADOS

           IF WS-LK-RET-COD = 00
              PERFORM 430000-GRAVAR-TRANSACAO-VALIDA
           ELSE
              PERFORM 440000-GRAVAR-TRA-REJEITADA
           END-IF

           .
       420000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       430000-GRAVAR-TRANSACAO-VALIDA SECTION.
      *-----------------------------------------------------------------

           MOVE WS-CLI-SDO(WS-INDICE-PESQ) TO WS-LK-CLI-SDO
           MOVE WS-CLI-SDO(WS-INDICE-PESQ) TO FD-EXT-SDO-ANT

           EVALUATE FD-TRA-TP
              WHEN 'D'
                 ADD FD-TRA-VLR TO WS-CLI-SDO(WS-INDICE-PESQ)
              WHEN 'S'
                 SUBTRACT FD-TRA-VLR FROM WS-CLI-SDO(WS-INDICE-PESQ)
              WHEN 'P'
                 SUBTRACT FD-TRA-VLR FROM WS-CLI-SDO(WS-INDICE-PESQ)
           END-EVALUATE

           MOVE WS-CLI-SDO(WS-INDICE-PESQ) TO FD-EXT-SDO-ATU
           MOVE FD-TRA-ID  TO FD-EXT-ID-TRA
           MOVE FD-TRA-CD-CLI TO FD-EXT-CD-CLI
           MOVE FD-TRA-TP  TO FD-EXT-TP-TRA
           MOVE FD-TRA-VLR TO FD-EXT-VLR-TRA
           MOVE FD-TRA-DT  TO FD-EXT-DT-TRA

           PERFORM 520000-GRAVAR-EXTRATO

           ADD 1 TO WS-TOT-TRA-VALIDAS

           .
       430000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       440000-GRAVAR-TRA-REJEITADA SECTION.
      *-----------------------------------------------------------------

           MOVE FD-TRA-ID     TO FD-LOG-ID-TRA
           MOVE FD-TRA-CD-CLI TO FD-LOG-CD-CLI
           MOVE FD-TRA-TP     TO FD-LOG-TP-TRA
           MOVE FD-TRA-VLR    TO FD-LOG-VLR-TRA
           MOVE WS-LK-RET-COD TO FD-LOG-CD-ERR
           MOVE WS-LK-RET-MSG TO FD-LOG-MSG-ERR

           PERFORM 530000-GRAVAR-LOG

           ADD 1 TO WS-TOT-TRA-REJ

           .
       440000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       500000-GERAR-JSON SECTION.
      *-----------------------------------------------------------------

           IF WS-ABRIU-JSN-SIM
              MOVE SPACES TO FD-REG-JSN
              STRING '{' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE SPACES TO FD-REG-JSN
              STRING '  "sistema": "FINANCE CORE",' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE SPACES TO FD-REG-JSN
              STRING '  "programa": "BHCPH001",' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE WS-TOT-CLI-LIDOS TO WS-JSON-CLI-LIDOS
              MOVE WS-TOT-TRA-LIDAS TO WS-JSON-TRA-LIDAS
              MOVE WS-TOT-TRA-VALIDAS TO WS-JSON-TRA-VALIDAS
              MOVE WS-TOT-TRA-REJ TO WS-JSON-TRA-REJ
              MOVE WS-TOT-EXT-GRV TO WS-JSON-EXT-GRV
              MOVE WS-TOT-LOG-GRV TO WS-JSON-LOG-GRV

              MOVE SPACES TO FD-REG-JSN
              STRING '  "totalizadores": {' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE SPACES TO FD-REG-JSN
              STRING '    "clientesLidos": '
               DELIMITED BY SIZE
               WS-JSON-CLI-LIDOS DELIMITED BY SIZE
                     ',' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE SPACES TO FD-REG-JSN
              STRING '    "transacoesLidas": '
               DELIMITED BY SIZE
               WS-JSON-TRA-LIDAS DELIMITED BY SIZE
                     ',' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE SPACES TO FD-REG-JSN
              STRING '    "transacoesValidas": '
               DELIMITED BY SIZE
               WS-JSON-TRA-VALIDAS DELIMITED BY SIZE
                     ',' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE SPACES TO FD-REG-JSN
              STRING '    "transacoesRejeitadas": '
               DELIMITED BY SIZE
               WS-JSON-TRA-REJ DELIMITED BY SIZE
                     ',' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE SPACES TO FD-REG-JSN
              STRING '    "extratosGerados": '
               DELIMITED BY SIZE
               WS-JSON-EXT-GRV DELIMITED BY SIZE
                     ',' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE SPACES TO FD-REG-JSN
              STRING '    "logsGerados": '
               DELIMITED BY SIZE
               WS-JSON-LOG-GRV DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE SPACES TO FD-REG-JSN
              STRING '  },' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE SPACES TO FD-REG-JSN
              STRING '  "clientes": [' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              PERFORM VARYING WS-INDICE-CLI FROM 1 BY 1
                 UNTIL WS-INDICE-CLI > WS-TOT-CLI-LIDOS
                 MOVE SPACES TO FD-REG-JSN
                 STRING '    {' DELIMITED BY SIZE
                   INTO FD-REG-JSN
                 END-STRING
                 PERFORM 510000-GRAVAR-LINHA-JSON

                 MOVE SPACES TO FD-REG-JSN
                 STRING '      "codigo": "' DELIMITED BY SIZE
                        WS-CLI-CD(WS-INDICE-CLI) DELIMITED BY SIZE
                        '"' DELIMITED BY SIZE
                   INTO FD-REG-JSN
                 END-STRING
                 PERFORM 510000-GRAVAR-LINHA-JSON

                 MOVE SPACES TO FD-REG-JSN
                 STRING '      "nome": "' DELIMITED BY SIZE
                     WS-CLI-NM(WS-INDICE-CLI) DELIMITED BY SIZE
                        '"' DELIMITED BY SIZE
                        ',' DELIMITED BY SIZE
                   INTO FD-REG-JSN
                 END-STRING
                 PERFORM 510000-GRAVAR-LINHA-JSON

                 MOVE SPACES TO FD-REG-JSN
                 STRING '      "saldoFinal": "' DELIMITED BY SIZE
                        WS-CLI-SDO(WS-INDICE-CLI) DELIMITED BY SIZE
                        '"' DELIMITED BY SIZE
                        ',' DELIMITED BY SIZE
                   INTO FD-REG-JSN
                 END-STRING
                 PERFORM 510000-GRAVAR-LINHA-JSON

                 MOVE SPACES TO FD-REG-JSN
                 STRING '      "status": "' DELIMITED BY SIZE
                        WS-CLI-ST(WS-INDICE-CLI) DELIMITED BY SIZE
                        '"' DELIMITED BY SIZE
                   INTO FD-REG-JSN
                 END-STRING

                 IF WS-INDICE-CLI < WS-TOT-CLI-LIDOS
                    STRING ',' DELIMITED BY SIZE INTO FD-REG-JSN
                    END-STRING
                 END-IF

                 PERFORM 510000-GRAVAR-LINHA-JSON
              END-PERFORM

              MOVE SPACES TO FD-REG-JSN
              STRING '  ]' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON

              MOVE SPACES TO FD-REG-JSN
              STRING '}' DELIMITED BY SIZE
                INTO FD-REG-JSN
              END-STRING
              PERFORM 510000-GRAVAR-LINHA-JSON
           END-IF

           .
       500000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       510000-GRAVAR-LINHA-JSON SECTION.
      *-----------------------------------------------------------------

           IF WS-ABRIU-JSN-SIM
              WRITE BHCFHJSN-REG FROM FD-REG-JSN
              IF WS-FS-BHCFHJSN = WS-FS-OK
                 ADD 1 TO WS-TOT-JSN-GRV
              ELSE
                 MOVE 'BHCPH001'       TO WS-ERR-PROGRAMA
                 MOVE 'BHCFHJSN'       TO WS-ERR-ARQUIVO
                 MOVE 'WRITE'          TO WS-ERR-OPERACAO
                 MOVE WS-FS-BHCFHJSN   TO WS-ERR-FS
                 MOVE 'FALHA AO GRAVAR ARQUIVO JSON'
                                       TO WS-ERR-MENSAGEM
                 PERFORM 850000-TRATAR-ERRO-TECNICO
              END-IF
           END-IF

           .
       510000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       520000-GRAVAR-EXTRATO SECTION.
      *-----------------------------------------------------------------

           IF WS-ABRIU-EXT-SIM
              WRITE BHCFHEXT-REG FROM FD-REG-EXT
              IF WS-FS-BHCFHEXT = WS-FS-OK
                 ADD 1 TO WS-TOT-EXT-GRV
              ELSE
                 MOVE 'BHCPH001'       TO WS-ERR-PROGRAMA
                 MOVE 'BHCFHEXT'       TO WS-ERR-ARQUIVO
                 MOVE 'WRITE'          TO WS-ERR-OPERACAO
                 MOVE WS-FS-BHCFHEXT   TO WS-ERR-FS
                 MOVE 'FALHA AO GRAVAR ARQUIVO DE EXTRATO'
                                       TO WS-ERR-MENSAGEM
                 PERFORM 850000-TRATAR-ERRO-TECNICO
              END-IF
           END-IF

           .
       520000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       530000-GRAVAR-LOG SECTION.
      *-----------------------------------------------------------------

           IF WS-ABRIU-LOG-SIM
              WRITE BHCFHLOG-REG FROM FD-REG-LOG
              IF WS-FS-BHCFHLOG = WS-FS-OK
                 ADD 1 TO WS-TOT-LOG-GRV
              ELSE
                 MOVE 'BHCPH001'       TO WS-ERR-PROGRAMA
                 MOVE 'BHCFHLOG'       TO WS-ERR-ARQUIVO
                 MOVE 'WRITE'          TO WS-ERR-OPERACAO
                 MOVE WS-FS-BHCFHLOG   TO WS-ERR-FS
                 MOVE 'FALHA AO GRAVAR ARQUIVO DE LOG'
                                       TO WS-ERR-MENSAGEM
                 PERFORM 850000-TRATAR-ERRO-TECNICO
              END-IF
           END-IF

           .
       530000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       850000-TRATAR-ERRO-TECNICO SECTION.
      *-----------------------------------------------------------------

           IF WS-ERR-PROGRAMA NOT = SPACES
              OR WS-ERR-ARQUIVO NOT = SPACES
              OR WS-ERR-OPERACAO NOT = SPACES
              OR WS-ERR-FS NOT = SPACES
              OR WS-ERR-MENSAGEM NOT = SPACES

              ADD 1 TO WS-TOT-ERROS

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
           END-IF

           .
       850000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       800000-FECHAR-ARQUIVOS SECTION.
      *-----------------------------------------------------------------

           IF WS-ABRIU-CLI-SIM
              CLOSE BHCFHCLI
           END-IF

           IF WS-ABRIU-TRA-SIM
              CLOSE BHCFHTRA
           END-IF

           IF WS-ABRIU-EXT-SIM
              CLOSE BHCFHEXT
           END-IF

           IF WS-ABRIU-LOG-SIM
              CLOSE BHCFHLOG
           END-IF

           IF WS-ABRIU-JSN-SIM
              CLOSE BHCFHJSN
           END-IF

           .
       800000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       900000-EXIBIR-TOTALIZADORES SECTION.
      *-----------------------------------------------------------------

           DISPLAY 'TOTAL DE CLIENTES LIDOS.......: ' WS-TOT-CLI-LIDOS
           DISPLAY 'TOTAL DE TRANSACOES LIDAS.....: ' WS-TOT-TRA-LIDAS
           DISPLAY 'TOTAL DE TRANSACOES VALIDAS...: ' WS-TOT-TRA-VALIDAS
           DISPLAY 'TOTAL DE TRANSACOES REJEITADAS: ' WS-TOT-TRA-REJ
           DISPLAY 'TOTAL DE REG. EXTRATO.........: ' WS-TOT-EXT-GRV
           DISPLAY 'TOTAL DE REG. LOG.............: ' WS-TOT-LOG-GRV
           DISPLAY 'TOTAL DE LINHAS JSON..........: ' WS-TOT-JSN-GRV
           DISPLAY 'TOTAL DE ERROS DE ARQUIVO.....: ' WS-TOT-ERROS

           .
       900000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       999999-FINALIZAR SECTION.
      *-----------------------------------------------------------------

           DISPLAY '-------------------------------------'
           DISPLAY 'BHCPH001 - FIM DO PROCESSAMENTO'
           DISPLAY 'DATA/HORA: ' WS-DATA-HORA-FMT
           GOBACK

           .
       999999-FIM.
           EXIT.
