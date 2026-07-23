******************************************************************
      * SIGLA.....: BHC - BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCPH000
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 04/07/2026
      * OBJETIVO..: GERADOR DE MASSA - FINANCE CORE
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

       PROGRAM-ID. BHCPH000.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BHCFHCLI ASSIGN TO "BHCFHCLI.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCFHCLI.

           SELECT BHCFHTRA ASSIGN TO "BHCFHTRA.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS WS-FS-BHCFHTRA.

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

      *----------------------------------------
       WORKING-STORAGE SECTION.
      *----------------------------------------

       01  WS-CTE-CABECALHO.
           05 WS-CB-SIGLA             PIC X(030) VALUE
              'BHC - BOOTCAMP HACKATHON COBOL'.
           05 WS-CB-PROGRAMA          PIC X(008) VALUE 'BHCPH000'.
           05 WS-CB-ANALISTA          PIC X(020) VALUE 'JOSE HILARIO'.
           05 WS-CB-AUTOR             PIC X(020) VALUE
              'TIAGO ASSIS SCHULZ'.
           05 WS-CB-DATA-HORA         PIC X(021).
           05 WS-CB-OBJETIVO          PIC X(035) VALUE
              'GERADOR DE MASSA - FINANCE CORE'.
           05 WS-CB-EXECUCAO          PIC X(015) VALUE
              'COBOL - BATCH'.

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

       01  WS-CTE-FILE-STATUS.
           05 WS-FS-BHCFHCLI         PIC X(002) VALUE SPACES.
           05 WS-FS-BHCFHTRA         PIC X(002) VALUE SPACES.
           05 WS-FS-OK               PIC X(002) VALUE '00'.
           05 WS-FS-EOF              PIC X(002) VALUE '10'.

       01  WS-CTE-CONTROLES.
           05 WS-PROCESSAR           PIC X(001) VALUE 'S'.
            88 WS-PROCESSAR-SIM        VALUE 'S'.
            88 WS-PROCESSAR-NAO        VALUE 'N'.
           05 WS-ABRIU-CLI           PIC X(001) VALUE 'N'.
            88 WS-ABRIU-CLI-SIM        VALUE 'S'.
            88 WS-ABRIU-CLI-NAO        VALUE 'N'.
           05 WS-ABRIU-TRA           PIC X(001) VALUE 'N'.
            88 WS-ABRIU-TRA-SIM        VALUE 'S'.
            88 WS-ABRIU-TRA-NAO        VALUE 'N'.

       01  WS-CTE-ERRO-TECNICO.
           05 WS-ERR-PROGRAMA         PIC X(008) VALUE SPACES.
           05 WS-ERR-ARQUIVO          PIC X(008) VALUE SPACES.
           05 WS-ERR-OPERACAO         PIC X(011) VALUE SPACES.
           05 WS-ERR-FS               PIC X(002) VALUE SPACES.
           05 WS-ERR-MENSAGEM         PIC X(060) VALUE SPACES.

       01  WS-GDA-TOTALIZADORES.
           05 WS-TOT-CLI-GRV         PIC 9(005) VALUE ZEROS.
           05 WS-TOT-TRA-GRV         PIC 9(005) VALUE ZEROS.
           05 WS-TOT-ERROS           PIC 9(005) VALUE ZEROS.

       01  WS-GDA-REGISTROS.
           05 WS-REG-CLI             PIC X(058).
           05 WS-REG-TRA             PIC X(031).

       01  WS-CLI-DETALHE.
           05 WS-CLI-CD-OUT          PIC 9(005).
           05 WS-CLI-NM-OUT          PIC X(030).
           05 WS-CLI-CPF-OUT         PIC 9(011).
           05 WS-CLI-SDO-OUT         PIC 9(009)V99.
           05 WS-CLI-ST-OUT          PIC X(001).

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

       000000-ROTINA-PRINCIPAL SECTION.
           PERFORM 100000-INICIALIZAR
           PERFORM 200000-ABRIR-ARQUIVOS
           IF WS-PROCESSAR-SIM
              PERFORM 300000-GERAR-CLIENTES
              PERFORM 400000-GERAR-TRANSACOES
              PERFORM 900000-FECHAR-ARQUIVOS
           END-IF
           PERFORM 910000-EXIBIR-TOTALIZADORES
           PERFORM 999999-FINALIZAR

           .
       000000-FIM.
           EXIT.


      *-----------------------------------------------------------------
       100000-INICIALIZAR SECTION.
      *-----------------------------------------------------------------

           MOVE ZEROS TO WS-TOT-CLI-GRV
                         WS-TOT-TRA-GRV
                         WS-TOT-ERROS

           SET WS-PROCESSAR-SIM TO TRUE

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
           DISPLAY '* SIGLA.....: ' WS-CB-SIGLA
           DISPLAY '* PROGRAMA..: ' WS-CB-PROGRAMA
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

           OPEN OUTPUT BHCFHCLI.

           IF WS-FS-BHCFHCLI = WS-FS-OK
                SET WS-ABRIU-CLI-SIM TO TRUE
           ELSE
                MOVE 'BHCPH000'       TO WS-ERR-PROGRAMA
                MOVE 'BHCFHCLI'       TO WS-ERR-ARQUIVO
                MOVE 'OPEN OUTPUT'    TO WS-ERR-OPERACAO
                MOVE WS-FS-BHCFHCLI   TO WS-ERR-FS
                MOVE 'FALHA AO ABRIR ARQUIVO DE CLIENTES'
                                      TO WS-ERR-MENSAGEM
                PERFORM 850000-TRATAR-ERRO-TECNICO
                SET WS-PROCESSAR-NAO  TO TRUE
           END-IF

           OPEN OUTPUT BHCFHTRA.

           IF WS-FS-BHCFHTRA = WS-FS-OK
                SET WS-ABRIU-TRA-SIM TO TRUE
           ELSE
                MOVE 'BHCPH000'       TO WS-ERR-PROGRAMA
                MOVE 'BHCFHTRA'       TO WS-ERR-ARQUIVO
                MOVE 'OPEN OUTPUT'    TO WS-ERR-OPERACAO
                MOVE WS-FS-BHCFHTRA   TO WS-ERR-FS
                MOVE 'FALHA AO ABRIR ARQUIVO DE TRANSACOES'
                                      TO WS-ERR-MENSAGEM
                PERFORM 850000-TRATAR-ERRO-TECNICO
                SET WS-PROCESSAR-NAO  TO TRUE
           END-IF

           .
       200000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       300000-GERAR-CLIENTES SECTION.
      *-----------------------------------------------------------------

           IF WS-ABRIU-CLI-SIM
              MOVE 00001 TO WS-CLI-CD-OUT
              MOVE 'JOAO SILVA' TO WS-CLI-NM-OUT
              MOVE 12345678901 TO WS-CLI-CPF-OUT
              MOVE 00000150000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00002 TO WS-CLI-CD-OUT
              MOVE 'MARIA SOUZA' TO WS-CLI-NM-OUT
              MOVE 23456789012 TO WS-CLI-CPF-OUT
              MOVE 00000087550 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00003 TO WS-CLI-CD-OUT
              MOVE 'CARLOS LIMA' TO WS-CLI-NM-OUT
              MOVE 34567890123 TO WS-CLI-CPF-OUT
              MOVE 00000005000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00004 TO WS-CLI-CD-OUT
              MOVE 'ANA PEREIRA' TO WS-CLI-NM-OUT
              MOVE 45678901234 TO WS-CLI-CPF-OUT
              MOVE 00000250000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00005 TO WS-CLI-CD-OUT
              MOVE 'PAULO SANTOS' TO WS-CLI-NM-OUT
              MOVE 56789012345 TO WS-CLI-CPF-OUT
              MOVE 00000000000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00006 TO WS-CLI-CD-OUT
              MOVE 'FERNANDA COSTA' TO WS-CLI-NM-OUT
              MOVE 67890123456 TO WS-CLI-CPF-OUT
              MOVE 00000032000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00007 TO WS-CLI-CD-OUT
              MOVE 'RICARDO ALMEIDA' TO WS-CLI-NM-OUT
              MOVE 78901234567 TO WS-CLI-CPF-OUT
              MOVE 00000120000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00008 TO WS-CLI-CD-OUT
              MOVE 'JULIANA ROCHA' TO WS-CLI-NM-OUT
              MOVE 89012345678 TO WS-CLI-CPF-OUT
              MOVE 00000065000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00009 TO WS-CLI-CD-OUT
              MOVE 'MARCOS OLIVEIRA' TO WS-CLI-NM-OUT
              MOVE 90123456789 TO WS-CLI-CPF-OUT
              MOVE 00000007500 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00010 TO WS-CLI-CD-OUT
              MOVE 'PATRICIA GOMES' TO WS-CLI-NM-OUT
              MOVE 01234567890 TO WS-CLI-CPF-OUT
              MOVE 00000180000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00011 TO WS-CLI-CD-OUT
              MOVE 'LUCAS MARTINS' TO WS-CLI-NM-OUT
              MOVE 11223344556 TO WS-CLI-CPF-OUT
              MOVE 00000040000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00012 TO WS-CLI-CD-OUT
              MOVE 'CAMILA RIBEIRO' TO WS-CLI-NM-OUT
              MOVE 22334455667 TO WS-CLI-CPF-OUT
              MOVE 00000099000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00013 TO WS-CLI-CD-OUT
              MOVE 'ROBERTO BARBOSA' TO WS-CLI-NM-OUT
              MOVE 33445566778 TO WS-CLI-CPF-OUT
              MOVE 00000003000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00014 TO WS-CLI-CD-OUT
              MOVE 'BEATRIZ MENDES' TO WS-CLI-NM-OUT
              MOVE 44556677889 TO WS-CLI-CPF-OUT
              MOVE 00000145000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00015 TO WS-CLI-CD-OUT
              MOVE 'GUSTAVO NUNES' TO WS-CLI-NM-OUT
              MOVE 55667788990 TO WS-CLI-CPF-OUT
              MOVE 00000025000 TO WS-CLI-SDO-OUT
              MOVE 'A' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00016 TO WS-CLI-CD-OUT
              MOVE 'HELENA DIAS' TO WS-CLI-NM-OUT
              MOVE 66778899001 TO WS-CLI-CPF-OUT
              MOVE 00000070000 TO WS-CLI-SDO-OUT
              MOVE 'I' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00017 TO WS-CLI-CD-OUT
              MOVE 'RAFAEL FREITAS' TO WS-CLI-NM-OUT
              MOVE 77889900112 TO WS-CLI-CPF-OUT
              MOVE 00000110000 TO WS-CLI-SDO-OUT
              MOVE 'I' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00018 TO WS-CLI-CD-OUT
              MOVE 'LARISSA CAMPOS' TO WS-CLI-NM-OUT
              MOVE 88990011223 TO WS-CLI-CPF-OUT
              MOVE 00000055000 TO WS-CLI-SDO-OUT
              MOVE 'I' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00019 TO WS-CLI-CD-OUT
              MOVE 'EDUARDO MOREIRA' TO WS-CLI-NM-OUT
              MOVE 99001122334 TO WS-CLI-CPF-OUT
              MOVE 00000200000 TO WS-CLI-SDO-OUT
              MOVE 'B' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV

              MOVE 00020 TO WS-CLI-CD-OUT
              MOVE 'TATIANE AZEVEDO' TO WS-CLI-NM-OUT
              MOVE 00112233445 TO WS-CLI-CPF-OUT
              MOVE 00000033000 TO WS-CLI-SDO-OUT
              MOVE 'B' TO WS-CLI-ST-OUT
              MOVE WS-CLI-DETALHE TO WS-REG-CLI
              WRITE BHCFHCLI-REG FROM WS-REG-CLI
              ADD 1 TO WS-TOT-CLI-GRV
           END-IF

           .
       300000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       400000-GERAR-TRANSACOES SECTION.
      *-----------------------------------------------------------------

           IF WS-ABRIU-TRA-SIM
              MOVE '00000100001D0000005000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00000200002S0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00000300003P0000000200020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00000400004S0000003000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00000500005D0000002500020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00000600006P0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00000700007S0000005000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00000800008D0000007500020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00000900009P0000000250020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00001000010S0000002000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00001100011D0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00001200012P0000000500020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00001300013S0000000200020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00001400014D0000003000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00001500015P0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00001600001S0000002500020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00001700002D0000004000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00001800003S0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00001900004P0000005000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00002000005S0000000100020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00002199999D0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00002288888S0000000500020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00002377777P0000000750020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00002466666D0000002000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00002555555S0000000100020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00002600016D0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00002700017S0000000500020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00002800018P0000000250020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00002900019D0000005000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00003000020S0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00003100001X0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00003200002Z0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00003300003T0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00003400004S0000030000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00003500006P0000010000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00003600009S0000001000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00003700013P0000005000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00003800005D0000000000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00003900007S0000000000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV

              MOVE '00004000008P0000000000020260704'
                TO WS-REG-TRA
              WRITE BHCFHTRA-REG FROM WS-REG-TRA
              ADD 1 TO WS-TOT-TRA-GRV
           END-IF

           .
       400000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       850000-TRATAR-ERRO-TECNICO SECTION.
      *-----------------------------------------------------------------

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

           .
       850000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       900000-FECHAR-ARQUIVOS SECTION.
      *-----------------------------------------------------------------

           IF WS-ABRIU-CLI-SIM
              CLOSE BHCFHCLI
           END-IF

           IF WS-ABRIU-TRA-SIM
              CLOSE BHCFHTRA
           END-IF

           .
       900000-FIM.
           EXIT.

      ******************************************************************
       910000-EXIBIR-TOTALIZADORES SECTION.
      ******************************************************************

           DISPLAY 'TOTAL DE CLIENTES GRAVADOS...: ' WS-TOT-CLI-GRV
           DISPLAY 'TOTAL DE TRANSACOES GRAVADAS.: ' WS-TOT-TRA-GRV
           DISPLAY 'TOTAL DE ERROS DE ARQUIVO....: ' WS-TOT-ERROS

           .
       910000-FIM.
           EXIT.


      ******************************************************************
       999999-FINALIZAR SECTION.
      ******************************************************************

           DISPLAY '-------------------------------------'
           DISPLAY 'BHCPH000 - FIM DO PROCESSAMENTO'
           GOBACK

           .
       999999-FIM.
           EXIT.
