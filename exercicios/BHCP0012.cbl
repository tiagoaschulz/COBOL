      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0012
      * PROFESSOR.: JOSE HILARIO
      * AUTOR.....: TIAGO ASSIS SCHULZ
      * DATA......: 01/07/2026
      * OBJETIVO..: GERAR ARQUIVO DE PARTICIPANTES
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL     DESCRICAO DA VERSAO
      * --- -------- --------------- ----------------------------------
      * 001 01.07.26 TIAGO A. SCHULZ        IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0012.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT BHCF012S ASSIGN TO "BHCF012S.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS  IS FS-BHCF012S.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************

      *----------------------------------------
       FILE SECTION.
      *----------------------------------------

       FD  BHCF012S
       RECORDING MODE IS F.
       01  REG-BHCF012S               PIC X(065).

      *----------------------------------------
       LOCAL-STORAGE SECTION.
      *----------------------------------------

      *---- FILE STATUS
       01  GDA-FILE-STATUS.
       05  FS-BHCF012S            PIC X(002) VALUE SPACES.

      *---- COMPARADOR DE FILE STATUS OK
       01  GDA-GR-FS.
       05  GDA-FS-OK              PIC X(002) VALUE '00'.

      *---- TABELA INTERNA DE PARTICIPANTES
       01  GDA-TABELA-PARTICIPANTES.
       05  GDA-PARTICIPANTE OCCURS 10 TIMES.
       10  GDA-CODIGO         PIC 9(005).
       10  GDA-NOME           PIC X(030).
       10  GDA-UF             PIC X(002).
       10  GDA-TRILHA         PIC X(010).
       10  GDA-SITUACAO       PIC X(010).
       10  GDA-DATA           PIC 9(008).

      *---- CONTROLES
       01  GDA-CONTROLES.
       05  GDA-INDICE             PIC 9(002) VALUE ZEROS.

      *---- TOTALIZADORES
       01  GDA-TOTALIZADORES.
       05  GDA-TOT-GRAVADOS       PIC 9(005) VALUE ZEROS.
       05  GDA-TOT-ERROS          PIC 9(005) VALUE ZEROS.

      *---- DATA E HORA ATUAL
       01  GDA-DATA-HORA.
       05  GDA-CURRENT-DATE       PIC X(021).

      *---- AREA DE MONTAGEM DO REGISTRO DE SAIDA
       01  GDA-GR-BHCF012S.
       05  FD-CODIGO              PIC 9(005).
       05  FD-NOME                PIC X(030).
       05  FD-UF                  PIC X(002).
       05  FD-TRILHA              PIC X(010).
       05  FD-SITUACAO            PIC X(010).
       05  FD-DATA                PIC 9(008).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

       000000-ROTINA-PRINCIPAL.
           PERFORM 100000-INICIALIZAR.
           PERFORM 200000-CARREGAR-TABELA.
           PERFORM 300000-ABRIR-ARQUIVO.
           PERFORM 400000-GRAVAR-PARTICIPANTES.
           PERFORM 900000-FECHAR-ARQUIVO.
           PERFORM 910000-EXIBIR-TOTALIZADORES.
           GOBACK.

      ******************************************************************
      *....INICIALIZA TOTALIZADORES E EXIBE INICIO
      ******************************************************************
       100000-INICIALIZAR.
           MOVE ZEROS TO GDA-TOT-GRAVADOS
                GDA-TOT-ERROS.

           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.

           DISPLAY 'BHCP0012 - INICIO DO PROCESSAMENTO'.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.

      ******************************************************************
      *....CARGA DA TABELA COM A MASSA OFICIAL DE DADOS
      ******************************************************************
       200000-CARREGAR-TABELA.
           MOVE 00001 TO GDA-CODIGO(1).
           MOVE 'ANA SILVA'       TO GDA-NOME(1).
           MOVE 'SP'              TO GDA-UF(1).
           MOVE 'COBOL'           TO GDA-TRILHA(1).
           MOVE 'ATIVO'           TO GDA-SITUACAO(1).
           MOVE 20260701          TO GDA-DATA(1).

           MOVE 00002 TO GDA-CODIGO(2).
           MOVE 'BRUNO LIMA'      TO GDA-NOME(2).
           MOVE 'RJ'              TO GDA-UF(2).
           MOVE 'COBOL'           TO GDA-TRILHA(2).
           MOVE 'ATIVO'           TO GDA-SITUACAO(2).
           MOVE 20260701          TO GDA-DATA(2).

      *----REGISTRO 00003 COM NOME EM BRANCO (SERA REJEITADO NO EX13/14)
           MOVE 00003 TO GDA-CODIGO(3).
           MOVE SPACES            TO GDA-NOME(3).
           MOVE 'MG'              TO GDA-UF(3).
           MOVE 'JCL'             TO GDA-TRILHA(3).
           MOVE 'ATIVO'           TO GDA-SITUACAO(3).
           MOVE 20260701          TO GDA-DATA(3).

           MOVE 00004 TO GDA-CODIGO(4).
           MOVE 'DANIEL COSTA'    TO GDA-NOME(4).
           MOVE 'BA'              TO GDA-UF(4).
           MOVE 'COBOL'           TO GDA-TRILHA(4).
           MOVE 'ATIVO'           TO GDA-SITUACAO(4).
           MOVE 20260701          TO GDA-DATA(4).

      *----REGISTRO 00005 COM UF EM BRANCO (SERA REJEITADO NO EX13/14)
           MOVE 00005 TO GDA-CODIGO(5).
           MOVE 'ELISA ROCHA'     TO GDA-NOME(5).
           MOVE SPACES            TO GDA-UF(5).
           MOVE 'DB2'             TO GDA-TRILHA(5).
           MOVE 'ATIVO'           TO GDA-SITUACAO(5).
           MOVE 20260701          TO GDA-DATA(5).

           MOVE 00006 TO GDA-CODIGO(6).
           MOVE 'FERNANDO ALVES'  TO GDA-NOME(6).
           MOVE 'SC'              TO GDA-UF(6).
           MOVE 'VSAM'            TO GDA-TRILHA(6).
           MOVE 'ATIVO'           TO GDA-SITUACAO(6).
           MOVE 20260701          TO GDA-DATA(6).

           MOVE 00007 TO GDA-CODIGO(7).
           MOVE 'GABRIELA NUNES'  TO GDA-NOME(7).
           MOVE 'RS'              TO GDA-UF(7).
           MOVE 'COBOL'           TO GDA-TRILHA(7).
           MOVE 'ATIVO'           TO GDA-SITUACAO(7).
           MOVE 20260701          TO GDA-DATA(7).

           MOVE 00008 TO GDA-CODIGO(8).
           MOVE 'HENRIQUE DIAS'   TO GDA-NOME(8).
           MOVE 'PE'              TO GDA-UF(8).
           MOVE 'JCL'             TO GDA-TRILHA(8).
           MOVE 'ATIVO'           TO GDA-SITUACAO(8).
           MOVE 20260701          TO GDA-DATA(8).

           MOVE 00009 TO GDA-CODIGO(9).
           MOVE 'ISABELA MOURA'   TO GDA-NOME(9).
           MOVE 'CE'              TO GDA-UF(9).
           MOVE 'DB2'             TO GDA-TRILHA(9).
           MOVE 'ATIVO'           TO GDA-SITUACAO(9).
           MOVE 20260701          TO GDA-DATA(9).

           MOVE 00010 TO GDA-CODIGO(10).
           MOVE 'JOAO PEREIRA'    TO GDA-NOME(10).
           MOVE 'GO'              TO GDA-UF(10).
           MOVE 'VSAM'            TO GDA-TRILHA(10).
           MOVE 'ATIVO'           TO GDA-SITUACAO(10).
           MOVE 20260701          TO GDA-DATA(10).

      ******************************************************************
      *....ABRE O ARQUIVO DE SAIDA
      ******************************************************************
       300000-ABRIR-ARQUIVO.
           OPEN OUTPUT BHCF012S.

           IF FS-BHCF012S = GDA-FS-OK
             DISPLAY 'ARQUIVO BHCF012S ABERTO COM SUCESSO'
           ELSE
             DISPLAY 'ERRO AO ABRIR BHCF012S. FILE STATUS: '
                FS-BHCF012S
             ADD 1 TO GDA-TOT-ERROS
           END-IF.

      ******************************************************************
      *....PERCORRE A TABELA E GRAVA OS 10 REGISTROS
      ******************************************************************
       400000-GRAVAR-PARTICIPANTES.
           IF GDA-TOT-ERROS = ZEROS

           PERFORM VARYING GDA-INDICE FROM 1 BY 1
             UNTIL GDA-INDICE > 10

           MOVE GDA-CODIGO(GDA-INDICE)   TO FD-CODIGO
           MOVE GDA-NOME(GDA-INDICE)     TO FD-NOME
           MOVE GDA-UF(GDA-INDICE)       TO FD-UF
           MOVE GDA-TRILHA(GDA-INDICE)   TO FD-TRILHA
           MOVE GDA-SITUACAO(GDA-INDICE) TO FD-SITUACAO
           MOVE GDA-DATA(GDA-INDICE)     TO FD-DATA

           WRITE REG-BHCF012S FROM GDA-GR-BHCF012S

           IF FS-BHCF012S = GDA-FS-OK
                ADD 1 TO GDA-TOT-GRAVADOS
           ELSE
                DISPLAY 'ERRO WRITE BHCF012S. FILE STATUS: '
                        FS-BHCF012S
                ADD 1 TO GDA-TOT-ERROS
           END-IF

           END-PERFORM

           END-IF.

      ******************************************************************
      *....FECHA O ARQUIVO E EXIBE DATA/HORA DE FIM
      ******************************************************************
       900000-FECHAR-ARQUIVO.
           CLOSE BHCF012S.

           MOVE FUNCTION CURRENT-DATE TO GDA-CURRENT-DATE.

           DISPLAY 'BHCP0012 - FIM DO PROCESSAMENTO'.
           DISPLAY 'DATA/HORA: ' GDA-CURRENT-DATE.

      ******************************************************************
      *....EXIBE OS TOTALIZADORES NO SYSOUT
      ******************************************************************
       910000-EXIBIR-TOTALIZADORES.
           DISPLAY 'TOTAL DE REGISTROS GRAVADOS.: ' GDA-TOT-GRAVADOS.
           DISPLAY 'TOTAL DE ERROS DE ARQUIVO...: ' GDA-TOT-ERROS.
