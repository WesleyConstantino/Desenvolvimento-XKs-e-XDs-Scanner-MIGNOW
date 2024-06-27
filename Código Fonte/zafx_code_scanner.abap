REPORT  zafx_code_scanner.

INCLUDE zafx_global_data_public.

TYPE-POOLS: slis, abap.

TYPES: BEGIN OF t_str_lines,
         devclass LIKE tadir-devclass,
         progname LIKE rs38m-programm,
         linno    LIKE rslgaxdata-line,
         line     LIKE abapsource-line,
       END   OF t_str_lines.

DATA: BEGIN OF g_tab_lines OCCURS 0,
        devclass LIKE tadir-devclass,
        progname LIKE rs38m-programm,
        linno    LIKE rslgaxdata-line,
        line     LIKE abapsource-line,
      END   OF g_tab_lines.


* Global data
TABLES:    tadir.                                           "#EC NEEDED

CONSTANTS: c_devc_tmp    TYPE devclass VALUE '$TMP'.

DATA: g_line_object TYPE sobj_name,
      g_line_number TYPE sytabix.

"Parametros de entrada do programa original AFX_CODE_SCANNER:
DATA: p_nohits TYPE c LENGTH 1, "Não usar
      p_edit   TYPE c LENGTH 1, "Não usar
      p_lrng TYPE n LENGTH 2 VALUE '01', "Não usar
      p_conpck TYPE c LENGTH 1 VALUE 'X',
      p_strg1 TYPE c LENGTH 80. "Parametro de pesquisa.

TYPES: BEGIN OF t_abapsource_long,  "CB
         line TYPE char255,
       END OF   t_abapsource_long.
TYPES: t_tab_long_lines TYPE STANDARD TABLE OF t_abapsource_long.  "CB


*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
SELECTION-SCREEN: BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_devc FOR  tadir-devclass OBLIGATORY MEMORY ID dvc.
  SELECT-OPTIONS: s_rest FOR  tadir-obj_name NO-DISPLAY.
  SELECTION-SCREEN:   SKIP.

  PARAMETERS: rb_xk01 RADIOBUTTON GROUP gr1 DEFAULT 'X',
              rb_xk02 RADIOBUTTON GROUP gr1,
              rb_xk03 RADIOBUTTON GROUP gr1,
              rb_xd01 RADIOBUTTON GROUP gr1,
              rb_xd02 RADIOBUTTON GROUP gr1,
              rb_xd03 RADIOBUTTON GROUP gr1.

SELECTION-SCREEN: END   OF BLOCK a.

SELECTION-SCREEN: BEGIN OF BLOCK c WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_prog AS CHECKBOX DEFAULT con_true,
              p_fugr AS CHECKBOX DEFAULT con_true,
              p_cinc AS CHECKBOX DEFAULT con_true.

SELECTION-SCREEN: END   OF BLOCK c.

"Início da execução
START-OF-SELECTION.

*Wesley Santos - Mignow - 25/06/24
  IF  rb_xk01 EQ 'X'.
    p_strg1 = '''xk01'''.
  ELSEIF rb_xk02 EQ 'X'.
    p_strg1 = '''xk02'''.
  ELSEIF rb_xk03 EQ 'X'.
    p_strg1 = '''xk03'''.
  ELSEIF rb_xd01 EQ 'X'.
    p_strg1 = '''xd01'''.
  ELSEIF rb_xd02 EQ 'X'.
    p_strg1 = '''xd02'''.
  ELSEIF rb_xd03 EQ 'X'.
    p_strg1 = '''xd03'''.
  ENDIF.
*Wesley Santos - Mignow - 25/06/24

  PERFORM process_devc.

*-----------------------------------------------------------------------
AT LINE-SELECTION.
  PERFORM navigate_to_object USING g_line_object g_line_number p_edit.

*---------------------------------------------------------------------*
*       FORM process_devc                                             *
*---------------------------------------------------------------------*
FORM process_devc.
  DATA: l_tab_tadir       TYPE TABLE OF tadir,
        l_str_tadir       TYPE tadir,
        l_cnt             TYPE i,
        l_cnt_str(10)     TYPE c,
        l_tabix           TYPE i,
        l_flg_process_tmp TYPE xfeld,
        l_answer          TYPE c,
        l_popuptext(200)  TYPE c,
        l_devclass        TYPE devclass.

* Initialization
  REFRESH: g_tab_lines,
           l_tab_tadir.

* Obtem os pacotes
  SELECT * FROM tadir INTO TABLE l_tab_tadir            "#EC CI_GENBUFF
                         WHERE pgmid    = 'R3TR' AND
                               object   = 'DEVC' AND
                               devclass IN s_devc.    "#EC CI_SGLSELECT

* Ignora as entradas da TADIR inválidas
  DELETE l_tab_tadir WHERE obj_name IS INITIAL.

  DESCRIBE TABLE l_tab_tadir LINES l_cnt.

* Verifica o pacote local $TMP nos critérios de seleção
  CLEAR l_flg_process_tmp.
  IF c_devc_tmp IN s_devc.
    l_flg_process_tmp = con_true.
    l_cnt = l_cnt + 1.
  ENDIF.

* inicia a explosão da estrutura de pacotes
  IF p_conpck EQ abap_true.

    FIELD-SYMBOLS:
      <f_tadir>      TYPE tadir,
      <f_cumul>      TYPE packname,
      <l_descendant> TYPE cl_pak_package_queries=>ty_subpackage_info.

    DATA: t_descendant TYPE cl_pak_package_queries=>tt_subpackage_info,
          t_cumul      TYPE scompaknam,
          f_tadir      TYPE tadir.

    CLEAR t_cumul.
    LOOP AT l_tab_tadir ASSIGNING <f_tadir>.

      CLEAR t_descendant.

*Obtem todos os subpacotes de um pacote específico
      CALL METHOD cl_pak_package_queries=>get_all_subpackages
        EXPORTING
          im_package     = <f_tadir>-devclass "Nome do pacote
        IMPORTING
          et_subpackages = t_descendant "Retorno dos subpacotes
        EXCEPTIONS
          OTHERS         = 1.

      LOOP AT t_descendant ASSIGNING <l_descendant>.
        APPEND <l_descendant>-package TO t_cumul.
      ENDLOOP.

      APPEND <f_tadir>-devclass TO t_cumul.

    ENDLOOP.

    SORT t_cumul BY table_line ASCENDING.
    DELETE ADJACENT DUPLICATES FROM t_cumul.

    LOOP AT t_cumul ASSIGNING <f_cumul>.
* O DEVC-TADIR já está em L_TAB_TADIR
      READ TABLE l_tab_tadir WITH KEY devclass = <f_cumul> TRANSPORTING
      NO FIELDS.
      IF sy-subrc EQ 0.
        CONTINUE.
      ENDIF.
      CLEAR f_tadir.
      SELECT SINGLE * FROM tadir INTO f_tadir WHERE devclass = <f_cumul>
                                                        "#EC CI_GENBUFF
                                              AND   pgmid    = 'R3TR'
                                              AND   object   = 'DEVC'.
      IF sy-subrc EQ 0.
        APPEND f_tadir TO l_tab_tadir.
      ENDIF.

    ENDLOOP.

  ENDIF.
****************Finaliza a explosão da estrutura de pacotes

* Processo de pacotes
  l_tabix = 0.
  LOOP AT l_tab_tadir INTO l_str_tadir.
    l_tabix = l_tabix + 1.
    l_devclass = l_str_tadir-obj_name.
    PERFORM scan_devc USING l_devclass l_tabix l_cnt p_lrng.
  ENDLOOP.

* Processo de pacotes locais $TMP
  IF l_flg_process_tmp = con_true.
    l_tabix = l_tabix + 1.
    PERFORM scan_devc USING c_devc_tmp l_tabix l_cnt p_lrng.
  ENDIF.

* Exibe dados de resultado da varredura
  PERFORM scan_result_display.

ENDFORM.                    "process_devc

*&---------------------------------------------------------------------*
*&      Form  scan_result_display
*&---------------------------------------------------------------------*
FORM scan_result_display.
  DATA: l_str_layout     TYPE slis_layout_alv,
        l_tab_all_events TYPE slis_t_event,
        l_tab_events     TYPE slis_t_event,
        l_str_event      TYPE slis_alv_event,
        l_repid          TYPE syrepid,
        l_tab_sort       TYPE slis_t_sortinfo_alv,
        l_str_sort       TYPE slis_sortinfo_alv,
        l_tab_fieldcat   TYPE slis_t_fieldcat_alv,
        l_str_fieldcat   TYPE slis_fieldcat_alv.

* Iniialização
  CLEAR:   l_str_layout,
           l_str_event,
           l_str_sort,
           l_str_fieldcat.
  REFRESH: l_tab_all_events,
           l_tab_events,
           l_tab_sort,
           l_tab_fieldcat.
  l_repid = sy-repid.

* Inicializa o Layout do ALV
  l_str_layout-detail_popup         = con_true.
  l_str_layout-detail_initial_lines = con_true.
  l_str_layout-expand_all           = con_true.
  l_str_layout-colwidth_optimize    = con_true.
  l_str_layout-zebra                = con_true.

* Chamar a função para obter todos os eventos possíveis para o ALV
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = l_tab_all_events. "Lista de eventos.


  READ TABLE l_tab_all_events WITH KEY name = slis_ev_user_command
             INTO l_str_event.
  IF sy-subrc = 0.
    l_str_event-form = 'ALV_USER_COMMAND'.
    APPEND l_str_event TO l_tab_events.
  ENDIF.

  l_str_sort-spos = '01'.
  l_str_sort-fieldname = 'DEVCLASS'.
  l_str_sort-up = con_true.

  APPEND l_str_sort TO l_tab_sort.

  l_str_sort-spos = '02'.
  l_str_sort-fieldname = 'PROGNAME'.
  l_str_sort-up = con_true.
  APPEND l_str_sort TO l_tab_sort.

* Monta fieldcat automáticamente:
"Obs: todas as linhas do report devem ter até 72 carácteres,
"caso contrário será gerado um dump nesta função.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = l_repid
      i_internal_tabname     = 'G_TAB_LINES'
      i_inclname             = l_repid
      i_bypassing_buffer     = con_true
    CHANGING
      ct_fieldcat            = l_tab_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
  ENDIF.

  READ TABLE l_tab_fieldcat WITH KEY fieldname = 'LINNO'
             INTO l_str_fieldcat.
  IF sy-subrc = 0.
    l_str_fieldcat-hotspot = con_true.
    l_str_fieldcat-just = 'R'.
    MODIFY l_tab_fieldcat FROM l_str_fieldcat INDEX sy-tabix.
  ENDIF.

  READ TABLE l_tab_fieldcat WITH KEY fieldname = 'LINE'
             INTO l_str_fieldcat.
  IF sy-subrc = 0.
    l_str_fieldcat-emphasize = 'C500'.

    l_str_fieldcat-lzero = con_true.
    MODIFY l_tab_fieldcat FROM l_str_fieldcat INDEX sy-tabix.
  ENDIF.

* Exibe ALV
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = l_repid
      is_layout          = l_str_layout
      it_fieldcat        = l_tab_fieldcat
      it_sort            = l_tab_sort
      i_save             = 'A'
      it_events          = l_tab_events
    TABLES
      t_outtab           = g_tab_lines
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " scan_result_display

*---------------------------------------------------------------------*
*       FORM scan_devc                                                *
*---------------------------------------------------------------------*
FORM scan_devc USING u_devc         TYPE devclass
                     u_index        TYPE i
                     u_count        TYPE i
                     u_cnt_line     TYPE n.

* Scaneia códigos do pacote atual
  "Reports:
  IF p_prog = con_true.
    PERFORM scan_devc_prog
      USING u_devc u_index u_count u_cnt_line.
  ENDIF.
  "Funções:
  IF p_fugr = con_true.
    PERFORM scan_devc_fugr
      USING u_devc u_index u_count u_cnt_line.
  ENDIF.
  "Classes:
  IF p_cinc = con_true.
    PERFORM scan_devc_class
      USING u_devc u_index u_count u_cnt_line.
  ENDIF.
ENDFORM.                    "scan_devc

*&---------------------------------------------------------------------*
*&      Form  scan_devc_prog
*&---------------------------------------------------------------------*
FORM scan_devc_prog USING u_devc     TYPE devclass
                          u_index    TYPE i
                          u_count    TYPE i
                          u_cnt_line TYPE n.
  DATA: l_tab_tadir     TYPE TABLE OF tadir,
        l_str_tadir     TYPE tadir,
        l_cnt           TYPE i,
        l_cnt_str(10)   TYPE c,
        l_idx_devc(10)  TYPE c,
        l_cnt_devc(10)  TYPE c,
        l_aux_devc(20)  TYPE c,
        l_percentage    TYPE p,
        l_tabix_str(10) TYPE c,
        l_rep_name      TYPE sobj_name,
        l_tab_source    TYPE t_tab_long_lines,    "CB
        l_text          TYPE itex132.

* Inicialização
  l_idx_devc = u_index.
  l_cnt_devc = u_count.
  CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
  CONDENSE l_aux_devc.

* Obtem programas do pacote atual
  REFRESH l_tab_tadir.
  IF u_devc <> c_devc_tmp.
    SELECT * FROM tadir INTO TABLE l_tab_tadir          "#EC CI_GENBUFF
      WHERE pgmid    = 'R3TR' AND
            object   = 'PROG' AND
            devclass = u_devc AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT

  ELSE.
    SELECT * FROM tadir INTO TABLE l_tab_tadir          "#EC CI_GENBUFF
      WHERE pgmid    = 'R3TR' AND
            object   = 'PROG' AND
            devclass = u_devc AND
            author   = sy-uname AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ENDIF.

* Ignora entradas da TADIR inválidas.
  DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Grava a contagem de programas na lista
  DESCRIBE TABLE l_tab_tadir LINES l_cnt.
  IF l_cnt = 0.
    EXIT.
  ENDIF.

* Processa todas as linhas do programa
  l_cnt_str = l_cnt.
  CONDENSE l_cnt_str.
  LOOP AT l_tab_tadir INTO l_str_tadir.
    l_tabix_str = sy-tabix.
    CONDENSE l_tabix_str.

*   Exibir indicador de progresso
    l_percentage = 100 * ( sy-tabix / l_cnt ).
    CONCATENATE 'Scanne Paket'(008) u_devc l_aux_devc
                '(' 'Report'(009) l_tabix_str 'von'(010) l_cnt_str ')'
                INTO l_text SEPARATED BY space.

*   Função que Exibe o indicador de progresso
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_percentage "Porcentagem progresso a ser exibida.
        text       = l_text. "Texto que será exibido durante o scanner.

    l_rep_name = l_str_tadir-obj_name.
    REFRESH l_tab_source.
*   Faz a leitura das linhas do repot atual:
    READ REPORT l_rep_name INTO l_tab_source STATE 'I'.
    IF sy-subrc NE 0.
      READ REPORT l_rep_name INTO l_tab_source.
    ENDIF.
    IF sy-subrc = 0.
*   Faz o scanner da linha atual de acordo com os filtros:
      PERFORM scan_prog USING    u_devc
                                 l_rep_name
                                 u_cnt_line
                        CHANGING l_tab_source.     "CB

    ENDIF.

  ENDLOOP.

ENDFORM.                    " scan_devc_prog

*&---------------------------------------------------------------------*
*&      Form  scan_devc_fugr
*&---------------------------------------------------------------------*
FORM scan_devc_fugr USING u_devc     TYPE devclass
                          u_index    TYPE i
                          u_count    TYPE i
                          u_cnt_line TYPE n.
  DATA: l_tab_tadir     TYPE TABLE OF tadir,
        l_str_tadir     TYPE tadir,
        l_tab_e071      TYPE TABLE OF e071,
        l_str_e071      TYPE e071,
        l_str_tfdir     TYPE tfdir,
        l_cnt           TYPE i,
        l_cnt_str(10)   TYPE c,
        l_idx_devc(10)  TYPE c,
        l_cnt_devc(10)  TYPE c,
        l_aux_devc(20)  TYPE c,
        l_percentage    TYPE p,
        l_tabix_str(10) TYPE c,
        l_rep_name      TYPE sobj_name,
        l_tab_source    TYPE TABLE OF t_abapsource_long,       "CB
        l_text          TYPE itex132.

* Inicialização
  l_idx_devc = u_index.
  l_cnt_devc = u_count.
  CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
  CONDENSE l_aux_devc.

* Obtem funções do pacote atual
  REFRESH l_tab_tadir.
  IF u_devc <> c_devc_tmp.
    SELECT * FROM tadir INTO TABLE l_tab_tadir          "#EC CI_GENBUFF
      WHERE pgmid    = 'R3TR' AND
            object   = 'FUGR' AND
            devclass = u_devc AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ELSE.
    SELECT * FROM tadir INTO TABLE l_tab_tadir          "#EC CI_GENBUFF
      WHERE pgmid    = 'R3TR' AND
            object   = 'FUGR' AND
            devclass = u_devc AND
            author   = sy-uname AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ENDIF.

* Ignora entradas inválidas da TADIR.
  DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Grava a contagem de linhas da função
  DESCRIBE TABLE l_tab_tadir LINES l_cnt.
  IF l_cnt = 0.
    EXIT.
  ENDIF.

* Processa todos os grupos de funções
  l_cnt_str = l_cnt.
  CONDENSE l_cnt_str.
  LOOP AT l_tab_tadir INTO l_str_tadir.
    l_tabix_str = sy-tabix.
    CONDENSE l_tabix_str.

*   Indicador de progresso durante a execução:
    l_percentage = 100 * ( sy-tabix / l_cnt ).
    CONCATENATE 'Scanne Paket'(008) u_devc l_aux_devc
                '(' 'FuGr'(011) l_tabix_str 'von'(010) l_cnt_str ')'
                INTO l_text SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_percentage
        text       = l_text.

*   Obtém objetos do grupo de funções atal:
    l_str_e071-pgmid    = l_str_tadir-pgmid.
    l_str_e071-object   = l_str_tadir-object.
    l_str_e071-obj_name = l_str_tadir-obj_name.
    REFRESH l_tab_e071.
    CALL FUNCTION 'STOR_RESOLVE_FUGR'
      EXPORTING
        is_e071 = l_str_e071 "Passa o grupo de função.
      TABLES
        tt_e071 = l_tab_e071 "Recebe os módulos de função.
      EXCEPTIONS
        OTHERS  = 0.

*Lê os programas base e procura cadeias de caracteres especificadas
    LOOP AT l_tab_e071 INTO l_str_e071 WHERE object = 'REPO' .
      l_rep_name = l_str_e071-obj_name.
      REFRESH l_tab_source.
      READ REPORT l_rep_name INTO l_tab_source STATE 'I'.
      IF sy-subrc NE 0.
        READ REPORT l_rep_name INTO l_tab_source.
      ENDIF.
      IF sy-subrc = 0.
        "scanner da linha atual:
        PERFORM scan_prog USING    u_devc
                                   l_rep_name
                                   u_cnt_line
                          CHANGING l_tab_source.       "CB

      ENDIF.
    ENDLOOP .

* Lê linhas do módulo de função e procure strings especificadas
    LOOP AT l_tab_e071 INTO l_str_e071 WHERE object = 'FUNC' .
      IF l_str_e071-obj_name(4) = 'VIEW'. "Keine gen. Dialoge
        CONTINUE.
      ENDIF.
      SELECT SINGLE * FROM tfdir INTO l_str_tfdir
        WHERE funcname = l_str_e071-obj_name.         "#EC CI_SGLSELECT
      IF sy-subrc = 0.
        CONCATENATE l_str_tfdir-pname 'U' l_str_tfdir-include
                    INTO l_rep_name.
        REPLACE 'SAPL' WITH 'L' INTO l_rep_name.
        REFRESH l_tab_source.
        READ REPORT l_rep_name INTO l_tab_source STATE 'I'.
        IF sy-subrc NE 0.
          READ REPORT l_rep_name INTO l_tab_source.
        ENDIF.
        IF sy-subrc = 0.
          "scanner da linha atual:
          PERFORM scan_prog USING    u_devc
                                     l_rep_name
                                     u_cnt_line
                            CHANGING l_tab_source.     "CB

        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP .

ENDFORM.                    " scan_devc_fugr


*&--------------------------------------------------------------------*
*&      Form  scan_devc_class
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->U_DEVC     text
*      -->U_INDEX    text
*      -->U_COUNT    text
*      -->U_CNT_LINE text
*---------------------------------------------------------------------*
FORM scan_devc_class USING u_devc     TYPE devclass
                           u_index    TYPE i
                           u_count    TYPE i
                           u_cnt_line TYPE n.
  DATA: l_tab_tadir     TYPE TABLE OF tadir,
        l_str_tadir     TYPE tadir,
        l_str_e071      TYPE e071,
        l_cnt           TYPE i,
        l_cnt_str(10)   TYPE c,
        l_idx_devc(10)  TYPE c,
        l_cnt_devc(10)  TYPE c,
        l_aux_devc(20)  TYPE c,
        l_percentage    TYPE p,
        l_tabix_str(10) TYPE c,
        l_rep_name      TYPE sobj_name,
        l_tab_source    TYPE TABLE OF t_abapsource_long,
        l_text          TYPE itex132,
        l_tab_trdir     TYPE STANDARD TABLE OF trdir,
        l_str_trdir     LIKE LINE OF l_tab_trdir,
        l_tab_selopt    TYPE STANDARD TABLE OF rsdsselopt,
        l_str_selopt    LIKE LINE OF l_tab_selopt.

* Inicialização
  l_idx_devc = u_index.
  l_cnt_devc = u_count.
  CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
  CONDENSE l_aux_devc.

* Obtém as classes do pacote atual
  REFRESH l_tab_tadir.
  IF u_devc <> c_devc_tmp.
    SELECT * FROM tadir INTO TABLE l_tab_tadir          "#EC CI_GENBUFF
      WHERE pgmid    = 'R3TR' AND
            object   = 'CLAS' AND
            devclass = u_devc AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ELSE.
    SELECT * FROM tadir INTO TABLE l_tab_tadir          "#EC CI_GENBUFF
      WHERE pgmid    = 'R3TR' AND
            object   = 'CLAS' AND
            devclass = u_devc AND
            author   = sy-uname AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ENDIF.

* Ignora entradas inválidas da TADIR.
  DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Grava a contagem de classes na lista
  DESCRIBE TABLE l_tab_tadir LINES l_cnt.
  IF l_cnt = 0.
  ENDIF.

* Processa todas as classes
  l_cnt_str = l_cnt.
  CONDENSE l_cnt_str.
  LOOP AT l_tab_tadir INTO l_str_tadir.
    l_tabix_str = sy-tabix.
    CONDENSE l_tabix_str.

*   Indicador de progresso durante a execução:
    l_percentage = 100 * ( sy-tabix / l_cnt ).
    CONCATENATE 'Scanne Paket'(008) u_devc l_aux_devc
                '(' 'Klasse'(012) l_tabix_str 'von'(010) l_cnt_str ')'
                INTO l_text SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_percentage
        text       = l_text.

* Obtém os includes da classe atual:
    REFRESH l_tab_selopt.
    l_str_selopt-sign = 'I'.
    l_str_selopt-option = 'CP'.
    CONCATENATE l_str_tadir-obj_name '*' INTO
     l_str_selopt-low.
    APPEND l_str_selopt TO l_tab_selopt.

    SELECT * FROM trdir INTO TABLE l_tab_trdir
              WHERE name IN l_tab_selopt.             "#EC CI_SGLSELECT

*   LOOP em todas as classes do pacote atual
    LOOP AT l_tab_trdir INTO l_str_trdir.
      l_rep_name = l_str_e071-obj_name.
      REFRESH l_tab_source.
      l_rep_name = l_str_trdir-name.
      READ REPORT l_rep_name INTO l_tab_source STATE 'I'.
      IF sy-subrc NE 0.
        READ REPORT l_rep_name INTO l_tab_source.
      ENDIF.

      IF sy-subrc = 0.
        "scanner da linha atual da classe:
        PERFORM scan_prog USING    u_devc
                                   l_rep_name
                                   u_cnt_line
                          CHANGING l_tab_source.     "CB
      ELSE.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: / 'Report'(009), l_rep_name, 'nicht gefunden!'(013).
      ENDIF.
    ENDLOOP.

  ENDLOOP .

ENDFORM.                    " scan_devc_class


*&---------------------------------------------------------------------*
*&      Form  scan_prog
*&---------------------------------------------------------------------*
FORM scan_prog USING    i_devclass   TYPE devclass
                        i_objname    TYPE sobj_name
                        i_cnt_line   TYPE n
               CHANGING i_tab_source TYPE t_tab_long_lines.
  DATA: l_str_source TYPE t_abapsource_long,
        l_flg_found  TYPE xfeld,
        l_flg_write  TYPE xfeld,
        l_cnt_line   TYPE i,
        l_str_lines  TYPE t_str_lines.

* Inicialização
  CLEAR l_flg_found.
  g_line_object = i_objname.
  l_cnt_line = 1000.

  CLEAR l_str_lines.

*Wesley Santos - Mignow - 25/06/24
  TYPES: BEGIN OF ty_line_split,
           line LIKE abapsource-line,
         END   OF ty_line_split.

  DATA: lt_line_split TYPE TABLE OF ty_line_split,
        lv_line_split_1 TYPE char255,
        lv_line_split_2 TYPE char255,
        lv_line         TYPE char255.
*Wesley Santos - Mignow - 25/06/24

* Fonte de pesquisa para critérios de seleção
  "LOOP de cada linha do programa atual:
  LOOP AT i_tab_source INTO l_str_source.

    g_line_number = sy-tabix.
    CLEAR l_flg_write.

    "Verifica se a linha tem a transação obsoleta
    IF l_str_source-line CS p_strg1.

*Wesley Santos - Mignow - 25/06/24
       "Ignora linhas com comentários e CALL TRANSACTION
       "para o split.
       IF  l_str_source-line(1) <> '*' AND
           l_str_source-line NP '*CALL TRANSACTION*'.

        lv_line = l_str_source-line.

        " Verifica se o primeiro caractere é um espaço
        IF lv_line+0(1) = ' '.
          " Remove o primeiro caractere (espaço)
          lv_line = lv_line+1.
        ENDIF.

        SPLIT lv_line AT '=' INTO lv_line_split_1 lv_line_split_2.
        CLEAR lv_line_split_2.
        "Remove os espaços:
        CONDENSE lv_line_split_1.
        CONCATENATE '*' lv_line_split_1 '*' INTO lv_line_split_2.

        APPEND lv_line_split_2 TO lt_line_split.
*Wesley Santos - Mignow - 25/06/24

        l_flg_write = con_true.
        l_cnt_line  = 0.
      ENDIF.
    ENDIF.

*Wesley Santos - Mignow - 25/06/24
      IF lt_line_split IS NOT INITIAL.
*   Loop com o nome da variável que tem a transação obsoleta
        LOOP AT lt_line_split INTO DATA(wa_line_split).

          "Filtro para efetuar o append na tabela de saída
          IF l_str_source-line CP '*CALL TRANSACTION*' AND
             l_str_source-line CP wa_line_split-line AND
             l_str_source-line+0(1) NE '*'.

            l_str_lines-linno = g_line_number.
            l_str_lines-line  = l_str_source-line.
            l_str_lines-devclass = i_devclass.
            l_str_lines-progname = i_objname.
            "Append na tabela da saída
            APPEND l_str_lines TO g_tab_lines.
            CLEAR l_str_lines.

          ENDIF.
          CLEAR: wa_line_split.
        ENDLOOP.

      ENDIF.
*Wesley Santos - Mignow - 25/06/24

  ENDLOOP.

* Nenhum resultado encontrado
  IF p_nohits = con_true AND l_flg_found IS INITIAL.

    l_str_lines-linno = 1.
    l_str_lines-line  = 'Keine Treffer'(014).
    APPEND l_str_lines TO g_tab_lines.
  ENDIF.

ENDFORM.                    " scan_prog

*&---------------------------------------------------------------------*
*&      Form  navigate_to_object
*&---------------------------------------------------------------------*
FORM navigate_to_object USING i_objname  TYPE sobj_name
                              i_position TYPE sytabix
                              i_edit     TYPE xfeld.
  DATA: l_operation(5).

* Valida se o modo é de edição ou visualização
  l_operation = 'EDIT'.
  IF i_edit <> con_true.
    l_operation = 'SHOW'.
  ENDIF.

* Navegação para o objeto atual
  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation           = l_operation "EDIT ou SHOW
      object_name         = i_objname "Nome do objeto atual
      object_type         = 'REPS' "Tipo do objeto
      position            = i_position  "Posição
    EXCEPTIONS
      not_executed        = 0
      invalid_object_type = 0
      OTHERS              = 0.

ENDFORM.                    " navigate_to_object

*---------------------------------------------------------------------*
*       FORM ALV_USER_COMMAND                                         *
*---------------------------------------------------------------------*
FORM alv_user_command
     USING i_ucomm TYPE syucomm
           i_selfield TYPE slis_selfield.                   "#EC CALLED

  DATA: l_str_lines TYPE t_str_lines,
        l_position  TYPE sytabix.

  l_position = 1.
  READ TABLE g_tab_lines INTO l_str_lines INDEX i_selfield-tabindex.
  IF sy-subrc = 0.
    l_position = l_str_lines-linno.
  ENDIF.

*  BREAK-POINT.
  CASE i_ucomm.
    WHEN '&IC1'.
      PERFORM navigate_to_object USING l_str_lines-progname
                                       l_position
                                       p_edit.
  ENDCASE.

* refresh sempre col- e row-stable
  IF i_selfield-refresh = con_true.
    i_selfield-col_stable = con_true.
    i_selfield-row_stable = con_true.
  ENDIF.

ENDFORM.                    "alv_user_command
