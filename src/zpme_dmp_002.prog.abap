*&---------------------------------------------------------------------*
*& Report ZPME_DMP_002
*&---------------------------------------------------------------------*
*& Created by Minahkim
*&---------------------------------------------------------------------*
REPORT ZPME_DMP_002.

" Global types
TYPES :
  BEGIN OF gty_tdata.
    INCLUDE STRUCTURE zpm_t015.
TYPES :
    fselc TYPE xfeld,
  END OF gty_tdata,

  BEGIN OF gty_equipdata,
    equnr TYPE equnr,
    objnr TYPE j_objnr,
    point TYPE imrc_point,
    psort TYPE imrc_psort,
    lvorm TYPE imrc_del2,
  END OF gty_equipdata,

  BEGIN OF gty_err,
    msgid  LIKE sy-msgid,
    msgty  LIKE sy-msgty,
    msgno  LIKE sy-msgno,
    msgv1  LIKE sy-msgv1,
    msgv2  LIKE sy-msgv2,
    msgv3  LIKE sy-msgv3,
    msgv4  LIKE sy-msgv4,
    lineno TYPE msgzeile,
  END OF gty_err.

" Global table type
TYPES : gtt_tdata TYPE TABLE OF gty_tdata,
        gtt_equip TYPE TABLE OF gty_equipdata,
        gtt_err   TYPE STANDARD TABLE OF gty_err.

" Global data
DATA : gtc_data TYPE gtt_tdata,
       gt_err   TYPE gtt_err.

" Table control
DATA : gsc_data TYPE gty_tdata.

" Global variable
DATA : gsc_equnr TYPE equnr,
       gsc_sowrk TYPE aufsowrk,
       gsc_aufnr TYPE aufnr.

" BDC Data
DATA: bdc_data    TYPE TABLE OF bdcdata WITH HEADER LINE,
      bdc_messtab TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE TEXT-001.
  PARAMETERS : p_werks TYPE werks_d OBLIGATORY,
               p_aufnr TYPE aufnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl01.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_aufnr.
  PERFORM f_searchhelp_aufnr.

START-OF-SELECTION.
  PERFORM f_at_selscreen.

*&---------------------------------------------------------------------*
*& Form f_searchhelp_aufnr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_searchhelp_aufnr .

  DATA : BEGIN OF ls_f4values,
           value(132) TYPE c,
         END OF ls_f4values.

  DATA  : ls_f4fiel TYPE dfies,
          lt_f4tab  LIKE TABLE OF ls_f4values WITH HEADER LINE,
          lt_f4fiel TYPE TABLE OF dfies,
          lt_f4ret  TYPE TABLE OF ddshretval WITH HEADER LINE,
          lt_dyntab TYPE TABLE OF dselc WITH HEADER LINE.

  CLEAR : ls_f4fiel, lt_f4tab[], lt_f4fiel[], lt_f4ret[], lt_dyntab[].

  SELECT FROM aufk
    FIELDS aufnr, ktext
    WHERE auart LIKE 'PM%'
      AND ( auart NE 'PM91'
      AND auart NOT LIKE 'PM4%'
      AND loekz NE 'X' )
    INTO TABLE @DATA(lt_aufk).
  IF lt_aufk[] IS INITIAL.
    MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'No search help found'.
    EXIT.
  ENDIF.

  ls_f4fiel-tabname = 'AUFK'.  ls_f4fiel-fieldname = 'AUFNR'.
  APPEND ls_f4fiel TO lt_f4fiel.
  ls_f4fiel-tabname = 'AUFK'.  ls_f4fiel-fieldname = 'KTEXT'.
  APPEND ls_f4fiel TO lt_f4fiel.

  LOOP AT lt_aufk INTO DATA(ls_aufk).
    ls_f4values-value = ls_aufk-aufnr. APPEND ls_f4values TO lt_f4tab.
    CLEAR ls_f4values.
    ls_f4values-value = ls_aufk-ktext. APPEND ls_f4values TO lt_f4tab.
    CLEAR : ls_aufk, ls_f4values.
  ENDLOOP.

  lt_dyntab-fldname = 'P_AUFNR'.
  APPEND lt_dyntab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'AUFNR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_AUFNR'
    TABLES
      value_tab       = lt_f4tab
      field_tab       = lt_f4fiel
      return_tab      = lt_f4ret
      dynpfld_mapping = lt_dyntab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_at_selscreen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_at_selscreen .

  DATA  : lv_err,
            lv_message(255) TYPE c.

  CLEAR : lv_err, lv_message.

  SELECT SINGLE FROM aufk
    FIELDS phas0, phas1, phas2, phas3
    WHERE aufnr EQ @p_aufnr
      AND sowrk EQ @p_werks
    INTO @DATA(ls_phas).
  IF sy-subrc = 0.
    IF ls_phas-phas0 EQ 'X'.
      lv_message = 'Work Order belum release'.  lv_err = 'X'.
    ELSEIF ls_phas-phas2 EQ 'X'.
      lv_message = 'Work Order sudah complete'.  lv_err = 'X'.
    ELSEIF ls_phas-phas3 EQ 'X'.
      lv_message =  'Work Order sudah closed'. lv_err = 'X'.
    ELSEIF ls_phas-phas1 EQ 'X'.
      PERFORM f_getdata CHANGING lv_message lv_err.
    ENDIF.
  ELSE.
    lv_message =  'Data not found'. lv_err = 'X'.
  ENDIF.

  IF lv_err EQ 'X'.
    MESSAGE s000(0k) DISPLAY LIKE 'W' WITH lv_message.
  ELSE.
    CALL SCREEN 100.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_getdata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_MESSAGE
*&      <-- LV_ERR
*&---------------------------------------------------------------------*
FORM f_getdata CHANGING pc_lv_message TYPE char255
                        pc_lv_err.

  DATA ls_data TYPE gty_tdata.
  CLEAR : gtc_data[], ls_data, gsc_aufnr, gsc_equnr, gsc_sowrk.

  gsc_aufnr = p_aufnr.
  gsc_sowrk = p_werks.

  SELECT SINGLE FROM afih AS a
    INNER JOIN aufk AS b ON b~aufnr = a~aufnr
    FIELDS equnr
    WHERE a~aufnr EQ @p_aufnr
      AND sowrk EQ @p_werks
    INTO @gsc_equnr.

  SELECT FROM zpm_t013 AS a
    INNER JOIN mkpf AS b ON b~mjahr = a~mjahr
                         AND b~mblnr = a~mblnr
    INNER JOIN mseg AS c ON c~mjahr = b~mjahr
                         AND c~mblnr = b~mblnr
    INNER JOIN makt AS d ON d~matnr = a~matnr
    FIELDS a~aufnr, a~werks, b~bldat, b~budat, a~mjahr, a~mblnr, c~zeile,
           a~bwart, a~sobkz, a~lgort, a~matnr, c~erfmg, c~erfme, a~charg, a~insdt, a~instm,
           c~bwtar, a~sernr, a~equnr, a~heqnr, a~posnr, a~hequi, c~usnam_mkpf, d~maktx
    WHERE a~aufnr EQ @p_aufnr
      AND a~werks EQ @p_werks
      AND mrdel NE 'X'
    INTO TABLE @DATA(lt_t013).
  IF sy-subrc = 0.
    LOOP AT lt_t013 INTO DATA(ls_t013).
      MOVE-CORRESPONDING ls_t013 TO ls_data.
      ls_data-heqnr = ls_t013-posnr.

      CALL FUNCTION 'CONVERSION_EXIT_GERNR_OUTPUT'
        EXPORTING
          input  = ls_data-sernr
        IMPORTING
          output = ls_data-sernr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_data-equnr
        IMPORTING
          output = ls_data-equnr.

      APPEND ls_data TO gtc_data.
      CLEAR : ls_t013, ls_data.
    ENDLOOP.
  ENDIF.

  SELECT FROM zpm_t014 AS a
  INNER JOIN mkpf AS b ON b~mjahr = a~mjahr
                       AND b~mblnr = a~mblnr
  INNER JOIN mseg AS c ON c~mjahr = b~mjahr
                       AND c~mblnr = b~mblnr
  INNER JOIN makt AS d ON d~matnr = a~matnr
  FIELDS a~aufnr, a~werks, b~bldat, b~budat, a~mjahr, a~mblnr, c~zeile,
         a~bwart, c~sobkz, a~lgort, a~matnr, c~erfmg, c~erfme, a~charg, a~disdt, a~distm,
         c~bwtar, a~sernr, a~equnl, a~heqnr, a~hequi, c~usnam_mkpf, d~maktx
  WHERE a~aufnr EQ @p_aufnr
    AND a~werks EQ @p_werks
    AND mrdel NE 'X'
  INTO TABLE @DATA(lt_t014).
  IF sy-subrc = 0.
    LOOP AT lt_t014 INTO DATA(ls_t014).
      MOVE-CORRESPONDING ls_t014 TO ls_data.
      ls_data-equnr = ls_t014-equnl.

      CALL FUNCTION 'CONVERSION_EXIT_GERNR_OUTPUT'
        EXPORTING
          input  = ls_data-sernr
        IMPORTING
          output = ls_data-sernr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_data-equnr
        IMPORTING
          output = ls_data-equnr.

      APPEND ls_data TO gtc_data.
      CLEAR : ls_t014, ls_data.
    ENDLOOP.
  ENDIF.

  IF gtc_data[] IS NOT INITIAL.
    SORT gtc_data BY werks ASCENDING bldat ASCENDING budat ASCENDING
                     mjahr ASCENDING mblnr zeile ASCENDING.
    DELETE ADJACENT DUPLICATES FROM gtc_data COMPARING werks bldat budat mjahr mblnr zeile.
  ELSE.
    pc_lv_message =  'Data not found'. pc_lv_err = 'X'.
  ENDIF.

ENDFORM.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_MAIN' ITSELF
CONTROLS: tc_main TYPE TABLEVIEW USING SCREEN 0100.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_MAIN'
DATA:     g_tc_main_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_MAIN'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_main_change_tc_attr OUTPUT.
  DESCRIBE TABLE gtc_data LINES tc_main-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_MAIN'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_main_get_lines OUTPUT.
  g_tc_main_lines = sy-loopc.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TC_MAIN'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc_main_mark INPUT.
  DATA: g_tc_main_wa2 LIKE LINE OF gtc_data.
  IF tc_main-line_sel_mode = 1
  AND gsc_data-fselc = 'X'.
    LOOP AT gtc_data INTO g_tc_main_wa2
      WHERE fselc = 'X'.
      g_tc_main_wa2-fselc = ''.
      MODIFY gtc_data
        FROM g_tc_main_wa2
        TRANSPORTING fselc.
    ENDLOOP.
  ENDIF.
  MODIFY gtc_data
    FROM gsc_data
    INDEX tc_main-current_line
    TRANSPORTING fselc.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC_MAIN'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_main_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_MAIN'
                              'GTC_DATA'
                              'FSELC'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.

  IF p_ok EQ 'CANCEL' OR p_ok EQ 'MESLOG'.
    l_ok = p_ok.
  ELSE.
    SEARCH p_ok FOR p_tc_name.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    l_offset = strlen( p_tc_name ) + 1.
    l_ok = p_ok+l_offset.
  ENDIF.

  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.
    WHEN 'CANCEL'.
      PERFORM f_cancel_proc.

    WHEN 'MESLOG'.
      IF gt_err[] IS NOT INITIAL.
        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = gt_err[].
      ENDIF.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZST100'.
  SET TITLEBAR 'ZTT100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_exit INPUT.
  CLEAR ok_code.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      PERFORM f_exit.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_exit
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exit .

  DATA  : lv_ans.
  CLEAR : lv_ans.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Exit Document'
      text_question         = 'Do you want to exit?'
      text_button_1         = 'Yes'
      text_button_2         = 'No'
      default_button        = '1'
      display_cancel_button = 'X'
      popup_type            = 'ICON_MESSAGE_WARNING'
    IMPORTING
      answer                = lv_ans.

  CASE lv_ans.
    WHEN '1'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_cancel_proc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_cancel_proc .

  DATA lcl_uid TYPE REF TO cl_system_uuid.

  DATA : lt_t015     TYPE TABLE OF zpm_t015,
         ls_t015     TYPE zpm_t015,
         lt_equip    TYPE gtt_equip,
         ls_equip    TYPE LINE OF gtt_equip,
         lt_data     TYPE gtt_tdata,
         ls_ctuparam TYPE ctu_params,
         ls_err      TYPE gty_err.

  DATA : lv_belnr        TYPE mblnr,
         lv_gjahr        TYPE mjahr,
         lv_buzei        TYPE mblpo,
         lv_message(255) TYPE c,
         lv_tcode(15)    TYPE c,
         lv_err.

  DATA : BEGIN OF ls_bapi,
           materialdocument    TYPE  bapi2017_gm_head_02-mat_doc,
           matdocumentyear     TYPE  bapi2017_gm_head_02-doc_year,
           goodsmvt_pstng_date TYPE  bapi2017_gm_head_02-pstng_date,
           goodsmvt_pr_uname   TYPE  bapi2017_gm_head_01-pr_uname,
           goodsmvt_headret    LIKE  bapi2017_gm_head_ret,
           return              TYPE TABLE OF bapiret2,
           goodsmvt_matdocitem TYPE TABLE OF bapi2017_gm_item_04,
         END OF ls_bapi.

  CLEAR : lt_t015[], ls_t015, lt_data[], ls_bapi, lt_equip[], ls_equip,
          bdc_data[], bdc_messtab[], ls_ctuparam, gt_err[], ls_err,
          lv_belnr, lv_gjahr, lv_buzei, lv_message, lv_tcode, lv_err.

  lt_data[] = gtc_data.
  SORT lt_data BY fselc ASCENDING.
  DELETE lt_data WHERE fselc NE 'X'.

  IF lt_data[] IS NOT INITIAL.

    SELECT FROM equi AS a
      INNER JOIN imptt AS b ON b~mpobj = a~objnr
      INNER JOIN imrg AS c ON c~point = b~point
      FIELDS a~equnr, a~objnr, b~point, b~psort
      FOR ALL ENTRIES IN @lt_data
      WHERE a~equnr = @lt_data-equnr
        AND b~lvorm NE 'X'
        AND b~psort IN ('HOURMETER', 'KILOMETER')
      INTO TABLE @lt_equip.
    SORT lt_equip BY equnr ASCENDING.

    IF lcl_uid IS NOT BOUND.
      CREATE OBJECT lcl_uid.
    ENDIF.

    DATA(lv_uuid) = lcl_uid->create_uuid_c36_static( ).

    LOOP AT lt_data INTO DATA(ls_data) FROM sy-tabix.

      DATA(lv_tabix) = sy-tabix.

      ls_bapi-materialdocument = ls_data-mblnr.
      ls_bapi-matdocumentyear  = ls_data-mjahr.
      ls_bapi-goodsmvt_pstng_date = ls_data-budat.

      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument    = ls_bapi-materialdocument
          matdocumentyear     = ls_bapi-matdocumentyear
          goodsmvt_pstng_date = ls_bapi-goodsmvt_pstng_date
        IMPORTING
          goodsmvt_headret    = ls_bapi-goodsmvt_headret
        TABLES
          return              = ls_bapi-return
          goodsmvt_matdocitem = ls_bapi-goodsmvt_matdocitem.

      IF ls_bapi-goodsmvt_headret IS NOT INITIAL.

        COMMIT WORK AND WAIT.

        ls_err-msgid  = 'M7'.
        ls_err-msgty  = 'S'.
        ls_err-msgno  = '060'.
        ls_err-msgv1  = |tr 1 - { ls_bapi-goodsmvt_headret-mat_doc }{ ls_bapi-goodsmvt_headret-doc_year }|.
        ls_err-lineno = lv_tabix.
        APPEND ls_err TO gt_err.
        CLEAR ls_err.

        "Prepare to saved table
        MOVE-CORRESPONDING ls_data TO ls_t015.

        READ TABLE ls_bapi-goodsmvt_matdocitem INTO DATA(ls_item) INDEX 1.

        ls_t015-uid32 = lv_uuid.
        lv_belnr = ls_t015-code1 = ls_t015-mblnr = ls_bapi-goodsmvt_headret-mat_doc.
        lv_gjahr = ls_t015-code2 = ls_t015-mjahr = ls_bapi-goodsmvt_headret-doc_year.
        lv_buzei = ls_t015-zeile = ls_item-matdoc_item.
        ls_t015-ernam = sy-uname.
        ls_t015-erdat = sy-datum.
        ls_t015-erzet = sy-uzeit.

        APPEND ls_t015 TO lt_t015.

        CASE ls_data-bwart.
          WHEN '261'.
            UPDATE zpm_t013 SET belnr = lv_belnr
                                gjahr = lv_gjahr
                                buzei = lv_buzei
                                mrdel = 'X'
                            WHERE aufnr EQ ls_data-aufnr
                              AND werks EQ ls_data-werks
                              AND mblnr EQ ls_data-mblnr
                              AND mjahr EQ ls_data-mjahr.
          WHEN '262'.
            UPDATE zpm_t014 SET belnr = lv_belnr
                                gjahr = lv_gjahr
                                buzei = lv_buzei
                                mrdel = 'X'
                            WHERE aufnr EQ ls_data-aufnr
                              AND werks EQ ls_data-werks
                              AND mblnr EQ ls_data-mblnr
                              AND mjahr EQ ls_data-mjahr.
        ENDCASE.

        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ELSE.
          ROLLBACK WORK.
        ENDIF.

        ls_ctuparam-dismode = 'N'.

        CASE ls_data-bwart.
          WHEN '261'.
            PERFORM f_next_261 USING ls_data ls_ctuparam lv_tabix.
          WHEN '262'.
            PERFORM f_next_262 USING ls_data ls_ctuparam lt_equip lv_tabix.
        ENDCASE.

      ELSE.

        ROLLBACK WORK.

        LOOP AT ls_bapi-return INTO DATA(ls_return).

          ls_err-msgid  = ls_return-id.
          ls_err-msgty  = ls_return-type.
          ls_err-msgno  = ls_return-number.
          ls_err-msgv1  = |tr 1 - { ls_return-message_v1 }|.
          ls_err-msgv2  = ls_return-message_v2.
          ls_err-msgv3  = ls_return-message_v3.
          ls_err-msgv4  = ls_return-message_v4.
          ls_err-lineno = lv_tabix.
          APPEND ls_err TO gt_err.

          CLEAR : ls_return, ls_err.
        ENDLOOP.

      ENDIF.

      CLEAR : ls_err, ls_data, ls_bapi, bdc_data[], bdc_messtab[], lv_message, lv_tcode.
    ENDLOOP.

    IF lt_t015[] IS NOT INITIAL.
      INSERT zpm_t015 FROM TABLE lt_t015[].
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.

        PERFORM f_getdata CHANGING lv_message lv_err.

        MESSAGE s000(0k) WITH 'Data saved successfully'.
      ELSE.
        ROLLBACK WORK.
        MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'Data not saved successfully'.
      ENDIF.
    ELSE.
      MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'No data is saved, check message log'.
    ENDIF.

  ELSE.
    MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'Please select a line'.
  ENDIF.

ENDFORM.

FORM bdc_dynpro USING program dynpro.
  CLEAR bdc_data.
  bdc_data-program  = program.
  bdc_data-dynpro   = dynpro.
  bdc_data-dynbegin = 'X'.
  APPEND bdc_data.
ENDFORM.

FORM bdc_field USING fnam fval.
  CLEAR bdc_data.
  bdc_data-fnam = fnam.
  bdc_data-fval = fval.
  APPEND bdc_data.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_bdcmessage
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> BDC_MESSTAB
*&      <-- LV_MESSAGE
*&---------------------------------------------------------------------*
FORM f_get_bdcmessage  USING    pu_lv_tcode   TYPE char15
                       CHANGING pc_lv_message TYPE char255.

  READ TABLE bdc_messtab WITH KEY tcode = pu_lv_tcode msgtyp = 'S'.
  IF sy-subrc EQ 0.
    pc_lv_message = bdc_messtab-msgv1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_next_261
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA
*&      --> LS_CTUPARAM
*&---------------------------------------------------------------------*
FORM f_next_261 USING pu_ls_data     TYPE gty_tdata
                      pu_ls_ctuparam TYPE ctu_params
                      pu_lv_tabix    TYPE sy-tabix.

  DATA  : lv_message TYPE char255,
          ls_err     TYPE LINE OF gtt_err,
          lv_uzeit   TYPE sy-uzeit.
  CLEAR : lv_message, ls_err.

  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'RM63E-EQUNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '/00'.
  PERFORM bdc_field       USING 'RM63E-EQUNR'       pu_ls_data-equnr.

  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=T\04'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'ITOB-SHTXT'.

  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=CIPL'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'ITOB-SHTXT'.

  PERFORM bdc_dynpro      USING 'SAPLIEL2'          '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'IEQINSTALL-UZEIT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=IPDL'.
  DATA(lv_insdt) = |{ pu_ls_data-insdt+6(2) }{ pu_ls_data-insdt+4(2) }{ pu_ls_data-insdt(4) }|.
  PERFORM bdc_field       USING 'IEQINSTALL-DATUM'  lv_insdt.
  lv_uzeit = pu_ls_data-instm + 1.
  PERFORM bdc_field       USING 'IEQINSTALL-UZEIT'  lv_uzeit. "pu_ls_data-instm.

  PERFORM bdc_dynpro      USING 'SAPLIEL2'          '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'IEQINSTALL-TPLNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=EXIT'.

  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=BU'.

  CALL TRANSACTION 'IE02' USING bdc_data OPTIONS FROM pu_ls_ctuparam
                                           MESSAGES INTO bdc_messtab.

  COMMIT WORK AND WAIT.
  WAIT UP TO 2 SECONDS.

  PERFORM f_get_bdcmessage USING 'IE02' CHANGING lv_message.
  LOOP AT bdc_messtab.
    ls_err-msgid  = bdc_messtab-msgid.
    ls_err-msgty  = bdc_messtab-msgtyp.
    ls_err-msgno  = bdc_messtab-msgnr.
    ls_err-msgv1  = |tr 2 - { bdc_messtab-msgv1 }|.
    ls_err-msgv2  = bdc_messtab-msgv2.
    ls_err-msgv3  = bdc_messtab-msgv3.
    ls_err-msgv4  = bdc_messtab-msgv4.
    IF sy-tabix = 1.
      ls_err-lineno = sy-tabix.
    ELSE.
      ls_err-lineno = pu_lv_tabix.
    ENDIF.
    APPEND ls_err TO gt_err.
    CLEAR ls_err.
  ENDLOOP.

  CLEAR : bdc_data[], bdc_messtab[], lv_message.

  PERFORM bdc_dynpro      USING 'SAPMIEQ0' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RM63E-EQUNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RM63E-EQUNR'
                                pu_ls_data-equnr.
  PERFORM bdc_dynpro      USING 'SAPMIEQ0' '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=T\05'.
  PERFORM bdc_dynpro      USING 'SAPMIEQ0' '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'ITOB-DATLWB'.
  PERFORM bdc_field       USING 'ITOB-DATLWB'
                                ''.
  CALL TRANSACTION 'IE02' USING bdc_data OPTIONS FROM pu_ls_ctuparam
                                              MESSAGES INTO bdc_messtab.
  PERFORM f_get_bdcmessage USING 'IE02' CHANGING lv_message.
  LOOP AT bdc_messtab.
    ls_err-msgid  = bdc_messtab-msgid.
    ls_err-msgty  = bdc_messtab-msgtyp.
    ls_err-msgno  = bdc_messtab-msgnr.
    ls_err-msgv1  = |tr 3 - { bdc_messtab-msgv1 }|.
    ls_err-msgv2  = bdc_messtab-msgv2.
    ls_err-msgv3  = bdc_messtab-msgv3.
    ls_err-msgv4  = bdc_messtab-msgv4.
    IF sy-tabix = 1.
      ls_err-lineno = sy-tabix.
    ELSE.
      ls_err-lineno = pu_lv_tabix.
    ENDIF.
    APPEND ls_err TO gt_err.
    CLEAR ls_err.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_numc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_RETURN_MESSAGE_V1
*&      <-- LS_T015_CODE1
*&      <-- LS_T015_CODE1
*&---------------------------------------------------------------------*
FORM f_set_numc  USING    pu_ls_return_message_v1 TYPE symsgv
                 CHANGING pc_ls_t015_code1 TYPE zcode1
                          pc_ls_t015_code2 TYPE zcode2.

  DATA: lv_int TYPE i,
        lv_len TYPE i.

  lv_len = strlen( pu_ls_return_message_v1 ).

  CLEAR lv_int.

  WHILE lv_int < lv_len.
    IF NOT pu_ls_return_message_v1+lv_int(1) CO '0123456789'.
      MOVE space TO pu_ls_return_message_v1+lv_int(1).
    ENDIF.
    ADD 1 TO lv_int.
  ENDWHILE.
  CONDENSE pu_ls_return_message_v1 NO-GAPS.
  pc_ls_t015_code1 = pu_ls_return_message_v1(10).
  pc_ls_t015_code2 = pu_ls_return_message_v1+10(4).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_next_262
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_TCODE
*&      --> LS_DATA
*&      --> LS_CTUPARAM
*&---------------------------------------------------------------------*
FORM f_next_262 USING pu_ls_data     TYPE gty_tdata
                      pu_ls_ctuparam TYPE ctu_params
                      pu_lt_equip    TYPE gtt_equip
                      pu_lv_tabix    TYPE sy-tabix.

  DATA  : lv_message(255) TYPE c,
          ls_err          TYPE LINE OF gtt_err,
          lv_uzeit        TYPE sy-uzeit.
  CLEAR : bdc_data[], bdc_messtab[], lv_message, ls_err, lv_uzeit.

  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'RM63E-EQUNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '/00'.
  PERFORM bdc_field       USING 'RM63E-EQUNR'       pu_ls_data-equnr.

  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=T\04'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'ITOB-SHTXT'.

  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=CIPL'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'ITOB-SHTXT'.

  PERFORM bdc_dynpro      USING 'SAPLIEL2'          '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'IEQINSTALL-UZEIT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=EXIT'.

  PERFORM bdc_field       USING 'IEQINSTALL-HEQNR'  pu_ls_data-hequi.
  PERFORM bdc_field       USING 'IEQINSTALL-HSTPS'  pu_ls_data-heqnr.
  DATA(lv_disdt) = |{ pu_ls_data-disdt+6(2) }{ pu_ls_data-disdt+4(2) }{ pu_ls_data-disdt(4) }|.
  PERFORM bdc_field       USING 'IEQINSTALL-DATUM'  lv_disdt.
  lv_uzeit = pu_ls_data-distm + 1.
  PERFORM bdc_field       USING 'IEQINSTALL-UZEIT'  lv_uzeit.

  " BDC baru
*  PERFORM bdc_dynpro      USING 'SAPLIEL2'          '0100'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'        'IEQINSTALL-TPLNR'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'        '=EXIT'.
*
*  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'        '=T\05'.
*
*  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'        '/00'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'        'ITOB-DATLWB'.
*  PERFORM bdc_field       USING 'ITOB-DATLWB'        ''.
  " End BDC baru

  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=BU'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'ITOB-SHTXT'.

  CALL TRANSACTION 'IE02' USING bdc_data OPTIONS FROM pu_ls_ctuparam
                                           MESSAGES INTO bdc_messtab.

  COMMIT WORK AND WAIT.

  PERFORM f_get_bdcmessage USING 'IE02' CHANGING lv_message.
  LOOP AT bdc_messtab.
    ls_err-msgid  = bdc_messtab-msgid.
    ls_err-msgty  = bdc_messtab-msgtyp.
    ls_err-msgno  = bdc_messtab-msgnr.
    ls_err-msgv1  = |tr 2 - { bdc_messtab-msgv1 }|.
    ls_err-msgv2  = bdc_messtab-msgv2.
    ls_err-msgv3  = bdc_messtab-msgv3.
    ls_err-msgv4  = bdc_messtab-msgv4.
    IF sy-tabix = 1.
      ls_err-lineno = sy-tabix.
    ELSE.
      ls_err-lineno = pu_lv_tabix.
    ENDIF.
    APPEND ls_err TO gt_err.
    CLEAR ls_err.
  ENDLOOP.

  CLEAR : bdc_data[], bdc_messtab[], lv_message.

  READ TABLE pu_lt_equip TRANSPORTING NO FIELDS
  WITH KEY equnr = pu_ls_data-equnr BINARY SEARCH.
  IF sy-subrc = 0.
    LOOP AT pu_lt_equip INTO DATA(ls_data) FROM sy-tabix.
      IF ls_data-equnr NE pu_ls_data-equnr.
        EXIT.
      ENDIF.

      PERFORM bdc_dynpro USING 'SAPLIMR0'                      '1110'.
      PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-POINT'.
      PERFORM bdc_field  USING 'BDC_OKCODE'                    '=GDPT'.
      PERFORM bdc_field  USING 'IMPT-POINT'                    ls_data-point.

      PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
      PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-PSORT'.
      PERFORM bdc_field  USING 'BDC_OKCODE'                    '=ADPT'.
      PERFORM bdc_field  USING 'IMPT-INDCT'                    'X'.
      PERFORM bdc_field  USING 'IMPT-INDTR'                    'X'.

      PERFORM bdc_dynpro USING 'SAPLIMR0'                      '6110'.
      PERFORM bdc_field  USING 'BDC_CURSOR'                    'RIMR0-MRMAC'.
      PERFORM bdc_field  USING 'BDC_OKCODE'                    '=TRDF'.
      PERFORM bdc_field  USING 'IMPT-INDTR'                    'X'.

      PERFORM bdc_dynpro USING 'SAPLIMR0'                      '6320'.
      PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPH-TRANS'.
      PERFORM bdc_field  USING 'BDC_OKCODE'                    '=NEXT'.

      PERFORM bdc_dynpro USING 'SAPLIMR0'                      '6110'.
      PERFORM bdc_field  USING 'BDC_CURSOR'                    'RIMR0-MRMAC'.
      PERFORM bdc_field  USING 'BDC_OKCODE'                    '=NEXT'.

      PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
      PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-PSORT'.
      PERFORM bdc_field  USING 'BDC_OKCODE'                    '=BU'.

      CALL TRANSACTION 'IK02' USING bdc_data OPTIONS FROM pu_ls_ctuparam
                                                  MESSAGES INTO bdc_messtab.
      PERFORM f_get_bdcmessage USING 'IK02' CHANGING lv_message.
      LOOP AT bdc_messtab.
        ls_err-msgid  = bdc_messtab-msgid.
        ls_err-msgty  = bdc_messtab-msgtyp.
        ls_err-msgno  = bdc_messtab-msgnr.
        ls_err-msgv1  = |tr 3 - { bdc_messtab-msgv1 }|.
        ls_err-msgv2  = bdc_messtab-msgv2.
        ls_err-msgv3  = bdc_messtab-msgv3.
        ls_err-msgv4  = bdc_messtab-msgv4.
        IF sy-tabix = 1.
          ls_err-lineno = sy-tabix.
        ELSE.
          ls_err-lineno = pu_lv_tabix.
        ENDIF.
        APPEND ls_err TO gt_err.
        CLEAR ls_err.
      ENDLOOP.

      CLEAR : ls_data, bdc_data[], bdc_messtab[], lv_message.
    ENDLOOP.
  ENDIF.

  CLEAR : lv_message, ls_err.

  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'RM63E-EQUNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '/00'.
  PERFORM bdc_field       USING 'RM63E-EQUNR'       pu_ls_data-equnr.

  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=T\05'.

  PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=BU'.
  PERFORM bdc_field       USING 'BDC_CURSOR'        'ITOB-DATLWB'.
  PERFORM bdc_field       USING 'ITOB-DATLWB'       ''.

  CALL TRANSACTION 'IE02' USING bdc_data OPTIONS FROM pu_ls_ctuparam
                                         MESSAGES INTO bdc_messtab.
  PERFORM f_get_bdcmessage USING 'IE02' CHANGING lv_message.
  LOOP AT bdc_messtab.
    ls_err-msgid  = bdc_messtab-msgid.
    ls_err-msgty  = bdc_messtab-msgtyp.
    ls_err-msgno  = bdc_messtab-msgnr.
    ls_err-msgv1  = |tr 3 - { bdc_messtab-msgv1 }|.
    ls_err-msgv2  = bdc_messtab-msgv2.
    ls_err-msgv3  = bdc_messtab-msgv3.
    ls_err-msgv4  = bdc_messtab-msgv4.
    IF sy-tabix = 1.
      ls_err-lineno = sy-tabix.
    ELSE.
      ls_err-lineno = pu_lv_tabix.
    ENDIF.
    APPEND ls_err TO gt_err.
    CLEAR ls_err.
  ENDLOOP.

ENDFORM.
