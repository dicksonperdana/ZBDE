*&---------------------------------------------------------------------*
*& Report ZPME_DMP_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPME_DMP_001.
TYPE-POOLS icon.

" Global Type
TYPES :
  " Installment Types
  BEGIN OF gty_inshed,
    aufnr TYPE aufnr,
    ktext TYPE auftext,
    rsnum TYPE rsnum,
  END OF gty_inshed,

  BEGIN OF gty_insitem,
    fselc,
    rspos TYPE rspos,
    matnr TYPE matnr,
    maktx TYPE maktx,
    bdmng TYPE bdmng,
    enmng TYPE enmng,
    menge TYPE menge_d,
    meins TYPE meins,
    postp TYPE postp,
    sobkz TYPE sobkz,
    lgort TYPE lgort_d,
    werks TYPE werks_d,
    charg TYPE charg_d,
    point TYPE imrc_point,
    mpobj TYPE imrc_mpobj,
    psort TYPE imrc_psort,
    bwart TYPE bwart,
    messt TYPE zmesst,
  END OF gty_insitem,

  BEGIN OF gty_insequip,
    sernr TYPE gernr,
    posnr TYPE zposn,
    insdt TYPE zinsdt,
    instm TYPE zinstm,
    hequi TYPE hequi,
    kmcnt TYPE zdekm,
    trcnt TYPE zdetread,
*    heqnr TYPE heqnr,
  END OF gty_insequip,

  BEGIN OF gty_datains.
    INCLUDE TYPE gty_inshed.
    INCLUDE TYPE gty_insitem.
    INCLUDE TYPE gty_insequip.
TYPES :
  END OF gty_datains,

  "Dismantle Types
  BEGIN OF gty_dished,
    equnr TYPE equnr,
    sowrk TYPE aufsowrk,
    hmval TYPE zhmval,
    kmval TYPE zkmval,
  END OF gty_dished,

  BEGIN OF gty_disalv.
    INCLUDE STRUCTURE zst_dismantle.
TYPES :
  END OF gty_disalv,

  BEGIN OF gty_err,
    msgid  LIKE sy-msgid,
    msgty  LIKE sy-msgty,
    msgno  LIKE sy-msgno,
    msgv1  LIKE sy-msgv1,
    msgv2  LIKE sy-msgv2,
    msgv3  LIKE sy-msgv3,
    msgv4  LIKE sy-msgv4,
    lineno TYPE msgzeile,
  END OF gty_err,

  BEGIN OF gty_nkey,
    nkey TYPE lvc_nkey,
    indx TYPE sy-index,
  END OF gty_nkey,

  BEGIN OF gty_nbapi,
    equnr LIKE equi-equnr,
    point LIKE imptt-point,
    psort LIKE imptt-psort,
  END OF gty_nbapi.

" Global Table Type
TYPES : gtt_insitem TYPE STANDARD TABLE OF gty_insitem,
        gtt_datains TYPE STANDARD TABLE OF gty_datains,
        gtt_disalv  TYPE STANDARD TABLE OF gty_disalv,
        gtt_err     TYPE STANDARD TABLE OF gty_err,
        gtt_nkey    TYPE SORTED TABLE OF gty_nkey WITH UNIQUE KEY nkey indx,
        gtt_nbapi   TYPE STANDARD TABLE OF gty_nbapi.

" Global data
DATA : gt_outtab     TYPE STANDARD TABLE OF gty_disalv,
       gt_err_ins    TYPE gtt_err,
       gt_err_dis    TYPE gtt_err,
       g_err_equi(1) TYPE c.

" Global data ( Screen )
DATA : gsc_inshed   TYPE gty_inshed,
       gsc_insequip TYPE gty_insequip,
       gsc_dished   TYPE gty_dished,
       gtv_disalv   TYPE gtt_disalv.

" Global data ( table control )
DATA : gttc_insitem TYPE TABLE OF gty_insitem,
       gstc_insitem TYPE gty_insitem.

" Global variable
DATA : ok_code   TYPE sy-ucomm.

" Global ranges
RANGES gtr_psort FOR imptt-psort.

" Tab Strip
CONSTANTS:
  BEGIN OF c_ts_main,
    tab1 LIKE sy-ucomm VALUE 'TS_MAIN_FC1',
    tab2 LIKE sy-ucomm VALUE 'TS_MAIN_FC2',
  END OF c_ts_main,

  gc_expand_nodes TYPE lvc_nkey VALUE '1'.

CONTROLS:  ts_main TYPE TABSTRIP.
DATA:
  BEGIN OF g_ts_main,
    subscreen   LIKE sy-dynnr,
    prog        LIKE sy-repid VALUE 'ZPME_DMP_001',
    pressed_tab LIKE sy-ucomm VALUE c_ts_main-tab1,
  END OF g_ts_main.

" Table Control
CONTROLS: tc_101 TYPE TABLEVIEW USING SCREEN 0101.
DATA:     g_tc_101_lines  LIKE sy-loopc.

" BDC Data
DATA: bdc_data    TYPE TABLE OF bdcdata WITH HEADER LINE,
      bdc_messtab TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.

" ALV
DATA: gc_alv_tree         TYPE REF TO cl_gui_alv_tree,
      gc_custom_container TYPE REF TO cl_gui_custom_container,
      gt_node_key         TYPE gtt_nkey.


SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE TEXT-001.
  PARAMETERS : p_werks TYPE werks_d OBLIGATORY,
               p_aufnr TYPE aufnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl01.

" Installment pop up screen
SELECTION-SCREEN BEGIN OF SCREEN 1011 TITLE TEXT-002.
  PARAMETERS : p_matnr TYPE matnr MODIF ID dis,
               p_eqsnm TYPE gernr MATCHCODE OBJECT eqsnm OBLIGATORY,
               p_posnr TYPE zposn OBLIGATORY.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(33) TEXT-021 FOR FIELD p_insdt MODIF ID cmt.
    PARAMETERS : p_insdt TYPE zinsdt MODIF ID cmt OBLIGATORY,
                 p_instm TYPE zinstm MODIF ID cmt OBLIGATORY.
  SELECTION-SCREEN END OF LINE.
  PARAMETERS  : p_hequi TYPE hequi OBLIGATORY,
                p_kmcnt TYPE zdekm,
                p_trcnt TYPE zdetread.
SELECTION-SCREEN END OF SCREEN 1011.

" Dismantle pop up screen
SELECTION-SCREEN BEGIN OF SCREEN 1021 TITLE TEXT-003.
  PARAMETERS : p_hequ1 TYPE hequi MODIF ID dis,
               p_matn1 TYPE matnr MODIF ID dis,
               p_sern1 TYPE gernr MODIF ID dis,
               p_heqn1 TYPE heqnr MODIF ID dis.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(33) TEXT-022 FOR FIELD p_insdt MODIF ID cmt.
    PARAMETERS : p_disdt TYPE zdisdt MODIF ID cmt OBLIGATORY,
                 p_distm TYPE zdistm MODIF ID cmt OBLIGATORY.
  SELECTION-SCREEN END OF LINE.
  PARAMETERS :
    p_lgort TYPE lgort_d OBLIGATORY,
    p_charg TYPE charg_d MATCHCODE OBJECT h_mcha OBLIGATORY,
    p_reasn TYPE mb_grbew OBLIGATORY,
    p_kmcnr TYPE zdekm,
    p_trcnr TYPE zdetread.
SELECTION-SCREEN END OF SCREEN 1021.

" Rotation pop up screen
SELECTION-SCREEN BEGIN OF SCREEN 1022 TITLE TEXT-004.
  PARAMETERS : p_equip TYPE equnr MODIF ID dis,
               p_rdsfr TYPE equnr MODIF ID dis,
               p_rdsps TYPE heqnr MODIF ID dis.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(33) TEXT-022 FOR FIELD p_rdisdt MODIF ID cmt.
    PARAMETERS : p_rdisdt TYPE zdisdt MODIF ID cmt OBLIGATORY,
                 p_rdistm TYPE zdistm MODIF ID cmt OBLIGATORY.
  SELECTION-SCREEN END OF LINE.
  PARAMETERS : p_rinsto TYPE hequi OBLIGATORY,
               p_rinsps TYPE heqnr OBLIGATORY.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(33) TEXT-021 FOR FIELD p_rinsdt MODIF ID cmt.
    PARAMETERS : p_rinsdt TYPE zinsdt MODIF ID cmt OBLIGATORY,
                 p_rinstm TYPE zinstm MODIF ID cmt OBLIGATORY.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF SCREEN 1022.

INITIALIZATION.
  PERFORM f_set_psortrange.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_screen_output.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_aufnr.
  PERFORM f_searchhelp_aufnr.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_hequi.
  PERFORM f_searchhelp_heqnr USING 'P_HEQUI'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_rinsto.
  PERFORM f_searchhelp_heqnr USING 'P_RINSTO'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lgort.
  PERFORM f_searchhelp_lgort.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_reasn.
  PERFORM f_searchhelp_reasn.

START-OF-SELECTION.
  PERFORM f_at_selscreen.

*&---------------------------------------------------------------------*
*& Form f_screen_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_screen_output .

  DATA lt_hide_ucom TYPE TABLE OF sy-ucomm.

  IF sy-dynnr EQ '1011' OR sy-dynnr EQ '1021' OR sy-dynnr EQ '1022'.
    APPEND 'NONE' TO lt_hide_ucom.
    APPEND 'SPOS' TO lt_hide_ucom.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = '%_CSP'
      TABLES
        p_exclude = lt_hide_ucom.
  ENDIF.

  LOOP AT SCREEN.
    CHECK screen-group1 EQ 'DIS'.
    screen-input = '0'.
    screen-invisible = '0'.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.


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
          lv_message(75) TYPE c.

  CLEAR : lv_err, lv_message.

  SELECT SINGLE FROM aufk
    FIELDS phas0, phas1, phas2, phas3
    WHERE aufnr EQ @p_aufnr
      AND sowrk EQ @p_werks "AND werks EQ @p_werks
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
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'REFRESH'.
      PERFORM f_refresh.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form f_getdata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_MESSAGE
*&      <-- LV_ERR
*&---------------------------------------------------------------------*
FORM f_getdata CHANGING pc_lv_message TYPE char75
                        pc_lv_err.

  DATA  : lv_equnr   LIKE equi-equnr,
          lt_rihequi TYPE TABLE OF rihequi,
          ls_disalv  TYPE gty_disalv,
          ls_insitem TYPE gty_insitem.

  CLEAR : lv_equnr, lt_rihequi, ls_disalv,
          gttc_insitem, gsc_inshed, gsc_dished,
          ls_insitem, gtv_disalv.

  gsc_inshed-aufnr = p_aufnr.
  SELECT SINGLE FROM aufk
    FIELDS ktext
    WHERE aufnr EQ @p_aufnr
    INTO @gsc_inshed-ktext.

  " Get data installment
  SELECT FROM resb AS a
    INNER JOIN makt AS b ON b~matnr = a~matnr
    INNER JOIN marc AS c ON c~matnr = a~matnr
                         AND c~werks = a~werks
    FIELDS rsnum, rspos, a~matnr, maktx, bdmng, enmng, meins,
           postp, sobkz, lgort, a~werks, charg
    WHERE aufnr EQ @p_aufnr
      AND a~werks EQ @p_werks
      AND xwaok EQ 'X'
      AND xloek NE 'X'
      AND kzear NE 'X'
      AND sernp IN ( 'ZS01', 'ZS02' )
    INTO TABLE @DATA(lt_data).

  IF sy-subrc = 0.
    SORT lt_data BY rspos ASCENDING.

    LOOP AT lt_data INTO DATA(ls_data).

      gsc_inshed-rsnum = ls_data-rsnum.

      MOVE-CORRESPONDING ls_data TO ls_insitem.
      ls_insitem-menge = ls_insitem-bdmng - ls_insitem-enmng.
      APPEND ls_insitem TO gttc_insitem.

      CLEAR : ls_data, ls_insitem.
    ENDLOOP.

  ELSE.
    pc_lv_message = 'No Data found'. pc_lv_err = 'X'.
  ENDIF.

  " Get data dismantle
  SELECT FROM afih AS a
    INNER JOIN aufk AS b ON b~aufnr = a~aufnr
    FIELDS equnr, sowrk
    WHERE a~aufnr = @p_aufnr
    INTO TABLE @DATA(lt_affk).
  IF lt_affk[] IS NOT INITIAL.
    SELECT FROM equi AS a
      INNER JOIN imptt AS b ON b~mpobj = a~objnr
      INNER JOIN imrg AS c ON c~point = b~point
      FIELDS a~objnr, b~point, b~psort,
             c~idate, c~itime, c~readg, c~recdv
      FOR ALL ENTRIES IN @lt_affk
      WHERE a~equnr = @lt_affk-equnr
        AND c~lvorm NE 'X'
        AND c~cancl NE 'X'
      INTO TABLE @DATA(lt_eqtt).
    IF sy-subrc = 0.
      SORT lt_eqtt BY objnr ASCENDING point psort ASCENDING
                      idate DESCENDING itime DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_eqtt COMPARING objnr point psort.
      SORT lt_eqtt BY psort ASCENDING.

      LOOP AT lt_affk INTO DATA(ls_affk).

        gsc_dished-equnr = lv_equnr = ls_affk-equnr.
        gsc_dished-sowrk = ls_affk-sowrk.

        READ TABLE lt_eqtt INTO DATA(ls_eqtt)
        WITH KEY psort = 'HOURMETER' BINARY SEARCH.
        IF sy-subrc = 0.
          CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
            EXPORTING
              i_number_of_digits   = 0
              i_fltp_value         = ls_eqtt-recdv "readg
              i_screen_fieldlength = 16
            IMPORTING
              e_char_field         = gsc_dished-hmval.
          CONDENSE gsc_dished-hmval NO-GAPS.
          CLEAR ls_eqtt.
        ENDIF.

        READ TABLE lt_eqtt INTO ls_eqtt
        WITH KEY psort = 'KILOMETER' BINARY SEARCH.
        IF sy-subrc = 0.
          CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
            EXPORTING
              i_number_of_digits   = 0
              i_fltp_value         = ls_eqtt-recdv "readg
              i_screen_fieldlength = 16
            IMPORTING
              e_char_field         = gsc_dished-kmval.
          CONDENSE gsc_dished-kmval NO-GAPS.
        ENDIF.

        CALL FUNCTION 'EQUI_HIERARCHY_READ'
          EXPORTING
            equipment  = lv_equnr
            level_down = '01'
          TABLES
            hier_tab   = lt_rihequi.

        IF lt_rihequi[] IS NOT INITIAL.
          LOOP AT lt_rihequi INTO DATA(ls_rihequi).

            MOVE-CORRESPONDING ls_rihequi TO ls_disalv.
            ls_disalv-equnl = ls_rihequi-equnr.
            CASE ls_disalv-eqtyp.
              WHEN 'T'.
                ls_disalv-typtx = 'TYRE'.
              WHEN 'R'.
                ls_disalv-typtx = 'RIM'.
              WHEN 'C'.
                ls_disalv-typtx = 'COMP'.
              WHEN 'A'.
                ls_disalv-typtx = 'AXLE'.
            ENDCASE.

            APPEND ls_disalv TO gtv_disalv.

            CLEAR ls_rihequi.
          ENDLOOP.
        ENDIF.

        CLEAR : ls_affk, ls_eqtt.
      ENDLOOP.

    ENDIF.
  ENDIF.

  IF gtv_disalv[] IS NOT INITIAL.
    CLEAR : pc_lv_message, pc_lv_err.
  ENDIF.

ENDFORM.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TS_MAIN'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE ts_main_active_tab_set OUTPUT.
  ts_main-activetab = g_ts_main-pressed_tab.
  CASE g_ts_main-pressed_tab.
    WHEN c_ts_main-tab1.
      g_ts_main-subscreen = '0101'.
    WHEN c_ts_main-tab2.
      g_ts_main-subscreen = '0102'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TS_MAIN'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE ts_main_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_ts_main-tab1.
      g_ts_main-pressed_tab = c_ts_main-tab1.
    WHEN c_ts_main-tab2.
      g_ts_main-pressed_tab = c_ts_main-tab2.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_101'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_101_change_tc_attr OUTPUT.
  DESCRIBE TABLE gttc_insitem LINES tc_101-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_101'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_101_get_lines OUTPUT.
  g_tc_101_lines = sy-loopc.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TC_101'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc_101_mark INPUT.
  DATA: g_tc_101_wa2 LIKE LINE OF gttc_insitem.
  IF tc_101-line_sel_mode = 1
  AND gstc_insitem-fselc = 'X'.
    LOOP AT gttc_insitem INTO g_tc_101_wa2
      WHERE fselc = 'X'.
      g_tc_101_wa2-fselc = ''.
      MODIFY gttc_insitem
        FROM g_tc_101_wa2
        TRANSPORTING fselc.
    ENDLOOP.
  ENDIF.
  MODIFY gttc_insitem
    FROM gstc_insitem
    INDEX tc_101-current_line
    TRANSPORTING fselc.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC_101'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_101_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_101'
                              'GTTC_INSITEM'
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

  IF p_ok EQ 'INSTALL' OR p_ok EQ 'MESLOG'.
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
    WHEN 'INSTALL'.
      PERFORM f_install.

    WHEN 'MESLOG'.
      IF gt_err_ins[] IS NOT INITIAL.
        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = gt_err_ins[].
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
  CALL METHOD cl_gui_cfw=>flush.
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
      IF gc_custom_container IS NOT INITIAL.
        CALL METHOD gc_custom_container->free.
      ENDIF.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_install
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_install.

  DATA lcl_uid TYPE REF TO cl_system_uuid.

  DATA  : lv_err,
          lv_message(255) TYPE c.

  DATA  : lt_t013 TYPE TABLE OF zpm_t013,
          ls_t013 TYPE zpm_t013,
          ls_err  TYPE gty_err.

  DATA  : lt_data     TYPE gtt_datains,
          ls_data     TYPE gty_datains,
          lt_insitem  TYPE gtt_insitem,
          ls_insequip TYPE gty_insequip,
          lt_nbapi    TYPE gtt_nbapi.

  DATA : lv_bapi_equipment  TYPE bapi_itob_parms-equipment,
         lv_bapi_material   TYPE bapi_itob_eq_only-material,
         lv_bapi_material_l TYPE bapi_itob_eq_only-material_long,
         lv_bapi_sernum     TYPE bapi_itob_eq_only-serialno,
         ls_bapi_is_install TYPE bapi_itob_eq_install_ext,
         ls_bapi_gdmvment   TYPE bapi_ie4n_goods_movement,
         lt_return          TYPE TABLE OF bapiret2.

  DATA : ls_ctuparam TYPE ctu_params.

  CLEAR : lv_err, lv_message, lt_t013[], ls_t013, lt_data[], ls_data, lt_insitem[], ls_insequip,
          lv_bapi_equipment, lv_bapi_material, lv_bapi_material_l, lv_bapi_sernum,
          ls_bapi_is_install, ls_bapi_gdmvment, lt_return[], bdc_data[], bdc_messtab[],
          ls_ctuparam, gt_err_ins[], ls_err, lt_nbapi[].

  lt_insitem[] = gttc_insitem[].
  SORT lt_insitem BY fselc ASCENDING.

  IF lcl_uid IS NOT BOUND.
    CREATE OBJECT lcl_uid.
  ENDIF.

  READ TABLE lt_insitem TRANSPORTING NO FIELDS
    WITH KEY fselc = 'X' BINARY SEARCH.
  IF sy-subrc = 0.
    LOOP AT lt_insitem INTO DATA(ls_insitem) FROM sy-tabix.
      IF ls_insitem-fselc NE 'X'.
        EXIT.
      ENDIF.

      PERFORM f_set_equipins USING ls_insitem-matnr CHANGING ls_insequip.
      IF sy-subrc NE 0.
        CONTINUE.
      ELSE.
        MOVE-CORRESPONDING : ls_insitem TO ls_data,
                             ls_insequip TO ls_data.
        ls_data-aufnr = p_aufnr.
        ls_data-rsnum = gsc_inshed-rsnum.

        APPEND ls_data TO lt_data.
      ENDIF.

      CLEAR ls_insitem.
    ENDLOOP.

    CHECK lt_data[] IS NOT INITIAL.

    SELECT FROM equi
      FIELDS equnr, matnr, sernr, objnr, eqtyp
      FOR ALL ENTRIES IN @lt_data
      WHERE matnr = @lt_data-matnr
        AND sernr = @lt_data-sernr
      INTO TABLE @DATA(lt_equi).

    IF sy-subrc = 0.

      SORT lt_equi BY matnr sernr ASCENDING.

      SELECT FROM imptt
        FIELDS mpobj, psort, point, lvorm
        FOR ALL ENTRIES IN @lt_equi
        WHERE mpobj = @lt_equi-objnr
        INTO TABLE @DATA(lt_imptt).
      IF sy-subrc = 0.
        SORT lt_imptt BY mpobj ASCENDING psort ASCENDING.
      ENDIF.

      SELECT FROM equi AS a
        INNER JOIN imptt AS b ON b~mpobj = a~objnr
        FIELDS equnr, point, psort
        FOR ALL ENTRIES IN @lt_data
        WHERE equnr = @lt_data-hequi
        INTO TABLE @lt_nbapi.
      IF sy-subrc = 0.
        SORT lt_nbapi BY equnr psort point ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_nbapi COMPARING equnr psort point.
      ENDIF.

      DATA(lv_uuid) = lcl_uid->create_uuid_c36_static( ).

      LOOP AT lt_data INTO ls_data.

        DATA(lv_tabix) = sy-tabix.

        READ TABLE lt_equi INTO DATA(ls_equi)
          WITH KEY matnr = ls_data-matnr sernr = ls_data-sernr BINARY SEARCH.

        IF sy-subrc EQ 0.

          lv_bapi_equipment = ls_equi-equnr.
          lv_bapi_material  = lv_bapi_material_l = ls_data-matnr.
          lv_bapi_sernum    = ls_data-sernr.

          ls_bapi_is_install-supequi    = ls_data-hequi. "heqnr.
          ls_bapi_is_install-inst_pos   = ls_data-posnr.
          ls_bapi_is_install-inst_date  = ls_data-insdt.
          ls_bapi_is_install-inst_time  = ls_data-instm.

          ls_bapi_gdmvment-reserv_no    = ls_data-rsnum.
          ls_bapi_gdmvment-res_item     = ls_data-rspos.
          ls_bapi_gdmvment-move_type    = ls_data-bwart = '261'.
          ls_bapi_gdmvment-pstng_date   = ls_data-insdt.
          ls_bapi_gdmvment-doc_date     = sy-datum.
          ls_bapi_gdmvment-orderid      = ls_data-aufnr.
          ls_bapi_gdmvment-plant        = ls_data-werks.
          ls_bapi_gdmvment-stge_loc     = ls_data-lgort.
          ls_bapi_gdmvment-new_batch    = ls_data-charg.
          ls_bapi_gdmvment-new_val_type = ls_data-charg.

          " Transaksi 1
          CALL FUNCTION 'BAPI_IE4N_INSTALL'
            EXPORTING
              i_equipment            = lv_bapi_equipment
              i_material             = lv_bapi_material
              i_material_long        = lv_bapi_material_l
              i_serialnumber         = lv_bapi_sernum
              is_data_install        = ls_bapi_is_install
              is_data_goods_movement = ls_bapi_gdmvment
            TABLES
              return                 = lt_return.

          READ TABLE lt_return INTO DATA(ls_return)
            WITH KEY type = 'S' id = 'M7' number = '060'.
          IF sy-subrc = 0.
            " Prepare for save to table
            MOVE-CORRESPONDING ls_data TO ls_t013.

            PERFORM f_set_numc USING ls_return-message_v1 CHANGING ls_t013-code1 ls_t013-code2.

            ls_err-msgid  = ls_return-id.
            ls_err-msgty  = ls_return-type.
            ls_err-msgno  = ls_return-number.
            ls_err-msgv1  = |tr 1 - { ls_return-message_v1 }|.
            ls_err-msgv2  = ls_return-message_v2.
            ls_err-msgv3  = ls_return-message_v3.
            ls_err-msgv4  = ls_return-message_v4.
            ls_err-lineno = lv_tabix.
            APPEND ls_err TO gt_err_ins.
            CLEAR ls_err.

            ls_t013-equnr = ls_equi-equnr.
            ls_t013-uid32 = lv_uuid.
            ls_t013-mblnr = |{ ls_t013-code1 }{ ls_t013-code2 }|.
            ls_t013-mjahr = sy-datum(4).
            ls_t013-ernam = sy-uname.
            ls_t013-erdat = sy-datum.
            ls_t013-erzet = sy-uzeit.

            ls_ctuparam-dismode = 'N'.

            READ TABLE gtr_psort TRANSPORTING NO FIELDS WITH KEY low = ls_equi-eqtyp.
            IF sy-subrc = 0.
              LOOP AT gtr_psort FROM sy-tabix.
                IF gtr_psort-low NE ls_equi-eqtyp.
                  EXIT.
                ENDIF.

                READ TABLE lt_imptt INTO DATA(ls_imptt) WITH KEY mpobj = ls_equi-objnr psort = gtr_psort-high BINARY SEARCH.
                IF sy-subrc = 0.

                  CASE gtr_psort-high.
                    WHEN 'TREAD'.
                      IF ls_imptt-lvorm NE 'X'.CONTINUE.ENDIF.
                      PERFORM f_install_tread USING ls_ctuparam ls_equi-equnr ls_imptt-psort ls_err lv_tabix
                                                    ls_data-insdt ls_data-instm ls_data-trcnt
                                                    ls_data-hequi lt_nbapi
                                              CHANGING ls_t013-point ls_t013-kmcnt.
                    WHEN 'PRESSURE'.
                      IF ls_imptt-lvorm NE 'X'.CONTINUE.ENDIF.
                      PERFORM f_install_press USING ls_ctuparam ls_equi-equnr ls_imptt-psort ls_err lv_tabix
                                            CHANGING ls_t013-point.
                    WHEN 'HOURMETER'.
                      PERFORM f_install_hour USING ls_ctuparam ls_equi-equnr ls_imptt-psort ls_err lv_tabix
                                                   ls_imptt-lvorm ls_imptt-point
                                            CHANGING ls_t013-point.
                    WHEN 'KILOMETER'.
                      PERFORM f_install_kilo USING ls_ctuparam ls_equi-equnr ls_imptt-psort ls_err lv_tabix
                                                   ls_data-insdt ls_data-instm ls_data-kmcnt
                                                   ls_data-hequi lt_nbapi
                                                   ls_imptt-lvorm ls_imptt-point
                                            CHANGING ls_t013-point ls_t013-kmcnt.
                  ENDCASE.
                  ADD 1 TO ls_t013-itemc.
                ELSE.
                  CASE gtr_psort-high.
                    WHEN 'TREAD'.
                      PERFORM f_install_tread USING ls_ctuparam ls_equi-equnr gtr_psort-high ls_err lv_tabix
                                                    ls_data-insdt ls_data-instm ls_data-trcnt
                                                    ls_data-hequi lt_nbapi
                                              CHANGING ls_t013-point ls_t013-kmcnt.
                    WHEN 'PRESSURE'.
                      PERFORM f_install_press USING ls_ctuparam ls_equi-equnr gtr_psort-high ls_err lv_tabix
                                            CHANGING ls_t013-point.
                    WHEN 'HOURMETER'.
                      PERFORM f_install_hour USING ls_ctuparam ls_equi-equnr gtr_psort-high ls_err lv_tabix
                                                   ls_imptt-lvorm ls_imptt-point
                                            CHANGING ls_t013-point.
                    WHEN 'KILOMETER'.
                      PERFORM f_install_kilo USING ls_ctuparam ls_equi-equnr gtr_psort-high ls_err lv_tabix
                                                   ls_data-insdt ls_data-instm ls_data-kmcnt
                                                   ls_data-hequi lt_nbapi
                                                   ls_imptt-lvorm ls_imptt-point
                                            CHANGING ls_t013-point ls_t013-kmcnt.
                  ENDCASE.
                  ADD 1 TO ls_t013-itemc.
                ENDIF.

                ls_t013-mpobj = ls_equi-objnr.
                ls_t013-psort = gtr_psort-high.
                IF ls_t013-point IS NOT INITIAL.
                  APPEND ls_t013 TO lt_t013.
                ENDIF.

                CLEAR : ls_err, lv_err, ls_imptt.
              ENDLOOP.
            ENDIF.

          ELSE.

            ROLLBACK WORK.

            LOOP AT lt_return INTO ls_return.
              ls_err-msgid  = ls_return-id.
              ls_err-msgty  = ls_return-type.
              ls_err-msgno  = ls_return-number.
              ls_err-msgv1  = |tr 1 - { ls_return-message_v1 }|.
              ls_err-msgv2  = ls_return-message_v2.
              ls_err-msgv3  = ls_return-message_v3.
              ls_err-msgv4  = ls_return-message_v4.
              ls_err-lineno = lv_tabix.
              APPEND ls_err TO gt_err_ins.
              CLEAR ls_err.
            ENDLOOP.

          ENDIF.

        ELSE.

          ls_err-msgid  = '0k'.
          ls_err-msgty  = 'E'.
          ls_err-msgno  = '000'.
          ls_err-msgv1  = 'Equipment master not found'.
          ls_err-msgv2  = ls_return-message_v2.
          ls_err-msgv3  = ls_return-message_v3.
          ls_err-msgv4  = ls_return-message_v4.
          ls_err-lineno = lv_tabix.
          APPEND ls_err TO gt_err_ins.
          CLEAR ls_err.

        ENDIF.

        CLEAR : ls_data, ls_t013, lv_bapi_equipment, lv_bapi_material, lv_bapi_material_l, lv_bapi_sernum,
                ls_bapi_is_install, ls_bapi_gdmvment, lt_return[], bdc_data[],
                bdc_messtab[], ls_ctuparam, ls_err.
      ENDLOOP.

    ELSE.
      g_err_equi = 'X'.
    ENDIF.

    IF lt_t013[] IS NOT INITIAL.
      INSERT zpm_t013 FROM TABLE lt_t013[].
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.

        PERFORM f_refresh.

        MESSAGE s000(0k) WITH 'Data saved successfully'.
      ELSE.
        ROLLBACK WORK.
        MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'Data not saved successfully'.
      ENDIF.
    ELSE.
      IF g_err_equi IS NOT INITIAL.
        MESSAGE 'Equipment not found' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR : g_err_equi.
      ELSE.
        MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'No data is saved - check message log'.
      ENDIF.
    ENDIF.

  ELSE.
    MESSAGE i000(0k) WITH 'Please select a line'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_set_equipins
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_INSEQUIP
*&---------------------------------------------------------------------*
FORM f_set_equipins USING pu_lv_matnr TYPE matnr
                    CHANGING pc_ls_insequip TYPE gty_insequip.

  CLEAR : p_matnr, p_eqsnm, p_posnr, p_insdt, p_instm, p_hequi,
          p_kmcnt, p_trcnt."p_heqnr.

  p_matnr = pu_lv_matnr.
  CALL SELECTION-SCREEN 1011 STARTING AT 40 3.
  pc_ls_insequip-sernr = p_eqsnm.
  pc_ls_insequip-posnr = p_posnr.
  pc_ls_insequip-insdt = p_insdt.
  pc_ls_insequip-instm = p_instm.
  pc_ls_insequip-hequi = p_hequi.
  pc_ls_insequip-kmcnt = p_kmcnt.
  pc_ls_insequip-trcnt = p_trcnt.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_searchhelp_heqnr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_searchhelp_heqnr USING pu_val TYPE char12.

  DATA : lv_equnr LIKE equi-equnr.

  DATA  : lt_equi TYPE TABLE OF rihequi.

  DATA : BEGIN OF ls_f4values,
           value TYPE hequi,
           text  TYPE equnr,
         END OF ls_f4values.

  DATA  : ls_f4fiel TYPE dfies,
          lt_f4tab  LIKE TABLE OF ls_f4values WITH HEADER LINE,
          lt_f4fiel TYPE TABLE OF dfies,
          lt_f4ret  TYPE TABLE OF ddshretval WITH HEADER LINE,
          lt_dyntab TYPE TABLE OF dselc WITH HEADER LINE,
          p_dyn     TYPE help_info-dynprofld.

  CLEAR : lv_equnr, lt_equi[], ls_f4fiel, lt_f4tab[],
          lt_f4fiel[], lt_f4ret[], lt_dyntab[], p_dyn.

  SELECT SINGLE FROM afih
    FIELDS equnr
    WHERE aufnr EQ @p_aufnr
    INTO @lv_equnr.

  CALL FUNCTION 'EQUI_HIERARCHY_READ'
    EXPORTING
      equipment  = lv_equnr
      level_down = '01'
    TABLES
      hier_tab   = lt_equi.

  IF lt_equi[] IS NOT INITIAL.

    ls_f4fiel-tabname = 'RIHEQUI'.
    ls_f4fiel-fieldname = 'HEQUI'. "'HEQNR'.
    ls_f4fiel-position = '1'.
    APPEND ls_f4fiel TO lt_f4fiel.

*    ls_f4fiel-tabname = 'RIHEQUI'.
*    ls_f4fiel-fieldname = 'EQUNR'. "'HEQNR'.
*    ls_f4fiel-position = '2'.
*    APPEND ls_f4fiel TO lt_f4fiel.

    LOOP AT lt_equi INTO DATA(ls_equi).
      "ls_f4values-value = ls_equi-heqnr. APPEND ls_f4values TO lt_f4tab.
      ls_f4values-value = ls_equi-equnr.
*      ls_f4values-text = ls_equi-equnr.
      APPEND ls_f4values TO lt_f4tab.
      CLEAR : ls_equi, ls_f4values.
    ENDLOOP.

    lt_dyntab-fldname = p_dyn = pu_val.
    APPEND lt_dyntab.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'HEQUI'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = p_dyn
      TABLES
        value_tab       = lt_f4tab
        field_tab       = lt_f4fiel
        return_tab      = lt_f4ret
        dynpfld_mapping = lt_dyntab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

  ELSE.
    MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'No Data found'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_refresh
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_refresh .
  DATA : lv_message(75) TYPE c,
         lv_err.

  CLEAR : lv_message, lv_err.

  PERFORM f_getdata CHANGING lv_message lv_err.

  IF gc_alv_tree IS NOT INITIAL.
    PERFORM f_updtree.
  ENDIF.

  IF lv_err IS NOT INITIAL.
    MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'Data not found'.
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
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN 'DISMANTLE'.
      PERFORM f_dismantle.
    WHEN 'ROTATION'.
      PERFORM f_rotation.
    WHEN 'MESLOG'.
      IF gt_err_dis[] IS NOT INITIAL.
        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = gt_err_dis[].
      ENDIF.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form f_dismantle
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_dismantle .

  DATA lcl_uid TYPE REF TO cl_system_uuid.

  DATA : lt_disalv TYPE gtt_disalv,
         lt_t014   TYPE TABLE OF zpm_t014,
         ls_t014   TYPE zpm_t014,
         ls_err    TYPE gty_err.

  DATA : lt_node_keys TYPE lvc_t_nkey,
         lt_nbapi     TYPE gtt_nbapi.

  DATA :
    BEGIN OF ls_bapi,
      equipment              TYPE bapi_itob_parms-equipment,
      material               TYPE bapi_itob_eq_only-material,
      material_long          TYPE bapi_itob_eq_only-material_long,
      serialnumber           TYPE bapi_itob_eq_only-serialno,
      i_dism_date            TYPE bapi_itob_eq_install_ext-inst_date,
      i_dism_time            TYPE bapi_itob_eq_install_ext-inst_time,
      is_data_goods_movement TYPE bapi_ie4n_goods_movement,
      return                 TYPE TABLE OF bapiret2,
    END OF ls_bapi.

  CLEAR : lt_disalv[], lt_t014[], ls_t014, lt_node_keys[], ls_bapi, lt_nbapi[],
          gt_err_dis[], ls_err.

  CALL METHOD gc_alv_tree->get_selected_nodes
    CHANGING
      ct_selected_nodes = lt_node_keys.

  IF lt_node_keys[] IS INITIAL.
    MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'Please select a line'.
  ELSE.

    lt_disalv[] = gtv_disalv[].
    SORT lt_disalv BY hequi ASCENDING.

    LOOP AT lt_node_keys INTO DATA(ls_node_keys).

      READ TABLE gt_node_key INTO DATA(ls_nkey) WITH KEY nkey = ls_node_keys.

      READ TABLE gtv_disalv INTO DATA(ls_disalv) INDEX ls_nkey-indx.

      p_hequ1 = ls_disalv-equnl.
      p_matn1 = ls_disalv-matnr.
      p_sern1 = ls_disalv-sernr.
      p_heqn1 = ls_disalv-heqnr.

      CLEAR : p_disdt, p_distm, p_lgort, p_charg, p_reasn, p_kmcnr, p_trcnr.
      CALL SELECTION-SCREEN 1021 STARTING AT 40 3.
      IF sy-subrc NE 0.
        CONTINUE.
      ELSE.
        MOVE-CORRESPONDING ls_disalv TO ls_t014.
        ls_t014-aufnr = gsc_inshed-aufnr.
        ls_t014-rsnum = gsc_inshed-rsnum.
        ls_t014-hmval = gsc_dished-hmval.
        ls_t014-kmval = gsc_dished-kmval.
        ls_t014-disdt = p_disdt.
        ls_t014-distm = p_distm.
        ls_t014-lgort = p_lgort.
        ls_t014-charg = p_charg.
        ls_t014-reasn = p_reasn.
        ls_t014-kmcnt = p_kmcnr.
        ls_t014-trcnt = p_trcnr.
        APPEND ls_t014 TO lt_t014.
      ENDIF.

      CLEAR ls_node_keys.
    ENDLOOP.

    IF lt_t014[] IS NOT INITIAL.

      SELECT FROM equi AS a
        INNER JOIN imptt AS b ON b~mpobj = a~objnr
        FIELDS equnr, b~point, psort
        FOR ALL ENTRIES IN @lt_t014
        WHERE ( equnr = @lt_t014-hequi OR equnr = @lt_t014-equnl )
          AND psort IN ( 'KILOMETER', 'TREAD' )
        INTO TABLE @lt_nbapi.
      IF sy-subrc = 0.
        SORT lt_nbapi BY equnr psort ASCENDING.
      ENDIF.

      IF lcl_uid IS NOT BOUND.
        CREATE OBJECT lcl_uid.
      ENDIF.
      DATA(lv_uuid) = lcl_uid->create_uuid_c36_static( ).

      LOOP AT lt_t014 ASSIGNING FIELD-SYMBOL(<fs_t014>).

        DATA(lv_tabix) = sy-tabix.

        ls_bapi-equipment     = <fs_t014>-equnl.
        ls_bapi-material      = ls_bapi-material_long = <fs_t014>-matnr.
        ls_bapi-serialnumber  = <fs_t014>-sernr.
        ls_bapi-i_dism_date   = <fs_t014>-disdt.
        ls_bapi-i_dism_time   = <fs_t014>-distm.

        ls_bapi-is_data_goods_movement-move_type    = <fs_t014>-bwart = '262'.
        ls_bapi-is_data_goods_movement-move_reas    = <fs_t014>-reasn.
        ls_bapi-is_data_goods_movement-pstng_date   = <fs_t014>-disdt.
        ls_bapi-is_data_goods_movement-doc_date     = sy-datum.
        ls_bapi-is_data_goods_movement-orderid      = <fs_t014>-aufnr.
        <fs_t014>-werks = ls_bapi-is_data_goods_movement-plant        = p_werks.
        ls_bapi-is_data_goods_movement-stge_loc     = <fs_t014>-lgort.
        ls_bapi-is_data_goods_movement-new_batch    =
        ls_bapi-is_data_goods_movement-new_val_type = <fs_t014>-charg.

        CALL FUNCTION 'BAPI_IE4N_DISMANTLE'
          EXPORTING
            i_equipment            = ls_bapi-equipment
            i_material             = ls_bapi-material
            i_material_long        = ls_bapi-material_long
            i_serialnumber         = ls_bapi-serialnumber
            i_dism_date            = ls_bapi-i_dism_date
            i_dism_time            = ls_bapi-i_dism_time
            is_data_goods_movement = ls_bapi-is_data_goods_movement
          TABLES
            return                 = ls_bapi-return.

        READ TABLE ls_bapi-return INTO DATA(ls_return)
          WITH KEY type = 'S' id = 'M7' number = '060'.
        IF sy-subrc = 0.

          COMMIT WORK AND WAIT.

          PERFORM f_set_numc USING ls_return-message_v1 CHANGING <fs_t014>-code1 <fs_t014>-code2.

          ls_err-msgid  = ls_return-id.
          ls_err-msgty  = ls_return-type.
          ls_err-msgno  = ls_return-number.
          ls_err-msgv1  = |Tr 1 - { ls_return-message_v1 }|.
          ls_err-msgv2  = ls_return-message_v2.
          ls_err-msgv3  = ls_return-message_v3.
          ls_err-msgv4  = ls_return-message_v4.
          ls_err-lineno = lv_tabix.
          APPEND ls_err TO gt_err_dis.

          <fs_t014>-uid32 = lv_uuid.
          <fs_t014>-mblnr = |{ <fs_t014>-code1 }{ <fs_t014>-code2 }|.
          <fs_t014>-mjahr = sy-datum(4).
          <fs_t014>-ernam = sy-uname.
          <fs_t014>-erdat = sy-datum.
          <fs_t014>-erzet = sy-uzeit.

          PERFORM f_bapi_meas USING lt_nbapi <fs_t014>-equnl <fs_t014>-hequi <fs_t014>-kmcnt <fs_t014>-trcnt
                                    <fs_t014>-disdt <fs_t014>-distm lv_tabix.

        ELSE.

          LOOP AT ls_bapi-return INTO ls_return.
            ls_err-msgid  = ls_return-id.
            ls_err-msgty  = ls_return-type.
            ls_err-msgno  = ls_return-number.
            ls_err-msgv1  = |Tr 1 - { ls_return-message_v1 }|.
            ls_err-msgv2  = ls_return-message_v2.
            ls_err-msgv3  = ls_return-message_v3.
            ls_err-msgv4  = ls_return-message_v4.
            ls_err-lineno = lv_tabix.
            APPEND ls_err TO gt_err_dis.
          ENDLOOP.

          ROLLBACK WORK.
        ENDIF.

        CLEAR : ls_bapi, ls_err.
      ENDLOOP.

      DELETE lt_t014 WHERE uid32 IS INITIAL.

      IF lt_t014[] IS NOT INITIAL.
        INSERT zpm_t014 FROM TABLE lt_t014[].
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.

          PERFORM f_refresh.

          MESSAGE s000(0k) WITH 'Data saved successfully'.
        ELSE.
          ROLLBACK WORK.
          MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'Data not saved successfully'.
        ENDIF.
      ELSE.
        MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'No data is saved'.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_rotation
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_rotation .

  TYPES :
    BEGIN OF lty_data,
      equip  TYPE equnr,
      rdsfr  TYPE equnr,
      rdsps  TYPE heqnr,
      rdisdt TYPE zdisdt,
      rdistm TYPE zdistm,
      rinsto TYPE hequi,
      rinsps TYPE heqnr,
      rinsdt TYPE zinsdt,
      rinstm TYPE zinstm,
    END OF lty_data.

  DATA  ls_ctuparam TYPE ctu_params.

  DATA : lt_disalv  TYPE gtt_disalv,
         lt_data    TYPE TABLE OF lty_data,
         ls_data    TYPE lty_data,
         ls_err     TYPE gty_err,
         lv_message TYPE char255.

  DATA : lt_node_keys TYPE lvc_t_nkey.

  CLEAR : lt_disalv[], lt_data[], ls_data, lt_node_keys[],
          gt_err_dis[], ls_err, lv_message.

  CALL METHOD gc_alv_tree->get_selected_nodes
    CHANGING
      ct_selected_nodes = lt_node_keys.

  IF lt_node_keys[] IS INITIAL.
    MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'Please select a line'.
  ELSE.

    lt_disalv[] = gtv_disalv[].
    SORT lt_disalv BY hequi ASCENDING.

    LOOP AT lt_node_keys INTO DATA(ls_node_keys).

      READ TABLE gt_node_key INTO DATA(ls_nkey) WITH KEY nkey = ls_node_keys.

      READ TABLE gtv_disalv INTO DATA(ls_disalv) INDEX ls_nkey-indx.

      ls_data-equip = p_equip = ls_disalv-equnl.
      ls_data-rdsfr = p_rdsfr = ls_disalv-hequi.
      ls_data-rdsps = p_rdsps = ls_disalv-heqnr.
      p_rinsto = ls_disalv-hequi.

      CLEAR : p_rinsdt, p_rinstm, p_rdisdt, p_rdistm, p_lgort, p_rinsps.
      CALL SELECTION-SCREEN 1022 STARTING AT 40 3.
      IF sy-subrc NE 0.
        CONTINUE.
      ELSE.
        ls_data-rdisdt = p_rinsdt.
        ls_data-rdistm = p_rinstm.
        ls_data-rinsto = p_rinsto.
        ls_data-rinsps = p_rinsps.
        ls_data-rinsdt = p_rdisdt.
        ls_data-rinstm = p_rdistm.
        APPEND ls_data TO lt_data.
      ENDIF.

      CLEAR ls_node_keys.
    ENDLOOP.

    IF lt_data[] IS NOT INITIAL.

      CLEAR : bdc_data[], bdc_messtab[], ls_ctuparam.

      ls_ctuparam-dismode = 'N'.

      LOOP AT lt_data INTO ls_data.

        DATA(lv_tabix) = sy-tabix.

        PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0100'.
        PERFORM bdc_field       USING 'BDC_CURSOR'        'RM63E-EQUNR'.
        PERFORM bdc_field       USING 'BDC_OKCODE'        '/00'.
        PERFORM bdc_field       USING 'RM63E-EQUNR'       ls_data-equip.

        PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
        PERFORM bdc_field       USING 'BDC_OKCODE'        '=T\04'.
        PERFORM bdc_field       USING 'BDC_CURSOR'        'ITOB-SHTXT'.

        PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
        PERFORM bdc_field       USING 'BDC_OKCODE'        '=CIPL'.
        PERFORM bdc_field       USING 'BDC_CURSOR'        'ITOB-SHTXT'.

        PERFORM bdc_dynpro      USING 'SAPLIEL2'          '0100'.
        PERFORM bdc_field       USING 'BDC_CURSOR'        'IEQINSTALL-UZEIT'.
        PERFORM bdc_field       USING 'BDC_OKCODE'        '=IPDL'.
        DATA(lv_disdt) = |{ ls_data-rdisdt+6(2) }{ ls_data-rdisdt+4(2) }{ ls_data-rdisdt(4) }|.
        PERFORM bdc_field       USING 'IEQINSTALL-DATUM'  lv_disdt.
        PERFORM bdc_field       USING 'IEQINSTALL-UZEIT'  ls_data-rdistm.

        PERFORM bdc_dynpro      USING 'SAPLIEL2'          '0100'.
        PERFORM bdc_field       USING 'BDC_CURSOR'        'IEQINSTALL-UZEIT'.
        PERFORM bdc_field       USING 'BDC_OKCODE'        '=EXIT'.
        PERFORM bdc_field       USING 'IEQINSTALL-HEQNR'  ls_data-rinsto.
        PERFORM bdc_field       USING 'IEQINSTALL-HSTPS'  ls_data-rinsps.
        DATA(lv_insdt) = |{ ls_data-rinsdt+6(2) }{ ls_data-rinsdt+4(2) }{ ls_data-rinsdt(4) }|.
        PERFORM bdc_field       USING 'IEQINSTALL-DATUM'  lv_insdt.
        PERFORM bdc_field       USING 'IEQINSTALL-UZEIT'  ls_data-rinstm.

        PERFORM bdc_dynpro      USING 'SAPMIEQ0'          '0101'.
        PERFORM bdc_field       USING 'BDC_OKCODE'        '=BU'.
        PERFORM bdc_field       USING 'ITOB-POSNR'        ls_data-rinsps.
        PERFORM bdc_field       USING 'BDC_CURSOR'        'ITOB-SHTXT'.

        CALL TRANSACTION 'IE02' USING bdc_data OPTIONS FROM ls_ctuparam
                                               MESSAGES INTO bdc_messtab.
        PERFORM f_get_bdcmessage USING 'IE02' CHANGING lv_message.
        LOOP AT bdc_messtab.
          ls_err-msgid  = bdc_messtab-msgid.
          ls_err-msgty  = bdc_messtab-msgtyp.
          ls_err-msgno  = bdc_messtab-msgnr.
          ls_err-msgv1  = bdc_messtab-msgv1.
          ls_err-msgv2  = bdc_messtab-msgv2.
          ls_err-msgv3  = bdc_messtab-msgv3.
          ls_err-msgv4  = bdc_messtab-msgv4.
          IF sy-tabix = 1.
            ls_err-lineno = sy-tabix.
          ELSE.
            ls_err-lineno = lv_tabix.
          ENDIF.
          APPEND ls_err TO gt_err_dis.
          CLEAR ls_err.
        ENDLOOP.

        CLEAR : ls_data, bdc_data[], bdc_messtab[].
      ENDLOOP.

      WAIT UP TO 1 SECONDS.
      PERFORM f_refresh.

      MESSAGE i000(0k) WITH 'Rotation is complete, check message log'.

    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_0102 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0102 OUTPUT.

  IF gc_alv_tree IS INITIAL.
    PERFORM f_init_tree.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error in the automation queue'(801)
        txt1  = 'Internal error:'(802)
        txt2  = 'A method in the automation queue'(803)
        txt3  = 'raised an error.'(804).
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form f_init_tree
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_tree .

  CREATE OBJECT gc_custom_container
    EXPORTING
      container_name              = 'CC_ALV'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'(100).
  ENDIF.

  CREATE OBJECT gc_alv_tree
    EXPORTING
      parent                      = gc_custom_container
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
      item_selection              = 'X'
      no_html_header              = 'X'
      no_toolbar                  = 'X'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.
  ENDIF.

  DATA  : ls_hierarchy_header TYPE treev_hhdr,
          lt_fieldcat         TYPE lvc_t_fcat.

  CLEAR : ls_hierarchy_header, lt_fieldcat[].

  PERFORM f_build_hierarchy_header CHANGING ls_hierarchy_header.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZST_DISMANTLE'
    CHANGING
      ct_fieldcat      = lt_fieldcat.

  LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
    CASE <fs_fieldcat>-fieldname.
      WHEN 'HEQUI' OR 'EQTYP' OR 'SOWRK' OR 'POINT'.
        <fs_fieldcat>-no_out = 'X'.
    ENDCASE.
  ENDLOOP.

  CALL METHOD gc_alv_tree->set_table_for_first_display
    EXPORTING
      is_hierarchy_header = ls_hierarchy_header
    CHANGING
      it_outtab           = gt_outtab
      it_fieldcatalog     = lt_fieldcat.

  PERFORM f_create_hierarchy.

  CALL METHOD gc_alv_tree->column_optimize( ).

  CALL METHOD gc_alv_tree->frontend_update.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_build_hierarchy_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
FORM f_build_hierarchy_header CHANGING pc_hierarchy_header TYPE treev_hhdr.

  pc_hierarchy_header-heading = 'Equipment'.
  pc_hierarchy_header-tooltip = 'Equipment'.
  pc_hierarchy_header-width = 35.
  pc_hierarchy_header-width_pix = ''.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_create_hierarchy
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_create_hierarchy .

  DATA  : lv_relat_key  TYPE lvc_nkey,
          lv_node_key   TYPE lvc_nkey,
          lv_last_key   TYPE lvc_nkey,
          lv_layn_image TYPE tv_image,
          lt_node_key   TYPE lvc_t_nkey,
          ls_nkey       TYPE gty_nkey.

  CLEAR : lv_relat_key, lv_node_key, lv_last_key, lv_layn_image,
          lt_node_key[], gt_node_key[], ls_nkey.

  LOOP AT gtv_disalv INTO DATA(ls_disalv).

    ls_nkey-indx = sy-tabix.

    CASE ls_disalv-eqtyp.
      WHEN 'P'.
        CLEAR : lv_node_key, lv_relat_key, gt_node_key[].
        lv_layn_image = icon_transport.
        PERFORM f_hierarchy_alv USING lv_layn_image
                                      ''
                                      ls_disalv
                                      ''
                                CHANGING lv_node_key.

        ls_nkey-nkey = lv_node_key.

        APPEND lv_node_key TO lt_node_key.

      WHEN 'A'.
        lv_layn_image = icon_connect.
        PERFORM f_hierarchy_alv USING lv_layn_image
                                      'X'
                                      ls_disalv
                                      lv_node_key
                                CHANGING lv_relat_key.

        READ TABLE gtv_disalv TRANSPORTING NO FIELDS
        WITH KEY hequi = ls_disalv-equnl.
        IF sy-subrc EQ 0.
          APPEND lv_relat_key TO lt_node_key.
        ENDIF.

        ls_nkey-nkey = lv_relat_key.

      WHEN 'T' OR 'C'.
        CASE ls_disalv-eqtyp.
          WHEN 'T'.
            lv_layn_image = icon_bw_open_hub_destination.
          WHEN 'C'.
            lv_layn_image = icon_system_settings.
        ENDCASE.

        IF lv_relat_key IS INITIAL.
          lv_relat_key = lv_node_key.
        ENDIF.

        PERFORM f_complete_line USING lv_layn_image
                                      ls_disalv
                                      lv_relat_key
                                CHANGING lv_last_key.
        ls_nkey-nkey = lv_last_key.
    ENDCASE.

    INSERT ls_nkey INTO TABLE gt_node_key.

    CLEAR : ls_disalv, ls_nkey.
  ENDLOOP.

  CALL METHOD gc_alv_tree->expand_nodes
    EXPORTING
      it_node_key = lt_node_key.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_hierarchy_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_RIHEQUI_EQUNR
*&      --> P_
*&---------------------------------------------------------------------*
FORM f_hierarchy_alv  USING     pu_lv_layn_image  TYPE tv_image
                                pu_val
                                pu_ls_disalv      TYPE zst_dismantle
                                pu_relat_key      TYPE lvc_nkey
                      CHANGING  pc_node_key       TYPE lvc_nkey.

  DATA  : lv_node_val    TYPE lvc_value,
          ls_node_layout TYPE lvc_s_layn.

  CLEAR : lv_node_val, ls_node_layout.

  lv_node_val = pu_ls_disalv-equnl.
  IF pu_val IS INITIAL.
    CLEAR pu_ls_disalv.
  ELSE.
    CLEAR : pu_ls_disalv-equnl, pu_ls_disalv-sernr, pu_ls_disalv-matnr.
  ENDIF.

  ls_node_layout-disabled = 'X'.
  ls_node_layout-n_image = ls_node_layout-exp_image = pu_lv_layn_image.

  CALL METHOD gc_alv_tree->add_node
    EXPORTING
      i_relat_node_key = pu_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = lv_node_val
      is_outtab_line   = pu_ls_disalv
      is_node_layout   = ls_node_layout
    IMPORTING
      e_new_node_key   = pc_node_key.

  pu_ls_disalv-equnl = lv_node_val.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_complete_line
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_RIHEQUI
*&      --> LV_NODE_KEY
*&      <-- LV_LAST_KEY
*&---------------------------------------------------------------------*
FORM f_complete_line  USING    pu_lv_layn_image TYPE tv_image
                               pu_ls_disalv   TYPE zst_dismantle
                               pu_lv_node_key TYPE lvc_nkey
                      CHANGING pc_lv_last_key TYPE lvc_nkey.

  DATA  : ls_outtab      TYPE zst_dismantle,
          lv_node_val    TYPE lvc_value,
          ls_node_layout TYPE lvc_s_layn.

  CLEAR : lv_node_val, ls_node_layout.

  lv_node_val = pu_ls_disalv-equnl.
  ls_node_layout-n_image = ls_node_layout-exp_image = pu_lv_layn_image.

  CALL METHOD gc_alv_tree->add_node
    EXPORTING
      i_relat_node_key = pu_lv_node_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      is_outtab_line   = pu_ls_disalv
      i_node_text      = lv_node_val
      is_node_layout   = ls_node_layout
    IMPORTING
      e_new_node_key   = pc_lv_last_key.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_searchhelp_lgort
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_searchhelp_lgort .

  DATA : BEGIN OF ls_f4values,
           value(132) TYPE c,
         END OF ls_f4values.

  DATA  : ls_f4fiel TYPE dfies,
          lt_f4tab  LIKE TABLE OF ls_f4values WITH HEADER LINE,
          lt_f4fiel TYPE TABLE OF dfies,
          lt_f4ret  TYPE TABLE OF ddshretval WITH HEADER LINE,
          lt_dyntab TYPE TABLE OF dselc WITH HEADER LINE.

  CLEAR : ls_f4fiel, lt_f4tab[],
          lt_f4fiel[], lt_f4ret[], lt_dyntab[].

  SELECT FROM t001l
    FIELDS werks, lgort, lgobe
    WHERE werks EQ @p_werks
    INTO TABLE @DATA(lt_t001l).

  IF lt_t001l[] IS NOT INITIAL.

    ls_f4fiel-tabname = 'T001L'.  ls_f4fiel-fieldname = 'WERKS'.
    APPEND ls_f4fiel TO lt_f4fiel.
    ls_f4fiel-tabname = 'T001L'.  ls_f4fiel-fieldname = 'LGORT'.
    APPEND ls_f4fiel TO lt_f4fiel.
    ls_f4fiel-tabname = 'T001L'.  ls_f4fiel-fieldname = 'LGOBE'.
    APPEND ls_f4fiel TO lt_f4fiel.

    LOOP AT lt_t001l INTO DATA(ls_t001l).
      ls_f4values-value = ls_t001l-werks. APPEND ls_f4values TO lt_f4tab.
      ls_f4values-value = ls_t001l-lgort. APPEND ls_f4values TO lt_f4tab.
      ls_f4values-value = ls_t001l-lgobe. APPEND ls_f4values TO lt_f4tab.
      CLEAR : ls_t001l, ls_f4values.
    ENDLOOP.

    lt_dyntab-fldname = 'P_LGORT'.
    APPEND lt_dyntab.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'LGORT'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'P_LGORT'
      TABLES
        value_tab       = lt_f4tab
        field_tab       = lt_f4fiel
        return_tab      = lt_f4ret
        dynpfld_mapping = lt_dyntab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

  ELSE.
    MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'No Data found'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_searchhelp_reasn
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_searchhelp_reasn .

  DATA : BEGIN OF ls_f4values,
           value(132) TYPE c,
         END OF ls_f4values.

  DATA  : ls_f4fiel TYPE dfies,
          lt_f4tab  LIKE TABLE OF ls_f4values WITH HEADER LINE,
          lt_f4fiel TYPE TABLE OF dfies,
          lt_f4ret  TYPE TABLE OF ddshretval WITH HEADER LINE,
          lt_dyntab TYPE TABLE OF dselc WITH HEADER LINE.

  CLEAR : ls_f4fiel, lt_f4tab[],
          lt_f4fiel[], lt_f4ret[], lt_dyntab[].

  SELECT FROM t157d AS a
    INNER JOIN t157e AS b ON b~bwart = a~bwart
                          AND b~grund = a~grund
    FIELDS a~bwart, a~grund, grtxt
    WHERE a~bwart EQ '262'
      AND spras EQ 'E'
    INTO TABLE @DATA(lt_t157d).

  IF lt_t157d[] IS NOT INITIAL.

    ls_f4fiel-tabname = 'T157D'.  ls_f4fiel-fieldname = 'BWART'.
    APPEND ls_f4fiel TO lt_f4fiel.
    ls_f4fiel-tabname = 'T157D'.  ls_f4fiel-fieldname = 'GRUND'.
    APPEND ls_f4fiel TO lt_f4fiel.
    ls_f4fiel-tabname = 'T157E'.  ls_f4fiel-fieldname = 'GRTXT'.
    APPEND ls_f4fiel TO lt_f4fiel.

    LOOP AT lt_t157d INTO DATA(ls_t157d).
      ls_f4values-value = ls_t157d-bwart. APPEND ls_f4values TO lt_f4tab.
      ls_f4values-value = ls_t157d-grund. APPEND ls_f4values TO lt_f4tab.
      ls_f4values-value = ls_t157d-grtxt. APPEND ls_f4values TO lt_f4tab.
      CLEAR : ls_t157d, ls_f4values.
    ENDLOOP.

    lt_dyntab-fldname = 'P_REASN'.
    APPEND lt_dyntab.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'GRUND'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'P_REASN'
      TABLES
        value_tab       = lt_f4tab
        field_tab       = lt_f4fiel
        return_tab      = lt_f4ret
        dynpfld_mapping = lt_dyntab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

  ELSE.
    MESSAGE s000(0k) DISPLAY LIKE 'W' WITH 'No Data found'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_updtree
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_updtree .

  CALL METHOD gc_alv_tree->delete_all_nodes.
  PERFORM f_create_hierarchy.
  CALL METHOD gc_alv_tree->update_calculations.
  CALL METHOD gc_alv_tree->frontend_update.
  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_numc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_RETURN_MESSAGE_V1
*&      <-- LS_T013_CODE1
*&      <-- LS_T013_CODE2
*&---------------------------------------------------------------------*
FORM f_set_numc  USING    pu_ls_return_message_v1 TYPE symsgv
                 CHANGING pc_ls_t013_code1 TYPE zcode1
                          pc_ls_t013_code2 TYPE zcode2.

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
  pc_ls_t013_code1 = pu_ls_return_message_v1(10).
  pc_ls_t013_code2 = pu_ls_return_message_v1+10(4).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_psortrange
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_psortrange .

  DEFINE d_psort.
    gtr_psort = &1.
    gtr_psort-high = &2.
    APPEND gtr_psort.
  END-OF-DEFINITION.

  CLEAR gtr_psort[].

  d_psort 'IEQT' 'HOURMETER'.
  d_psort 'IEQT' 'KILOMETER'.
  d_psort 'IEQT' 'TREAD'.
  d_psort 'IEQT' 'PRESSURE'.

  d_psort 'IEQR' 'HOURMETER'.
  d_psort 'IEQR' 'KILOMETER'.
  d_psort 'IEQR' 'TREAD'.
  d_psort 'IEQR' 'PRESSURE'.

  d_psort 'IEQC' 'HOURMETER'.
  d_psort 'IEQC' 'KILOMETER'.

  SORT gtr_psort BY low.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_install_tread
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_INDCT
*&      --> LS_CTUPARAM
*&      --> LS_EQUI_EQUNR
*&      --> LS_IMPTT_PSORT
*&      --> LS_ERR
*&      --> LV_TABIX
*&      <-- LS_T013_POINT
*&---------------------------------------------------------------------*
FORM f_install_tread  USING pu_ctuparam       TYPE ctu_params
                            pu_ls_equi_equnr  TYPE equnr
                            pu_gtr_psort_high TYPE imrc_psort
                            pu_ls_err         TYPE gty_err
                            pu_lv_tabix       TYPE sy-tabix
                            pu_insdt          TYPE zdisdt
                            pu_instm          TYPE zdistm
                            pu_trcnt          TYPE zdetread
                            pu_hequi          TYPE hequi
                            pu_lt_nbapi       TYPE gtt_nbapi
                      CHANGING pc_point       TYPE imrc_point
                               pc_trcnt       TYPE zdetread.

  DATA  : lv_tcode(15)             TYPE c,
          lv_message(255)          TYPE c,
          lv_recval                TYPE imrc_recdc,
          lv_measurement_document  LIKE imrg-mdocm,
          ls_complete_document     TYPE imrg,
          lv_notification          LIKE qmel-qmnum,
          lv_custom_duprec_occured LIKE iref-iind,
          ls_linear_data_exp       LIKE eaml_s_lfe_data_api.
  CLEAR : bdc_data[], bdc_messtab[], lv_tcode, lv_message,
          pc_point, lv_recval, lv_measurement_document, ls_complete_document,
          lv_notification, lv_custom_duprec_occured, ls_linear_data_exp.

  " Transaksi 2
  lv_tcode = 'IK01'.
  PERFORM bdc_dynpro USING 'SAPLIMR0'                      '1110'.
  PERFORM bdc_field  USING 'BDC_OKCODE'                    '/00'.
  PERFORM bdc_field  USING 'RIMR0-MPOTY'                   'IEQ'.
  PERFORM bdc_field  USING 'IMPT-MPTYP'                    'K'.
  PERFORM bdc_field  USING 'BDC_CURSOR'                    'EQUI-EQUNR'.
  PERFORM bdc_field  USING 'EQUI-EQUNR'                    pu_ls_equi_equnr.

  PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
  PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-ATNAM'.
  PERFORM bdc_field  USING 'BDC_OKCODE'                    '/00'.
  PERFORM bdc_field  USING 'IMPT-PSORT'                    pu_gtr_psort_high.
  DATA(lv_pttxt) = |{ pu_ls_equi_equnr }-{ pu_gtr_psort_high }|.
  PERFORM bdc_field  USING 'IMPT-PTTXT'                    lv_pttxt.
  PERFORM bdc_field  USING 'IMPT-ATNAM'                    pu_gtr_psort_high.

  PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
  PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-PSORT'.
  PERFORM bdc_field  USING 'BDC_OKCODE'                    '=BU'.

  CALL TRANSACTION lv_tcode USING bdc_data OPTIONS FROM pu_ctuparam
                            MESSAGES INTO bdc_messtab.
  IF sy-subrc EQ 0.

    COMMIT WORK AND WAIT.

    PERFORM f_get_bdcmessage USING lv_tcode CHANGING lv_message.
    LOOP AT bdc_messtab.
      pu_ls_err-msgid  = bdc_messtab-msgid.
      pu_ls_err-msgty  = bdc_messtab-msgtyp.
      pu_ls_err-msgno  = bdc_messtab-msgnr.
      pu_ls_err-msgv1  = |Tr 2 - { bdc_messtab-msgv1 }|.
      pu_ls_err-msgv2  = bdc_messtab-msgv2.
      pu_ls_err-msgv3  = bdc_messtab-msgv3.
      pu_ls_err-msgv4  = bdc_messtab-msgv4.
      IF sy-tabix = 1.
        pu_ls_err-lineno = sy-tabix.
      ELSE.
        pu_ls_err-lineno = pu_lv_tabix.
      ENDIF.
      APPEND pu_ls_err TO gt_err_ins.
      CLEAR pu_ls_err.
    ENDLOOP.

    pc_point = lv_message.

    " Transaksi 3
*    READ TABLE pu_lt_nbapi INTO DATA(ls_nbapi)
*    WITH KEY equnr = pu_hequi psort = 'TREAD' BINARY SEARCH. "
*    IF sy-subrc = 0.
    lv_recval = pu_trcnt.
    CONDENSE lv_recval.
    ADD 5 TO pu_instm.
    CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
      EXPORTING
        measurement_point     = pc_point
        reading_date          = pu_insdt
        reading_time          = pu_instm
        short_text            = 'UPDATE INSTALL TIRE TREAD'
        reader                = sy-uname
        origin_indicator      = 'A'
        recorded_value        = lv_recval
        prepare_update        = 'X'
        commit_work           = 'X'
        wait_after_commit     = 'X'
      IMPORTING
        measurement_document  = lv_measurement_document
        complete_document     = ls_complete_document
        notification          = lv_notification
        custom_duprec_occured = lv_custom_duprec_occured
        linear_data_exp       = ls_linear_data_exp.
    IF sy-subrc EQ 0.
      pc_trcnt = p_trcnt.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
*    ENDIF.

  ELSE.
    PERFORM f_get_bdcmessage USING lv_tcode CHANGING lv_message.
    LOOP AT bdc_messtab.
      pu_ls_err-msgid  = bdc_messtab-msgid.
      pu_ls_err-msgty  = bdc_messtab-msgtyp.
      pu_ls_err-msgno  = bdc_messtab-msgnr.
      pu_ls_err-msgv1  = |Tr 2 - { bdc_messtab-msgv1 }|.
      pu_ls_err-msgv2  = bdc_messtab-msgv2.
      pu_ls_err-msgv3  = bdc_messtab-msgv3.
      pu_ls_err-msgv4  = bdc_messtab-msgv4.
      IF sy-tabix = 1.
        pu_ls_err-lineno = sy-tabix.
      ELSE.
        pu_ls_err-lineno = pu_lv_tabix.
      ENDIF.
      APPEND pu_ls_err TO gt_err_ins.
      CLEAR pu_ls_err.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_install_press
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_INDCT
*&      --> LS_CTUPARAM
*&      --> LS_EQUI_EQUNR
*&      --> LS_IMPTT_PSORT
*&      --> LS_ERR
*&      --> LV_TABIX
*&      <-- LS_T013_POINT
*&---------------------------------------------------------------------*
FORM f_install_press  USING pu_ctuparam       TYPE ctu_params
                            pu_ls_equi_equnr  TYPE equnr
                            pu_gtr_psort_high TYPE imrc_psort
                            pu_ls_err         TYPE gty_err
                            pu_lv_tabix       TYPE sy-tabix
                      CHANGING pc_point       TYPE imrc_point.

  DATA  : lv_tcode(15)    TYPE c,
          lv_message(255) TYPE c.
  CLEAR : bdc_data[], bdc_messtab[], lv_tcode, lv_message, pc_point.

  " Transaksi 2
  lv_tcode = 'IK01'.
  PERFORM bdc_dynpro USING 'SAPLIMR0'                      '1110'.
  PERFORM bdc_field  USING 'BDC_OKCODE'                    '/00'.
  PERFORM bdc_field  USING 'RIMR0-MPOTY'                   'IEQ'.
  PERFORM bdc_field  USING 'IMPT-MPTYP'                    'K'.
  PERFORM bdc_field  USING 'BDC_CURSOR'                    'EQUI-EQUNR'.
  PERFORM bdc_field  USING 'EQUI-EQUNR'                    pu_ls_equi_equnr.

  PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
  PERFORM bdc_field  USING 'BDC_CURSOR'                    'RIMR0-MPOBT'.
  PERFORM bdc_field  USING 'BDC_OKCODE'                    '/00'.
  PERFORM bdc_field  USING 'IMPT-PSORT'                    pu_gtr_psort_high.
  DATA(lv_pttxt) = |{ pu_ls_equi_equnr }-{ pu_gtr_psort_high }|.
  PERFORM bdc_field  USING 'IMPT-PTTXT'                    lv_pttxt.
  PERFORM bdc_field  USING 'IMPT-ATNAM'                    pu_gtr_psort_high.

  PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
  PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-PSORT'.
  PERFORM bdc_field  USING 'BDC_OKCODE'                    '=BU'.

  CALL TRANSACTION lv_tcode USING bdc_data OPTIONS FROM pu_ctuparam
                            MESSAGES INTO bdc_messtab.

  PERFORM f_get_bdcmessage USING lv_tcode CHANGING lv_message.
  LOOP AT bdc_messtab.
    pu_ls_err-msgid  = bdc_messtab-msgid.
    pu_ls_err-msgty  = bdc_messtab-msgtyp.
    pu_ls_err-msgno  = bdc_messtab-msgnr.
    pu_ls_err-msgv1  = |Tr 2 - { bdc_messtab-msgv1 }|.
    pu_ls_err-msgv2  = bdc_messtab-msgv2.
    pu_ls_err-msgv3  = bdc_messtab-msgv3.
    pu_ls_err-msgv4  = bdc_messtab-msgv4.
    IF sy-tabix = 1.
      pu_ls_err-lineno = sy-tabix.
    ELSE.
      pu_ls_err-lineno = pu_lv_tabix.
    ENDIF.
    APPEND pu_ls_err TO gt_err_ins.
    CLEAR pu_ls_err.
  ENDLOOP.

  pc_point = lv_message.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_install_hour
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_INDCT
*&      --> LS_CTUPARAM
*&      --> LS_EQUI_EQUNR
*&      --> LS_IMPTT_PSORT
*&      --> LS_ERR
*&      --> LV_TABIX
*&      <-- LS_T013_POINT
*&---------------------------------------------------------------------*
FORM f_install_hour  USING pu_ctuparam       TYPE ctu_params
                           pu_ls_equi_equnr  TYPE equnr
                           pu_gtr_psort_high TYPE imrc_psort
                           pu_ls_err         TYPE gty_err
                           pu_lv_tabix       TYPE sy-tabix
                           pu_lvorm          TYPE imrc_del2
                           pu_point          TYPE imrc_point
                     CHANGING pc_point       TYPE imrc_point.

  DATA  : lv_tcode(15)    TYPE c,
          lv_message(255) TYPE c.
  CLEAR : bdc_data[], bdc_messtab[], lv_tcode, lv_message, pc_point.

  IF pu_point IS INITIAL OR ( pu_point IS NOT INITIAL AND pu_lvorm EQ 'X' ).
    " Transaksi 2
    lv_tcode = 'IK01'.
    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '1110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-INDCT'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=GDPT'.
    PERFORM bdc_field  USING 'RIMR0-MPOTY'                   'IEQ'.
    PERFORM bdc_field  USING 'IMPT-MPTYP'                    'K'.
    PERFORM bdc_field  USING 'IMPT-INDCT'                    'X'.
    PERFORM bdc_field  USING 'EQUI-EQUNR'                    pu_ls_equi_equnr.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-ATNAM'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '/00'.
    PERFORM bdc_field  USING 'IMPT-PSORT'                    pu_gtr_psort_high.
    DATA(lv_pttxt) = |{ pu_ls_equi_equnr }-{ pu_gtr_psort_high }|.
    PERFORM bdc_field  USING 'IMPT-PTTXT'                    lv_pttxt.
    PERFORM bdc_field  USING 'IMPT-ATNAM'                    pu_gtr_psort_high(4).

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-PSORT'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=BU'.

    CALL TRANSACTION lv_tcode USING bdc_data OPTIONS FROM pu_ctuparam
                              MESSAGES INTO bdc_messtab.

    IF sy-subrc EQ 0.

      PERFORM f_get_bdcmessage USING lv_tcode CHANGING lv_message.
      LOOP AT bdc_messtab.
        pu_ls_err-msgid  = bdc_messtab-msgid.
        pu_ls_err-msgty  = bdc_messtab-msgtyp.
        pu_ls_err-msgno  = bdc_messtab-msgnr.
        pu_ls_err-msgv1  = |Tr 2 - { bdc_messtab-msgv1 }|.
        pu_ls_err-msgv2  = bdc_messtab-msgv2.
        pu_ls_err-msgv3  = bdc_messtab-msgv3.
        pu_ls_err-msgv4  = bdc_messtab-msgv4.
        IF sy-tabix = 1.
          pu_ls_err-lineno = sy-tabix.
        ELSE.
          pu_ls_err-lineno = pu_lv_tabix.
        ENDIF.
        APPEND pu_ls_err TO gt_err_ins.
        CLEAR pu_ls_err.
      ENDLOOP.

      pc_point = lv_message.

    ELSE.

      PERFORM f_get_bdcmessage USING lv_tcode CHANGING lv_message.
      LOOP AT bdc_messtab.
        pu_ls_err-msgid  = bdc_messtab-msgid.
        pu_ls_err-msgty  = bdc_messtab-msgtyp.
        pu_ls_err-msgno  = bdc_messtab-msgnr.
        pu_ls_err-msgv1  = |Tr 2 - { bdc_messtab-msgv1 }|.
        pu_ls_err-msgv2  = bdc_messtab-msgv2.
        pu_ls_err-msgv3  = bdc_messtab-msgv3.
        pu_ls_err-msgv4  = bdc_messtab-msgv4.
        IF sy-tabix = 1.
          pu_ls_err-lineno = sy-tabix.
        ELSE.
          pu_ls_err-lineno = pu_lv_tabix.
        ENDIF.
        APPEND pu_ls_err TO gt_err_ins.
        CLEAR pu_ls_err.
      ENDLOOP.

    ENDIF.
  ELSE.
    pc_point = pu_point.
  ENDIF.

  IF pc_point IS NOT INITIAL.
    " Transaksi 3
    CLEAR : bdc_data[], bdc_messtab[], lv_message, lv_tcode.
    lv_tcode = 'IK02'.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '1110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-POINT'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=GDPT'.
    PERFORM bdc_field  USING 'IMPT-POINT'                    pc_point.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-PSORT'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=ADPT'.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '6110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'RIMR0-MRMAC'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=TRDF'.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '6320'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPH-TRANS'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=NEXT'.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '6110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'RIMR0-MRMAC'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=NEXT'.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-PSORT'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=BU'.

    CALL TRANSACTION lv_tcode USING bdc_data OPTIONS FROM pu_ctuparam
                              MESSAGES INTO bdc_messtab.

    PERFORM f_get_bdcmessage USING lv_tcode CHANGING lv_message.
    LOOP AT bdc_messtab.
      pu_ls_err-msgid  = bdc_messtab-msgid.
      pu_ls_err-msgty  = bdc_messtab-msgtyp.
      pu_ls_err-msgno  = bdc_messtab-msgnr.
      pu_ls_err-msgv1  = |Tr 3 - { bdc_messtab-msgv1 }|.
      pu_ls_err-msgv2  = bdc_messtab-msgv2.
      pu_ls_err-msgv3  = bdc_messtab-msgv3.
      pu_ls_err-msgv4  = bdc_messtab-msgv4.
      IF sy-tabix = 1.
        pu_ls_err-lineno = sy-tabix.
      ELSE.
        pu_ls_err-lineno = pu_lv_tabix.
      ENDIF.
      APPEND pu_ls_err TO gt_err_ins.
      CLEAR pu_ls_err.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_install_kilo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_INDCT
*&      --> LS_CTUPARAM
*&      --> LS_EQUI_EQUNR
*&      --> LS_IMPTT_PSORT
*&      --> LS_ERR
*&      --> LV_TABIX
*&      <-- LS_T013_POINT
*&---------------------------------------------------------------------*
FORM f_install_kilo  USING pu_ctuparam       TYPE ctu_params
                           pu_ls_equi_equnr  TYPE equnr
                           pu_gtr_psort_high TYPE imrc_psort
                           pu_ls_err         TYPE gty_err
                           pu_lv_tabix       TYPE sy-tabix
                           pu_insdt          TYPE zdisdt
                           pu_instm          TYPE zdistm
                           pu_kmcnt          TYPE zdekm
                           pu_hequi          TYPE hequi
                           pu_lt_nbapi       TYPE gtt_nbapi
                           pu_lvorm          TYPE imrc_del2
                           pu_point          TYPE imrc_point
                     CHANGING pc_point       TYPE imrc_point
                              pc_kmcnt       TYPE zdekm.

  DATA  : lv_tcode(15)             TYPE c,
          lv_message(255)          TYPE c,
          lv_recval                TYPE imrc_recdc,
          lv_measurement_document  LIKE imrg-mdocm,
          ls_complete_document     TYPE imrg,
          lv_notification          LIKE qmel-qmnum,
          lv_custom_duprec_occured LIKE iref-iind,
          ls_linear_data_exp       LIKE eaml_s_lfe_data_api.
  CLEAR : bdc_data[], bdc_messtab[], lv_tcode, lv_message,
          pc_point, lv_recval, lv_measurement_document, ls_complete_document,
          lv_notification, lv_custom_duprec_occured, ls_linear_data_exp..

  IF pu_point IS INITIAL OR ( pu_point IS NOT INITIAL AND pu_lvorm EQ 'X' ).
    " Transaksi 2
    lv_tcode = 'IK01'.
    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '1110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-INDCT'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '/00'.
    PERFORM bdc_field  USING 'RIMR0-MPOTY'                   'IEQ'.
    PERFORM bdc_field  USING 'IMPT-MPTYP'                    'K'.
    PERFORM bdc_field  USING 'IMPT-INDCT'                    'X'.
    PERFORM bdc_field  USING 'EQUI-EQUNR'                    pu_ls_equi_equnr.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-ATNAM'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '/00'.
    PERFORM bdc_field  USING 'IMPT-PSORT'                    pu_gtr_psort_high.
    DATA(lv_pttxt) = |{ pu_ls_equi_equnr }-{ pu_gtr_psort_high }|.
    PERFORM bdc_field  USING 'IMPT-PTTXT'                    lv_pttxt.
    PERFORM bdc_field  USING 'IMPT-ATNAM'                    pu_gtr_psort_high.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-PSORT'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=BU'.

    CALL TRANSACTION lv_tcode USING bdc_data OPTIONS FROM pu_ctuparam
                              MESSAGES INTO bdc_messtab.
    IF sy-subrc EQ 0.

      PERFORM f_get_bdcmessage USING lv_tcode CHANGING lv_message.
      LOOP AT bdc_messtab.
        pu_ls_err-msgid  = bdc_messtab-msgid.
        pu_ls_err-msgty  = bdc_messtab-msgtyp.
        pu_ls_err-msgno  = bdc_messtab-msgnr.
        pu_ls_err-msgv1  = |Tr 2 - { bdc_messtab-msgv1 }|.
        pu_ls_err-msgv2  = bdc_messtab-msgv2.
        pu_ls_err-msgv3  = bdc_messtab-msgv3.
        pu_ls_err-msgv4  = bdc_messtab-msgv4.
        IF sy-tabix = 1.
          pu_ls_err-lineno = sy-tabix.
        ELSE.
          pu_ls_err-lineno = pu_lv_tabix.
        ENDIF.
        APPEND pu_ls_err TO gt_err_ins.
        CLEAR pu_ls_err.
      ENDLOOP.

      pc_point = lv_message.

    ELSE.

      PERFORM f_get_bdcmessage USING lv_tcode CHANGING lv_message.
      LOOP AT bdc_messtab.
        pu_ls_err-msgid  = bdc_messtab-msgid.
        pu_ls_err-msgty  = bdc_messtab-msgtyp.
        pu_ls_err-msgno  = bdc_messtab-msgnr.
        pu_ls_err-msgv1  = |Tr 2 - { bdc_messtab-msgv1 }|.
        pu_ls_err-msgv2  = bdc_messtab-msgv2.
        pu_ls_err-msgv3  = bdc_messtab-msgv3.
        pu_ls_err-msgv4  = bdc_messtab-msgv4.
        IF sy-tabix = 1.
          pu_ls_err-lineno = sy-tabix.
        ELSE.
          pu_ls_err-lineno = pu_lv_tabix.
        ENDIF.
        APPEND pu_ls_err TO gt_err_ins.
        CLEAR pu_ls_err.
      ENDLOOP.

    ENDIF.
  ELSE.
    pc_point = pu_point.
  ENDIF.

  IF pc_point IS NOT INITIAL.
    " Transaksi 3
    CLEAR : bdc_data[], bdc_messtab[], lv_message, lv_tcode.
    lv_tcode = 'IK02'.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '1110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-POINT'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=GDPT'.
    PERFORM bdc_field  USING 'IMPT-POINT'                    pc_point.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-PSORT'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=ADPT'.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '6110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'RIMR0-MRMAC'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=TRDF'.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '6320'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPH-TRANS'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=NEXT'.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '6110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'RIMR0-MRMAC'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=NEXT'.

    PERFORM bdc_dynpro USING 'SAPLIMR0'                      '5110'.
    PERFORM bdc_field  USING 'BDC_CURSOR'                    'IMPT-PSORT'.
    PERFORM bdc_field  USING 'BDC_OKCODE'                    '=BU'.

    CALL TRANSACTION lv_tcode USING bdc_data OPTIONS FROM pu_ctuparam
                              MESSAGES INTO bdc_messtab.

    IF sy-subrc EQ 0.

      COMMIT WORK AND WAIT.

      PERFORM f_get_bdcmessage USING lv_tcode CHANGING lv_message.
      LOOP AT bdc_messtab.
        pu_ls_err-msgid  = bdc_messtab-msgid.
        pu_ls_err-msgty  = bdc_messtab-msgtyp.
        pu_ls_err-msgno  = bdc_messtab-msgnr.
        pu_ls_err-msgv1  = |Tr 3 - { bdc_messtab-msgv1 }|.
        pu_ls_err-msgv2  = bdc_messtab-msgv2.
        pu_ls_err-msgv3  = bdc_messtab-msgv3.
        pu_ls_err-msgv4  = bdc_messtab-msgv4.
        IF sy-tabix = 1.
          pu_ls_err-lineno = sy-tabix.
        ELSE.
          pu_ls_err-lineno = pu_lv_tabix.
        ENDIF.
        APPEND pu_ls_err TO gt_err_ins.
        CLEAR pu_ls_err.
      ENDLOOP.

      " Transaksi 4
      READ TABLE pu_lt_nbapi INTO DATA(ls_nbapi)
      WITH KEY equnr = pu_hequi psort = 'KILOMETER' BINARY SEARCH.
      IF sy-subrc = 0.
        lv_recval = pu_kmcnt.
        CONDENSE lv_recval.
*        ADD 5 TO pu_instm.
        CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
          EXPORTING
            measurement_point     = ls_nbapi-point
            reading_date          = pu_insdt
            reading_time          = pu_instm
            short_text            = 'UPDATE INSTALL TIRE KM'
            reader                = sy-uname
            origin_indicator      = 'A'
            recorded_value        = lv_recval
            prepare_update        = 'X'
            commit_work           = 'X'
            wait_after_commit     = 'X'
          IMPORTING
            measurement_document  = lv_measurement_document
            complete_document     = ls_complete_document
            notification          = lv_notification
            custom_duprec_occured = lv_custom_duprec_occured
            linear_data_exp       = ls_linear_data_exp.
        IF sy-subrc EQ 0.
          pc_kmcnt = p_kmcnt.

          pu_ls_err-msgid  = '0k'.
          pu_ls_err-msgty  = 'S'.
          pu_ls_err-msgno  = '000'.
          pu_ls_err-msgv1  = 'Tr 4 - Success bapi measurement'.
          IF sy-tabix = 1.
            pu_ls_err-lineno = sy-tabix.
          ELSE.
            pu_ls_err-lineno = pu_lv_tabix.
          ENDIF.
          APPEND pu_ls_err TO gt_err_ins.
          CLEAR pu_ls_err.

        ELSE.

          pu_ls_err-msgid  = '0k'.
          pu_ls_err-msgty  = 'E'.
          pu_ls_err-msgno  = '000'.
          pu_ls_err-msgv1  = 'Tr 4 - Error bapi measurement'.
          IF sy-tabix = 1.
            pu_ls_err-lineno = sy-tabix.
          ELSE.
            pu_ls_err-lineno = pu_lv_tabix.
          ENDIF.
          APPEND pu_ls_err TO gt_err_ins.
          CLEAR pu_ls_err.

          ROLLBACK WORK.
        ENDIF.
      ENDIF.

    ELSE.
      PERFORM f_get_bdcmessage USING lv_tcode CHANGING lv_message.
      LOOP AT bdc_messtab.
        pu_ls_err-msgid  = bdc_messtab-msgid.
        pu_ls_err-msgty  = bdc_messtab-msgtyp.
        pu_ls_err-msgno  = bdc_messtab-msgnr.
        pu_ls_err-msgv1  = |Tr 3 - { bdc_messtab-msgv1 }|.
        pu_ls_err-msgv2  = bdc_messtab-msgv2.
        pu_ls_err-msgv3  = bdc_messtab-msgv3.
        pu_ls_err-msgv4  = bdc_messtab-msgv4.
        IF sy-tabix = 1.
          pu_ls_err-lineno = sy-tabix.
        ELSE.
          pu_ls_err-lineno = pu_lv_tabix.
        ENDIF.
        APPEND pu_ls_err TO gt_err_ins.
        CLEAR pu_ls_err.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_bapi_meas
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_NBAPI
*&      --> <FS_T014>_HEQUI
*&      --> <FS_T014>_KMCNT
*&      --> <FS_T014>_TRCNT
*&---------------------------------------------------------------------*
FORM f_bapi_meas USING pu_lt_nbapi    TYPE gtt_nbapi
                       pu_equnl       TYPE equnr
                       pu_hequi       TYPE hequi
                       pu_kmcnt       TYPE zdekm
                       pu_trcnt       TYPE zdetread
                       pu_disdt       TYPE zdisdt
                       pu_distm       TYPE zdistm
                       pu_lv_tabix.

  DATA  : lv_recval TYPE imrc_recdc,
          ls_err    TYPE gty_err.

  CLEAR : lv_recval, ls_err.

  READ TABLE pu_lt_nbapi INTO DATA(ls_nbapi)
  WITH KEY equnr = pu_equnl psort = 'TREAD' BINARY SEARCH.
  IF sy-subrc = 0.
    lv_recval = pu_trcnt.
    CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
      EXPORTING
        measurement_point = ls_nbapi-point
        reading_date      = pu_disdt
        reading_time      = pu_distm
        short_text        = 'UPDATE INSTALL TIRE TREAD'
        reader            = sy-uname
        origin_indicator  = 'A'
        recorded_value    = lv_recval
        prepare_update    = 'X'
        commit_work       = 'X'
        wait_after_commit = 'X'.
    IF sy-subrc EQ 0.

      ls_err-msgid  = '0k'.
      ls_err-msgty  = 'S'.
      ls_err-msgno  = '000'.
      ls_err-msgv1  = 'Tr 2 - Success bapi measurement'.
      IF sy-tabix = 1.
        ls_err-lineno = sy-tabix.
      ELSE.
        ls_err-lineno = pu_lv_tabix.
      ENDIF.
      APPEND ls_err TO gt_err_dis.
      CLEAR ls_err.

    ELSE.
      ROLLBACK WORK.

      ls_err-msgid  = '0k'.
      ls_err-msgty  = 'S'.
      ls_err-msgno  = '000'.
      ls_err-msgv1  = 'Tr 2 - Error bapi measurement'.
      IF sy-tabix = 1.
        ls_err-lineno = sy-tabix.
      ELSE.
        ls_err-lineno = pu_lv_tabix.
      ENDIF.
      APPEND ls_err TO gt_err_dis.
      CLEAR ls_err.

    ENDIF.
  ENDIF.

  CLEAR : ls_nbapi, lv_recval.

  READ TABLE pu_lt_nbapi INTO ls_nbapi
  WITH KEY equnr = pu_hequi psort = 'KILOMETER' BINARY SEARCH.
  IF sy-subrc = 0.
    lv_recval = pu_kmcnt.
    CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
      EXPORTING
        measurement_point = ls_nbapi-point
        reading_date      = pu_disdt
        reading_time      = pu_distm
        short_text        = 'UPDATE INSTALL TIRE KILOMETER'
        reader            = sy-uname
        origin_indicator  = 'A'
        recorded_value    = lv_recval
        prepare_update    = 'X'
        commit_work       = 'X'
        wait_after_commit = 'X'.
    IF sy-subrc EQ 0.

      ls_err-msgid  = '0k'.
      ls_err-msgty  = 'S'.
      ls_err-msgno  = '000'.
      ls_err-msgv1  = 'Tr 2 - Success bapi measurement'.
      IF sy-tabix = 1.
        ls_err-lineno = sy-tabix.
      ELSE.
        ls_err-lineno = pu_lv_tabix.
      ENDIF.
      APPEND ls_err TO gt_err_dis.
      CLEAR ls_err.

    ELSE.
      ROLLBACK WORK.

      ls_err-msgid  = '0k'.
      ls_err-msgty  = 'S'.
      ls_err-msgno  = '000'.
      ls_err-msgv1  = 'Tr 2 - Error bapi measurement'.
      IF sy-tabix = 1.
        ls_err-lineno = sy-tabix.
      ELSE.
        ls_err-lineno = pu_lv_tabix.
      ENDIF.
      APPEND ls_err TO gt_err_dis.
      CLEAR ls_err.

    ENDIF.
  ENDIF.

ENDFORM.
