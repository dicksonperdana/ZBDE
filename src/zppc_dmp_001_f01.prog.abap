*&---------------------------------------------------------------------*
*& Include          ZPPC_DMP_001_F01
*&---------------------------------------------------------------------*

FORM AT_SEL_RAD1 .
  IF IT_TMP IS NOT INITIAL.
    READ TABLE IT_TMP INTO WA_TMP INDEX 1.
    IF WA_TMP-ZQTY1 IS NOT INITIAL.
      P_QTY1 = WA_TMP-ZQTY1.
      P_UOM1 = WA_TMP-ZQTY1_UOM.
      MODIFY SCREEN.
    ENDIF.

    IF WA_TMP-ZQTY2 IS NOT INITIAL.
      P_QTY2 = WA_TMP-ZQTY2.
      P_UOM2 = WA_TMP-ZQTY2_UOM.

      IF SCREEN-NAME = 'P_QTY1'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-NAME = 'P_UOM1'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDIF.


  IF P_QTY2 IS NOT INITIAL.
    IF SCREEN-NAME = 'P_QTY1'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.

    IF SCREEN-NAME = 'P_UOM1'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.

FORM F_GET_RAD1 .
  REFRESH : IT_TMP.
  SELECT * FROM ZTA_PP_001_DMP INTO CORRESPONDING FIELDS OF TABLE IT_TMP
    WHERE WERKS = P_WERKSB
    AND ZPERIOD = P_PERIOB
    AND ZYEAR = P_YEARSB
    AND MATNR = P_MATNRB.
ENDFORM.

FORM F_SAVE_RAD1 .
  CLEAR WA_TAB.

  WA_TAB-WERKS = P_WERKSB.
  WA_TAB-ZPERIOD = P_PERIOB.
  WA_TAB-ZYEAR = P_YEARSB.
  WA_TAB-MATNR = P_MATNRB.
  WA_TAB-ZQTY1 = P_QTY1.
  WA_tAB-ZQTY1_UOM = p_UOM1.

  INSERT ZTA_PP_001_DMP FROM WA_TAB.
  COMMIT WORK AND WAIT.

  IF SY-SUBRC =  0.
    MESSAGE 'Saved Success!' TYPE 'S' DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.

FORM F_UPDATE_RAD1.
  LOOP AT IT_TMP INTO WA_TMP.
    MOVE-CORRESPONDING WA_TMP TO WA_TAB.
    WA_TAB-ZQTY2 = P_QTY2.
    WA_TAB-ZQTY2_UOM = P_UOM2.
    UPDATE ZTA_PP_001_DMP FROM WA_TAB.
    COMMIT WORK AND WAIT.
    CLEAR WA_TAB.
  ENDLOOP.

ENDFORM.

FORM F_GET_RAD2 .

ENDFORM.

FORM F_DISPLAY_RAD2 .
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*      EXPORTING
*        text = 'Preparing report.......'.

  PERFORM F_LAYOUT_INIT USING I_LAYOUT.
  PERFORM F_EVENTTAB_BUILD USING I_EVENTS[].
  I_VARIANT-REPORT = SY-REPID.
  PERFORM F_PRINT_CONTROL.
  PERFORM F_BUILD_HEADER_LIST.
  PERFORM F_BUILD_SORT.
  PERFORM F_BUILD_FIELDCAT.
  PERFORM F_BUILD_DATA TABLES IT_TAB2.
  PERFORM PRINT_ALV.
ENDFORM.

FORM F_LAYOUT_INIT  USING   P_LAYOUT TYPE SLIS_LAYOUT_ALV.
  P_LAYOUT-DETAIL_POPUP       = YES.
  P_LAYOUT-NUMC_SUM           = YES.
  P_LAYOUT-COLWIDTH_OPTIMIZE  = YES.
  P_LAYOUT-ZEBRA              = NO.
  P_LAYOUT-GET_SELINFOS        = YES.
  P_LAYOUT-COLTAB_FIELDNAME   = 'COLINFO'.
  P_LAYOUT-SUBTOTALS_TEXT     = 'SubTotal'.
  P_LAYOUT-TOTALS_TEXT        = 'Total'.
*  p_layout-box_fieldname      = 'CHECK'.
ENDFORM.

FORM F_EVENTTAB_BUILD  USING P_EVENTS TYPE SLIS_T_EVENT.
  DATA:
    LS_EVENT TYPE SLIS_ALV_EVENT.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = P_EVENTS.

  READ TABLE P_EVENTS WITH KEY NAME = SLIS_EV_TOP_OF_PAGE INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE SLIS_EV_TOP_OF_PAGE TO: LS_EVENT-FORM, LS_EVENT-NAME.
    APPEND LS_EVENT TO P_EVENTS.
  ENDIF.

  READ TABLE P_EVENTS WITH KEY NAME = SLIS_EV_END_OF_PAGE INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE SLIS_EV_END_OF_PAGE TO: LS_EVENT-FORM, LS_EVENT-NAME.
    APPEND LS_EVENT TO P_EVENTS.
  ENDIF.

*  READ TABLE p_events WITH KEY name = slis_ev_end_of_list INTO ls_event.
*  IF sy-subrc = 0.
*    MOVE slis_ev_end_of_list TO: ls_event-form, ls_event-name.
*    APPEND ls_event TO p_events.
*  ENDIF.

  READ TABLE P_EVENTS WITH KEY NAME = SLIS_EV_TOP_OF_LIST INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE SLIS_EV_TOP_OF_LIST TO: LS_EVENT-FORM, LS_EVENT-NAME.
    APPEND LS_EVENT TO P_EVENTS.
  ENDIF.

  READ TABLE P_EVENTS WITH KEY NAME = SLIS_EV_PF_STATUS_SET INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE SLIS_EV_PF_STATUS_SET TO: LS_EVENT-FORM, LS_EVENT-NAME.
    APPEND LS_EVENT TO P_EVENTS.
  ENDIF.

  READ TABLE P_EVENTS WITH KEY NAME = SLIS_EV_USER_COMMAND INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE SLIS_EV_USER_COMMAND TO: LS_EVENT-FORM, LS_EVENT-NAME.
    APPEND LS_EVENT TO P_EVENTS.
  ENDIF.

ENDFORM.

FORM F_PRINT_CONTROL .
  I_PRINT-NO_PRINT_SELINFOS      = YES.
  I_PRINT-NO_PRINT_LISTINFOS     = YES.
  I_PRINT-RESERVE_LINES          = 0.
  I_PRINT-NO_CHANGE_PRINT_PARAMS = YES.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BUILD_HEADER_LIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_BUILD_HEADER_LIST .
  DATA: LWA_LIST_TOP LIKE LINE OF I_LIST_TOP,
        LV_TEXT      LIKE SY-TITLE,
        GS_LINE      TYPE SLIS_LISTHEADER,
        VTIME        TYPE CHAR4,
        VTITLE       TYPE CHAR50,
        VSLOW        TYPE CHAR10,
        VSHIGH       TYPE CHAR10,
        VPLANT       TYPE STRING,
        VVERID       TYPE STRING,
        VSTART       TYPE STRING,
        VEND         TYPE STRING,
        VNAME1       TYPE T001W-NAME1.

*  CLEAR vplant.
*  CONCATENATE 'FM Area : ' p_fikrs-low INTO vplant SEPARATED BY space.
*
*
*  CLEAR: vverid,vtime.
*  CONCATENATE 'Fiscal Years : ' p_gjahr-low INTO vverid SEPARATED BY space.
*
*  CLEAR vstart.
*  CONCATENATE 'Funds Center : ' p_fictr-low ' - ' p_fictr-high INTO vstart SEPARATED BY space.
*  IF p_fictr-high IS INITIAL.
*    CLEAR vstart.
*    CONCATENATE 'Funds Center : ' p_fictr-low ' - ' p_fictr-low INTO vstart SEPARATED BY space.
*  ENDIF.
*  IF p_fictr-low IS INITIAL.
*    CLEAR vstart.
*    vstart = 'Funds Center : All'.
*  ENDIF.
*
*  CLEAR vend.
*  CONCATENATE 'Commitment item : ' p_fipex-low ' - ' p_fipex-high INTO vend SEPARATED BY space.
*  IF p_fipex-high IS INITIAL.
*    CLEAR vend.
*    CONCATENATE 'Commitment item : ' p_fipex-low ' - ' p_fipex-low INTO vend SEPARATED BY space.
*  ENDIF.
*  IF p_fipex-low IS INITIAL.
*    CLEAR vend.
*    vend = 'Commitment item : All'.
*  ENDIF.


  REFRESH I_LIST_TOP.
  CLEAR LWA_LIST_TOP.
  LWA_LIST_TOP-TYP = 'H'.
  LWA_LIST_TOP-KEY = NO.
  LWA_LIST_TOP-INFO = 'Report Quanity Planning vs Join Survey'.
  APPEND LWA_LIST_TOP TO I_LIST_TOP.

  CLEAR LWA_LIST_TOP.
  LWA_LIST_TOP-TYP = 'S'.
  LWA_LIST_TOP-KEY = ''.
  LWA_LIST_TOP-INFO = VPLANT.
  APPEND LWA_LIST_TOP TO I_LIST_TOP.

  CLEAR LWA_LIST_TOP.
  LWA_LIST_TOP-TYP = 'S'.
  LWA_LIST_TOP-KEY = ''.
  LWA_LIST_TOP-INFO = VVERID.
  APPEND LWA_LIST_TOP TO I_LIST_TOP.

  CLEAR LWA_LIST_TOP.
  LWA_LIST_TOP-TYP = 'S'.
  LWA_LIST_TOP-KEY = ''.
  LWA_LIST_TOP-INFO = VSTART.
  APPEND LWA_LIST_TOP TO I_LIST_TOP.

  CLEAR LWA_LIST_TOP.
  LWA_LIST_TOP-TYP = 'S'.
  LWA_LIST_TOP-KEY = ''.
  LWA_LIST_TOP-INFO = VEND.
  APPEND LWA_LIST_TOP TO I_LIST_TOP.
ENDFORM.

FORM F_BUILD_SORT .
  DATA: L_SORT TYPE SLIS_SORTINFO_ALV.


  DEFINE DEF_SORT.
    CLEAR L_SORT.
*Sort position
    L_SORT-SPOS      = &1.
    L_SORT-FIELDNAME = &2.

*Sort by ascending
    L_SORT-UP        = YES.

*Subtot key
    L_SORT-SUBTOT    = YES.

*Separate Line between Group
    L_SORT-GROUP     = 'MJSV'.
    APPEND L_SORT TO I_SORT.
  END-OF-DEFINITION.

  DEF_SORT '01' 'MJSV'.
*  DEF_SORT '01' 'COUNT'
ENDFORM.

FORM F_BUILD_FIELDCAT .
  DATA LS_POS TYPE SY-CUCOL.
  CLEAR LS_POS.
  REFRESH I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'WERKS'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-KEY           = 'X'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Plant'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'BLDAT'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Posting Date'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'ZPER'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Period'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'ZYEAR'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'Years'.
  WA_FIELDCAT-SELTEXT_L     = ''. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'MPLAN'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Material Plan'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'ZQTY'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Quantity'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'ZAMT'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Amount'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'MACT'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Material Actual'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'MPLAN'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Material Actual'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'HACT'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Hasil Actual'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'MJSV'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Material Join Survey'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'HJSV'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Hasil Join Survey'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.

  LS_POS = LS_POS + 1.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-COL_POS       = LS_POS.
  WA_FIELDCAT-FIELDNAME     = 'DEV'.
  WA_FIELDCAT-TABNAME       = WA_FIELDCAT-CTABNAME = WA_FIELDCAT-QTABNAME = 'IT_TAB2'.
  WA_FIELDCAT-JUST          = 'L'.
  WA_FIELDCAT-SELTEXT_L     = 'Deviasi'. "ditambahkan jika title didefine sendiri
  WA_FIELDCAT-DDICTXT       = 'L'.      "ditambahkan jika title didefine sendiri
  APPEND WA_FIELDCAT TO I_FIELDCAT.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BUILD_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_TAB2
*&---------------------------------------------------------------------*
FORM F_BUILD_DATA TABLES LT_DATA STRUCTURE WA_TAB2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PRINT_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PRINT_ALV .
  CLEAR I_LAYOUT.
  I_LAYOUT-COLWIDTH_OPTIMIZE  = 'X'.
  I_LAYOUT-ZEBRA              = 'X'.
  DATA:
    I_SAVE VALUE 'U'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = GS_EXTRACT1-REPORT
*     i_callback_pf_status_set  =
*     i_callback_user_command   =
      IS_LAYOUT          = I_LAYOUT
      IT_FIELDCAT        = I_FIELDCAT
      I_SAVE             = I_SAVE
      IS_VARIANT         = I_VARIANT
      IT_EVENTS          = I_EVENTS[]
      IT_SORT            = I_SORT
      IS_PRINT           = I_PRINT
*     i_html_height_top  = 110
    TABLES
      T_OUTTAB           = IT_TAB2
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.

FORM TOP_OF_PAGE.                                           "#EC CALLED
  DATA: LFLG_GRID TYPE C,
        LV_TEXT   LIKE SY-TITLE.
  IMPORT LFLG_GRID FROM MEMORY ID 'ALV_GRID_TOP_OF_PAGE'.
  IF LFLG_GRID = YES.
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
*       i_logo             = 'ZLOGO_NIPINDO'
        IT_LIST_COMMENTARY = I_LIST_TOP.
*              I_END_OF_LIST_GRID = i_list_end.
  ELSE.
    PERFORM F_PRINT_HEADER USING TEXT-100 LV_TEXT SPACE.
  ENDIF.
ENDFORM.                    "TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  " SET PF-STATUS 'STATALV' EXCLUDING fcode.
  " SET PF-STATUS 'ZSTANDARD'.
*  SET PF-STATUS 'STANDARD'.
*  SET TITLEBAR  'TITLEALV'.
ENDFORM.                    "PF_STATUS_SET

*&---------------------------------------------------------------------*
*&Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM
                        I_SELFIELD LIKE I_SELFIELD.
  DATA: T_ANSWER TYPE CHAR1.

  DATA: F_SUBRC LIKE SY-SUBRC.
*  READ TABLE it_tab2 INDEX i_selfield-tabindex INTO s_arseg.

  CASE R_UCOMM.
    WHEN '&IC1'.
*        CASE i_SELFIELD-SEL_TAB_FIELD.
*    WHEN 'PRUEFLOS'.
*        ENDCASE.

*    WHEN '&DOWNLOAD'.
*      PERFORM f_ucomm_download.

  ENDCASE.

ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&Form  END_OF_PAGE
*&---------------------------------------------------------------------*
FORM END_OF_PAGE.                                           "#EC CALLED
  DATA: L_POS(3)   TYPE N VALUE 0, L_FOOT(30) VALUE '*** END OF PAGE ***'.
  L_POS = ( SY-LINSZ / 2 ) - ( STRLEN( L_FOOT ) / 2 ).
  ULINE.
  WRITE: /, AT L_POS L_FOOT.

ENDFORM.                    "END_OF_PAGE
*&---------------------------------------------------------------------*
*&Form  END_OF_LIST
*&---------------------------------------------------------------------*
FORM END_OF_LIST.                                           "#EC CALLED
  DATA: LV_PAGE_COUNT(3).
  DATA: L_POS(3)   TYPE N VALUE 0, L_FOOT(30) VALUE '*** end of report ***'.
  L_POS = ( SY-LINSZ / 2 ) - ( STRLEN( L_FOOT ) / 2 ).
  ULINE.
  WRITE: /, AT L_POS L_FOOT.
  CHECK SY-UCOMM EQ '&rnt'.
  V_TOTAL = SY-PAGNO.

  CALL FUNCTION 'reuse_alv_commentary_write'
    EXPORTING
*     I_LOGO             = 'zlogo_nipindo'
      I_END_OF_LIST_GRID = I_LIST_END.
ENDFORM.                    "END_OF_LIST
*&---------------------------------------------------------------------*
*&Form  f_print_header
*&---------------------------------------------------------------------*
FORM F_PRINT_HEADER  USING    P_TITLE1 LIKE SY-TITLE
                              P_TITLE2 LIKE SY-TITLE
                              P_TITLE3 LIKE SY-TITLE.

  DATA: L_POST1 TYPE I, L_POST2 TYPE I, L_POST3 TYPE I, L_POST4 TYPE I, L_POSIN TYPE I.

  L_POST1 = ( SY-LINSZ / 2 ) - ( STRLEN( P_TITLE1 ) / 2 ).
  L_POST2 = ( SY-LINSZ / 2 ) - ( STRLEN( P_TITLE2 ) / 2 ).
  L_POST3 = ( SY-LINSZ / 2 ) - ( STRLEN( P_TITLE3 ) / 2 ).
  L_POST4 = L_POST1 - 15.
  L_POSIN = SY-LINSZ - 22.

  WRITE:  / 'Report  :', SY-CPROG.
  WRITE AT  L_POST1 'TMS Report'.
  WRITE AT  L_POSIN 'Date :'.
  WRITE     SY-DATUM.

  WRITE:  / 'Cli/Sys :', SY-MANDT, '/', SY-SYSID.
  WRITE AT  L_POST4 SY-TITLE.
  WRITE AT: L_POSIN 'Time :'.
  WRITE     SY-UZEIT.

  WRITE:  / 'UserID  :', SY-UNAME.
  WRITE AT  L_POSIN 'Page :'.

  IF V_TOTAL IS NOT INITIAL.
    WRITE: (3) SY-PAGNO NO-ZERO, 'of', (3) V_TOTAL.
  ELSE.
    WRITE (3) SY-PAGNO NO-ZERO.
  ENDIF.
ENDFORM.                    "f_print_header
