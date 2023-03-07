*&---------------------------------------------------------------------*
*& Include          ZFIR003_F01B
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_write_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_write_data .
  DATA : ld_title(100).
  CLEAR: t_fieldcat, d_print, d_layout.
  REFRESH: t_fieldcat, t_list_top_of_page.

  ld_title = sy-title.


  PERFORM f_passing_field.
  PERFORM f_build_init.
  PERFORM f_eventtab_build   USING t_events[].
  PERFORM f_comment_build_main_header  USING t_list_top_of_page[] :
                                             ld_title.

*  PERFORM f_comment_build_header  USING t_list_top_of_page[].
*                                        'Year :   ' p_gjahr.
*                                        'Period : ' p_MONAT-LOW.

  PERFORM f_build_print  USING d_print.
  PERFORM f_build_layout USING d_layout
                               ''
                               ''.

  PERFORM f_alv_display  TABLES it_rpt.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_passing_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_passing_field .

  PERFORM f_fieldcats USING :
    'ROWCH'        ''              ''          'Check' ''    ''          ''              ''           ''     ''  'X'         ''        'X'       ''      '',
    'BELNR'        'IT_RPT'              ''          'Document Number' ''    ''          ''              ''           ''     ''  ''         ''        ''       ''      '',
    'BUKRS'        'IT_RPT'              ''          'Company Code' ''    ''          ''              ''           ''     ''  ''         ''        ''       ''      '',
    'GJAHR'        'IT_RPT'              ''          'Fiscal Year' ''    ''          ''              ''           ''     ''  ''         ''        ''       ''      '',
    'BLART'        'IT_RPT'              ''          'Document Type' ''    ''          ''              ''           ''     ''  ''         ''        ''       ''      '',
    'BLDAT'        ''              ''          'Document Date' ''    ''          ''              ''           ''     ''  ''         ''        ''       ''      '',
    'BUDAT'        ''              ''          'Posting Date' ''    ''          ''              ''           ''     ''  ''         ''        ''       ''      '',
    'ZSTAT'        'IT_RPT'              ''          'Document Status' ''    ''          ''              ''           ''     ''  ''         ''        ''       ''      '',
    'WAERS'        'IT_RPT'              ''          'Currency' ''    ''          ''              ''           ''     ''  ''         ''        ''       ''      '',
    'ZAMNT'        'IT_RPT'              ''          'Amount'  ''    ''          'WAERS'              ''           ''     ''  ''         ''        ''       ''      '',
    'ZVNDR'        'IT_RPT'              ''          'Vendor'  ''    ''          ''              ''           ''     ''  ''         ''        ''       ''      '',
    'ZNAME'        'IT_RPT'              ''          'Name'  ''    ''          ''              ''           ''     ''  ''         ''        ''       ''      '',
    'ZDESC'        'IT_RPT'              ''          'Description'  ''    ''          ''              ''           ''     ''  ''         ''        ''       ''      ''
    .

ENDFORM.

FORM f_user_command USING fu_ucomm TYPE sy-ucomm
                              i_selfield LIKE i_selfield.
*
  DATA: t_answer TYPE char1.

  PERFORM f_check_tick.

  CLEAR ok_code.
  ok_code = fu_ucomm.
*  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN '&PRINT'.
      PERFORM f_print_forms.
      i_selfield-refresh = 'X'.

    WHEN '&SELECT'.
      PERFORM f_toggle_select USING abap_true.
      i_selfield-refresh = 'X'.
    WHEN '&DESELECT'.
      PERFORM f_toggle_select USING abap_false.
      i_selfield-refresh = 'X'.
  ENDCASE.
ENDFORM.

FORM f_status USING fu_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZSTANDARD' EXCLUDING fu_extab.
  SET TITLEBAR  'TITLEALV'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_build_init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_build_init .
  d_alv_user_command = 'F_USER_COMMAND'.
  d_alv_pfstatus_set = 'F_STATUS'.
ENDFORM.
