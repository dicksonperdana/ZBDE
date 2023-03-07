*&---------------------------------------------------------------------*
*& Include          ZFIF_BDE_003_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  f_initialization
*&---------------------------------------------------------------------*
FORM f_initialization.

ENDFORM. " f_initialization

*&---------------------------------------------------------------------*
*& Form f_init_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_data .
  "Set for default ALV usage
  d_alv_user_command = 'F_USER_COMMAND'.
  d_alv_pfstatus_set = 'F_STATUS'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_get_data
*&---------------------------------------------------------------------*
FORM f_get_data.

  PERFORM f_validate_input USING s_bukrs[] s_gjahr[].

  PERFORM f_get_bkpf    USING    s_bukrs[] s_belnr[] s_gjahr[] s_budat[]
                        CHANGING gt_bkpf.

  PERFORM f_get_bsak    USING    gt_bkpf
                        CHANGING gt_bsak.

  PERFORM f_get_acdoca  USING    gt_bkpf
                        CHANGING gt_acdoca.

  PERFORM f_get_lfa1    USING    gt_acdoca
                        CHANGING gt_lfa1.

  PERFORM f_get_t012    USING    gt_acdoca
                        CHANGING gt_t012.

  PERFORM f_get_t012k   USING     gt_acdoca
                        CHANGING gt_t012k.

  PERFORM f_get_bnka    USING    gt_t012
                        CHANGING gt_bnka.

  PERFORM f_get_pa105   CHANGING gt_pa0105.

ENDFORM. " f_get_data

*&---------------------------------------------------------------------*
*& Form f_fill_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_fill_data.

  DATA ls_display LIKE LINE OF gt_display.

  CLEAR gt_display[].
  LOOP AT gt_bkpf ASSIGNING FIELD-SYMBOL(<fs_bkpf>).
    CLEAR ls_display.
    MOVE-CORRESPONDING: <fs_bkpf> TO ls_display.

    PERFORM f_fill_acdoca USING gt_acdoca
                                <fs_bkpf>-belnr
                                <fs_bkpf>-gjahr
                                <fs_bkpf>-bukrs
                       CHANGING ls_display.

    PERFORM f_fill_lfa1 USING <fs_bkpf>-belnr
                              <fs_bkpf>-gjahr
                              <fs_bkpf>-bukrs
                              gt_bkpf
                              gt_lfa1
                              gt_acdoca
                    CHANGING  ls_display.

    CASE <fs_bkpf>-blart.
      WHEN 'KZ'.
        ls_display-payment = 'Manual'.
      WHEN 'ZP'.
        ls_display-payment = 'H2H'.
      WHEN OTHERS.
        ls_display-payment = <fs_bkpf>-blart.
    ENDCASE.

    APPEND ls_display TO gt_display.
  ENDLOOP.
  UNASSIGN <fs_bkpf>.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_WRITE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_write_data.
  DATA : ld_title(100).
  CLEAR: t_fieldcat, d_print, d_layout.
  REFRESH: t_fieldcat, t_list_top_of_page.

  ld_title = sy-title.

  PERFORM f_passing_field.
  PERFORM f_eventtab_build   USING t_events[].
  PERFORM f_comment_build_main_header  USING t_list_top_of_page[] :
                                             ld_title.

*  PERFORM F_COMMENT_BUILD_HEADER  USING T_LIST_TOP_OF_PAGE[] :
*                                        'Year :   ' S_GJAHR-LOW,
*                                        'Period : ' S_MONAT-LOW.

  PERFORM f_build_print  USING d_print.
  PERFORM f_build_layout USING d_layout
                               ''
                               ''.

  PERFORM f_alv_display  TABLES gt_display.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_passing_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_passing_field.
  PERFORM f_fieldcats USING :
* FieldName       RefTable       RefFieldName     Text
*   Len     sum(X/'')   CurrFieldname   spot(X/'')   Hide   UOM
*   Input Key   Align   Checkbox   Decimals   Icon
    'ROWCH'        ''              ''          'Check'
      ''    ''          ''              ''           ''     ''
      'X'         ''        'X'       ''      '',
    'BELNR'        'BKPF'              'BELNR'          'Doc. Number'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'BUKRS'        'BKPF'              'BUKRS'          'Company Code'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'GJAHR'        'BKPF'              'GJAHR'          'Fiscal Year'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'BLART'        'BKPF'              'BLART'          'Doc. Type'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'BLDAT'        'BKPF'              'BLDAT'          'Doc. Date'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'BUDAT'        'BKPF'              'BUDAT'          'Posting Date'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'WAERS'        'BKPF'              'WAERS'          'Currency'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'WSL'        'ACDOCA'              'WSL'          'Amt. in Doc. Currency'
      ''    ''          'WAERS'              ''           ''     ''
      ''         ''        ''       ''      '',
    'SGTXT'        'ACDOCA'              'SGTXT'          'Description'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'LIFNR'        'ACDOCA'              'LIFNR'          'Vendor'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'NAME1'        'LFA1'              'NAME1'          'Name'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'HBKID'        'ACDOCA'              'HBKID'          'House Bank'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'HKTID'        'ACDOCA'              'HKTID'          'Account ID'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'BANKA'        'BNKA'              'BANKA'          'Bank'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'BANKN'        'T012K'              'BANKN'          'Account Number'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      '',
    'PAYMENT'        'GT_DISPLAY'              'PAYMENT'          'Payment Method'
      ''    ''          ''              ''           ''     ''
      ''         ''        ''       ''      ''.
ENDFORM.  " f_passing_field_prodis


*&---------------------------------------------------------------------*
*&      Form  f_alv_pfstatus_set
*&---------------------------------------------------------------------*
FORM f_status USING fu_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZSTANDARD' EXCLUDING fu_extab.
ENDFORM. "f_alv_status


*&---------------------------------------------------------------------*
*&      Form  f_alv_user_command
*&---------------------------------------------------------------------*
FORM f_user_command USING fu_ucomm TYPE sy-ucomm
                              fu_selfield TYPE slis_selfield.

  PERFORM f_check_tick.

  CASE fu_ucomm.
    WHEN 'PRINT'.
      PERFORM f_print.
      fu_selfield-refresh = 'X'.
    WHEN 'SELECT'.
      PERFORM f_toggle_select USING abap_true.
      fu_selfield-refresh = 'X'.
    WHEN 'DESELECT'.
      PERFORM f_toggle_select USING abap_false.
      fu_selfield-refresh = 'X'.
  ENDCASE.

ENDFORM. "f_alv_user_command


*&---------------------------------------------------------------------*
*& Form f_print
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_print .

  DATA : ls_header LIKE LINE OF gt_header.

  CLEAR gt_header[].

  LOOP AT gt_display INTO DATA(ls_display) WHERE rowch = abap_true.
    MOVE-CORRESPONDING : ls_display TO ls_header.

    "get company name
    PERFORM f_get_company_name USING    ls_header-bukrs
                               CHANGING ls_header-name2.

    PERFORM f_spell_amt USING     ls_header-wsl
                                  ls_header-waers
                        CHANGING  ls_header-say.

    PERFORM f_posted_by USING     ls_display-usnam
                        CHANGING  ls_header-pernr
                                  ls_header-last_name.

    APPEND ls_header TO gt_header.

    CLEAR ls_display.
  ENDLOOP.

  CHECK NOT gt_header IS INITIAL.

  DATA : ls_control_parameters TYPE  ssfctrlop,
         ls_output_options     TYPE  ssfcompop,
         ls_job_output_options TYPE  ssfcresop.

  DATA: v_formname TYPE tdsfname,
        v_funcname TYPE rs38l_fnam,
        lt_details TYPE ty_t_details,
        ls_details LIKE LINE OF lt_details.

  CLEAR : v_formname, v_funcname.
  v_formname = 'ZFIFSFBDE002'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = v_formname
    IMPORTING
      fm_name            = v_funcname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

*  "Print Parameter
  CLEAR : ls_control_parameters, ls_output_options.

*  "preview, fill X
  ls_control_parameters-preview  = 'X'.

*  "Show Dialog, fill blank
  ls_control_parameters-no_dialog = ''.
  ls_output_options-tdimmed = 'X'.
  ls_output_options-tddest = 'LOCL'.


  IF NOT gt_header IS INITIAL.

    CLEAR ls_header.
    LOOP AT gt_header INTO ls_header.
      AT FIRST.
        ls_output_options-tdnewid   = 'X'.
        ls_control_parameters-no_open   = space .
        ls_control_parameters-no_close  = 'X' .
      ENDAT.

      AT LAST.
        ls_control_parameters-no_close  = '' .
      ENDAT.

      CLEAR lt_details[].

      LOOP AT gt_bsak INTO DATA(ls_bsak) WHERE augbl = ls_header-belnr.
        MOVE-CORRESPONDING ls_bsak TO ls_details.
        DATA: lv_nomer TYPE sy-tabix.

        lv_nomer = lv_nomer + 1.
        ls_details-no = lv_nomer.

        SELECT SINGLE bktxt
          FROM bkpf
          INTO ls_details-bktxt
          WHERE belnr EQ ls_bsak-belnr
          AND bukrs EQ ls_bsak-bukrs
          AND gjahr EQ ls_bsak-gjahr.


        ls_details-amount = ls_bsak-wrbtr + ls_bsak-wmwst.
        IF ls_bsak-shkzg EQ 'H'.
          ls_details-amount = ls_details-amount * -1.
        ENDIF.

        APPEND ls_details TO lt_details.
        CLEAR: ls_bsak, ls_details.
      ENDLOOP.

      CALL FUNCTION v_funcname
        EXPORTING
          user_settings      = space
          control_parameters = ls_control_parameters
          output_options     = ls_output_options
          header             = ls_header
        IMPORTING
          job_output_options = ls_job_output_options
        TABLES
          details            = lt_details
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.

      ls_control_parameters-no_open   = 'X' .
      ls_output_options-tdnewid   = '' .

      CLEAR ls_header.
    ENDLOOP.

  ELSE.

    MESSAGE 'No data for print. Please check item print!' TYPE fiehc_con_msgty_s DISPLAY LIKE fiehc_con_msgty_e.

  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_TICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_tick .
  DATA : ref1 TYPE REF TO cl_gui_alv_grid.

  IF ref1 IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref1.
  ENDIF.

  IF NOT ref1 IS INITIAL.
    CALL METHOD ref1->check_changed_data.
  ENDIF.

ENDFORM.                    " F_CHECK_TICK


*&---------------------------------------------------------------------*
*& Form f_toggle_select
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ABAP_FALSE
*&---------------------------------------------------------------------*
FORM f_toggle_select  USING pv_select TYPE c.
  DATA ls_display TYPE ty_display.
  ls_display-rowch = pv_select.

  MODIFY gt_display FROM ls_display TRANSPORTING rowch WHERE belnr IS NOT INITIAL .
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_bkpf
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> S_BUKRS
*&      --> S_BELNR[]
*&      --> S_GJAHR
*&      --> S_BUDAT[]
*&      <-- GT_BKPF
*&---------------------------------------------------------------------*
FORM f_get_bkpf  USING    p_s_bukrs TYPE ty_r_bukrs
                          p_s_belnr TYPE ty_r_belnr
                          p_s_gjahr TYPE ty_r_gjahr
                          p_s_budat TYPE ty_r_budat
                 CHANGING p_gt_bkpf TYPE ty_t_bkpf.

  SELECT *
    FROM bkpf
    INTO CORRESPONDING FIELDS OF TABLE p_gt_bkpf
    WHERE bukrs IN p_s_bukrs
     AND belnr IN p_s_belnr
     AND gjahr IN p_s_gjahr
     AND budat IN p_s_budat
     AND stblg EQ ''
     AND ( blart EQ 'KZ' OR blart EQ 'ZP' ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_acdoca
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_BKPF
*&      <-- GT_ACDOCA
*&---------------------------------------------------------------------*
FORM f_get_acdoca  USING    p_gt_bkpf TYPE ty_t_bkpf
                   CHANGING p_gt_acdoca TYPE ty_t_acdoca.

  DATA(lt_bkpf) = p_gt_bkpf[].

  SELECT *
    FROM acdoca
    INTO CORRESPONDING FIELDS OF TABLE p_gt_acdoca
    FOR ALL ENTRIES IN lt_bkpf
    WHERE rbukrs EQ lt_bkpf-bukrs
    AND gjahr EQ lt_bkpf-gjahr
    AND belnr EQ lt_bkpf-belnr
    AND rldnr EQ '0L'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_lfa1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_ACDOCA
*&      <-- GT_LFA1
*&---------------------------------------------------------------------*
FORM f_get_lfa1  USING    p_gt_acdoca TYPE ty_t_acdoca
                 CHANGING p_gt_lfa1 TYPE ty_t_lfa1.

  DATA(lt_acdoca) = p_gt_acdoca[].

  SELECT lifnr name1
    FROM lfa1
    INTO TABLE p_gt_lfa1
    FOR ALL ENTRIES IN lt_acdoca
    WHERE lifnr EQ lt_acdoca-lifnr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_t012
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_ACDOCA
*&      <-- GT_T012
*&---------------------------------------------------------------------*
FORM f_get_t012  USING    p_gt_acdoca TYPE ty_t_acdoca
                 CHANGING p_gt_t012 TYPE ty_t_t012.

  DATA(lt_acdoca) = p_gt_acdoca[].

  SELECT bukrs hbkid banks bankl
    FROM t012
    INTO TABLE p_gt_t012
    FOR ALL ENTRIES IN lt_acdoca
    WHERE hbkid EQ lt_acdoca-hbkid
    AND bukrs EQ lt_acdoca-rbukrs.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_t012k
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_ACDOCA
*&      <-- GT_T012K
*&---------------------------------------------------------------------*
FORM f_get_t012k  USING    p_gt_acdoca TYPE ty_t_acdoca
                  CHANGING p_gt_t012k TYPE ty_t_t012k.

  DATA(lt_acdoca) = p_gt_acdoca[].

  SELECT bukrs hbkid hktid bankn
    FROM t012k
    INTO TABLE p_gt_t012k
    FOR ALL ENTRIES IN lt_acdoca
    WHERE hbkid EQ lt_acdoca-hbkid
    AND hktid EQ lt_acdoca-hktid
    AND bukrs EQ lt_acdoca-rbukrs.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_bnka
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_T012
*&      <-- GT_BNKA
*&---------------------------------------------------------------------*
FORM f_get_bnka  USING    p_gt_t012 TYPE ty_t_t012
                 CHANGING p_gt_bnka TYPE ty_t_bnka.

  DATA(lt_t012) = p_gt_t012[].

  SELECT banks bankl banka
    FROM bnka
    INTO TABLE p_gt_bnka
    FOR ALL ENTRIES IN lt_t012
    WHERE bankl EQ lt_t012-bankl
    AND banks EQ lt_t012-banks.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fill_acdoca
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_ACDOCA
*&      --> <FS_BKPF>_BELNR
*&      --> <FS_BKPF>_GJAHR
*&      --> <FS_BKPF>_BUKRS
*&      <-- LS_DISPLAY
*&---------------------------------------------------------------------*
FORM f_fill_acdoca  USING    p_gt_acdoca  TYPE ty_t_acdoca
                             p_belnr TYPE bkpf-belnr
                             p_gjahr TYPE bkpf-gjahr
                             p_bukrs TYPE bkpf-bukrs
                    CHANGING p_ls_display TYPE ty_display.

  READ TABLE p_gt_acdoca
  INTO DATA(ls_acdoca)
  WITH KEY rldnr = '0L' belnr = p_belnr rbukrs = p_bukrs gjahr = p_gjahr glaccount_type = 'C'.

  IF sy-subrc EQ 0.
    p_ls_display-wsl = ls_acdoca-wsl.
    p_ls_display-sgtxt = ls_acdoca-sgtxt.
    p_ls_display-hbkid = ls_acdoca-hbkid.
    p_ls_display-hktid = ls_acdoca-hktid.


    "get bank
    PERFORM f_fill_bank USING     ls_acdoca
                                  gt_t012
                                  gt_t012k
                                  gt_bnka
                        CHANGING  p_ls_display.
  ENDIF.

  READ TABLE p_gt_acdoca
  INTO ls_acdoca
  WITH KEY rldnr = '0L' belnr = p_belnr rbukrs = p_bukrs gjahr = p_gjahr koart = 'K'.

  IF sy-subrc EQ 0.
    p_ls_display-lifnr = ls_acdoca-lifnr.
  ENDIF.

  CLEAR ls_acdoca.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fill_lfa1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_LFA1
*&      --> GT_ACDOCA
*&      <-- LS_DISPLAY
*&---------------------------------------------------------------------*
FORM f_fill_lfa1  USING    p_belnr TYPE bkpf-belnr
                           p_gjahr TYPE bkpf-gjahr
                           p_bukrs TYPE bkpf-bukrs
                           p_gt_bkpf TYPE ty_t_bkpf
                           p_gt_lfa1 TYPE ty_t_lfa1
                           p_gt_acdoca TYPE ty_t_acdoca
                  CHANGING p_ls_display TYPE ty_display.

  READ TABLE p_gt_acdoca
  INTO DATA(ls_acdoca)
  WITH KEY rldnr = '0L' belnr = p_belnr rbukrs = p_bukrs gjahr = p_gjahr koart = 'K'.

  IF sy-subrc EQ 0.
    READ TABLE p_gt_lfa1
    INTO DATA(ls_lfa1)
    WITH KEY lifnr = ls_acdoca-lifnr.

    IF sy-subrc EQ 0.
      p_ls_display-name1 = ls_lfa1-name1.
    ENDIF.
  ENDIF.

  CLEAR: ls_acdoca, ls_lfa1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fill_bank
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_ACDOCA
*&      <-- LS_DISPLAY
*&---------------------------------------------------------------------*
FORM f_fill_bank  USING    p_ls_acdoca  TYPE ty_acdoca
                           p_gt_t012    TYPE ty_t_t012
                           p_gt_t012k   TYPE ty_t_t012k
                           p_gt_bnka    TYPE ty_t_bnka
                  CHANGING p_ls_display TYPE ty_display.

  READ TABLE p_gt_t012
  INTO DATA(ls_t012)
  WITH KEY hbkid = p_ls_acdoca-hbkid bukrs = p_ls_acdoca-rbukrs.

  IF sy-subrc EQ 0.
    READ TABLE p_gt_bnka
    INTO DATA(ls_bnka)
    WITH KEY bankl = ls_t012-bankl banks = ls_t012-banks.

    IF sy-subrc EQ 0.
      p_ls_display-banka = ls_bnka-banka.
    ENDIF.
  ENDIF.


  READ TABLE p_gt_t012k
  INTO DATA(ls_t012k)
  WITH KEY hbkid = p_ls_acdoca-hbkid hktid = p_ls_acdoca-hktid bukrs = p_ls_acdoca-rbukrs.

  IF sy-subrc EQ 0.
    p_ls_display-bankn = ls_t012k-bankn.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_company_name
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_HEADER_BUKRS
*&      --> ABAP_TRUE
*&      <-- LS_HEADER_NAME2
*&---------------------------------------------------------------------*
FORM f_get_company_name  USING    p_ls_header_bukrs TYPE bukrs
                         CHANGING p_ls_header_name2 TYPE name1.

  DATA adrnr TYPE t001-adrnr.

  SELECT SINGLE adrnr
    FROM t001
    INTO adrnr
    WHERE bukrs EQ p_ls_header_bukrs.

  IF sy-subrc EQ 0.
    SELECT SINGLE name1
        FROM adrc
        INTO p_ls_header_name2
        WHERE addrnumber EQ adrnr.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_spell_amt
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_HEADER_WSL
*&      --> LS_HEADER_WAERS
*&      <-- LS_HEADER_SAY
*&---------------------------------------------------------------------*
FORM f_spell_amt  USING    p_ls_header_wsl TYPE ty_acdoca-wsl
                           p_ls_header_waers TYPE ty_bkpf-waers
                  CHANGING p_ls_header_say.

  DATA spell TYPE spell.

  CALL FUNCTION 'SPELL_AMOUNT'
    EXPORTING
      amount    = p_ls_header_wsl
      currency  = p_ls_header_waers
*     FILLER    = ' '
      language  = 'E'
    IMPORTING
      in_words  = spell
    EXCEPTIONS
      not_found = 1
      too_large = 2
      OTHERS    = 3.

  IF sy-subrc <> 0.
    p_ls_header_say = '0'.
  ENDIF.

  IF sy-subrc EQ 0.
    p_ls_header_say = spell-word.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_posted_by
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISPLAY_USNAM
*&      <-- LS_HEADER_PERNR
*&      <-- LS_HEADER_LAST_NAME
*&---------------------------------------------------------------------*
FORM f_posted_by  USING    p_ls_display_usnam TYPE ty_display-usnam
                  CHANGING p_ls_header_pernr TYPE p_pernr
                           p_ls_header_last_name TYPE nachn.

  DATA lt_personal_data TYPE STANDARD TABLE OF bapip0002b WITH HEADER LINE.

  SELECT SINGLE pernr
    FROM pa0105
    INTO p_ls_header_pernr
    WHERE subty EQ '0001'
    AND usrty EQ '0001'
    AND usrid EQ p_ls_display_usnam.

  CALL FUNCTION 'BAPI_EMPLOYEE_GETDATA'
    EXPORTING
      employee_id   = p_ls_header_pernr
    TABLES
      personal_data = lt_personal_data.

  IF sy-subrc EQ 0.
    p_ls_header_last_name = lt_personal_data-last_name.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_bsak
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_BKPF
*&      <-- GT_BSAK
*&---------------------------------------------------------------------*
FORM f_get_bsak  USING    p_gt_bkpf TYPE ty_t_bkpf
                 CHANGING p_gt_bsak TYPE ty_t_bsak.

  DATA(lt_bkpf) = p_gt_bkpf[].

  SELECT *
    FROM bsak_view
    INTO CORRESPONDING FIELDS OF TABLE @p_gt_bsak
    FOR ALL ENTRIES IN @lt_bkpf
    WHERE bukrs EQ @lt_bkpf-bukrs
*    AND   gjahr EQ @lt_bkpf-gjahr
    AND   ( blart NE 'KZ' AND blart NE 'ZP' )
    AND   augbl EQ @lt_bkpf-belnr.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_pa105
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_PA0105
*&---------------------------------------------------------------------*
FORM f_get_pa105  CHANGING p_gt_pa0105 TYPE ty_t_pa0105.

  SELECT *
    FROM pa0105
    INTO CORRESPONDING FIELDS OF TABLE p_gt_pa0105
    WHERE subty EQ '0001'
    AND usrty EQ '0001'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_input
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> S_BUKRS[]
*&      --> S_GJAHR[]
*&---------------------------------------------------------------------*
FORM f_validate_input  USING    p_s_bukrs TYPE ty_r_bukrs
                                p_s_gjahr TYPE ty_r_gjahr.
  IF p_s_bukrs IS INITIAL.
    MESSAGE 'Please fill Company Code.' TYPE fiehc_con_msgty_s DISPLAY LIKE fiehc_con_msgty_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_s_gjahr IS INITIAL.
    MESSAGE 'Please fill Fiscal Year.' TYPE fiehc_con_msgty_s DISPLAY LIKE fiehc_con_msgty_e.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
