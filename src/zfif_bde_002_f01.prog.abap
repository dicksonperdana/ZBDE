
FORM f_get_data .
  REFRESH : it_bkpf, it_vbsegk, it_acdoca, it_lfa1, it_vbsegs, it_acdoca2 .

  REFRESH : lt_crtdby, lt_aprvby, lt_pstdby.

  SELECT belnr, bukrs, gjahr, blart, bldat, budat, bstat, waers, ppnam, usnam, xblnr, bktxt
    FROM bkpf
    INTO CORRESPONDING FIELDS OF TABLE @it_bkpf
    WHERE bukrs EQ @p_bukrs
    AND belnr IN @p_belnr
    AND gjahr IN  @p_gjahr
    AND budat IN @p_budat
    AND stblg EQ ' '
    AND ( blart EQ 'KR'OR blart EQ 'RE' )
    AND ppnam IN @p_ppnam
    AND usnam IN @p_usnam
    .



  SORT it_bkpf ASCENDING BY belnr.

  IF it_bkpf IS NOT INITIAL.
    SELECT belnr bukrs gjahr wrbtr lifnr sgtxt
      FROM vbsegk
      INTO CORRESPONDING FIELDS OF TABLE it_vbsegk
      FOR ALL ENTRIES IN it_bkpf
      WHERE belnr EQ it_bkpf-belnr
      AND bukrs EQ it_bkpf-bukrs
      AND gjahr EQ it_bkpf-gjahr.

    SELECT belnr rbukrs gjahr wsl lifnr sgtxt buzei ebeln
      FROM acdoca
      INTO CORRESPONDING FIELDS OF TABLE it_acdoca
      FOR ALL ENTRIES IN it_bkpf
      WHERE belnr EQ it_bkpf-belnr
      AND rbukrs EQ it_bkpf-bukrs
      AND gjahr EQ it_bkpf-gjahr
      AND rldnr EQ '0L'
      AND koart EQ 'K'.

    SELECT belnr rbukrs gjahr wsl lifnr sgtxt buzei ebeln
    FROM acdoca
    INTO CORRESPONDING FIELDS OF TABLE it_hcdoca
    FOR ALL ENTRIES IN it_bkpf
    WHERE belnr EQ it_bkpf-belnr
    AND rbukrs EQ it_bkpf-bukrs
    AND gjahr EQ it_bkpf-gjahr
    AND rldnr EQ '0L'
    AND ebeln NE ''.
*      AND koart EQ 'K'
    SORT it_hcdoca ASCENDING BY ebeln.
    DELETE ADJACENT DUPLICATES FROM it_hcdoca COMPARING ebeln.



    SELECT belnr bukrs gjahr saknr kostl ps_psp_pnr sgtxt wrbtr buzei
      FROM vbsegs
      INTO CORRESPONDING FIELDS OF TABLE it_vbsegs
      FOR ALL ENTRIES IN it_bkpf
      WHERE belnr EQ it_bkpf-belnr
      AND bukrs EQ it_bkpf-bukrs
      AND gjahr EQ it_bkpf-gjahr.

    SELECT belnr rbukrs gjahr racct rcntr ps_psp_pnr sgtxt wsl buzei
      FROM acdoca
      INTO CORRESPONDING FIELDS OF TABLE it_acdoca2
      FOR ALL ENTRIES IN it_bkpf
      WHERE belnr EQ it_bkpf-belnr
      AND rbukrs EQ it_bkpf-bukrs
      AND gjahr EQ it_bkpf-gjahr
      AND rldnr EQ '0L'
      AND koart NE 'K'.


    SELECT bukrs belnr gjahr wf_id statu rqsnr appnr crdat
      FROM ZFIT_BDE_001
      INTO CORRESPONDING FIELDS OF TABLE lt_crtdby
      FOR ALL ENTRIES IN it_bkpf
       WHERE belnr EQ it_bkpf-belnr
    AND bukrs EQ it_bkpf-bukrs
*    AND gjahr EQ it_bkpf-gjahr
    AND statu EQ 'S'.

    SELECT bukrs belnr gjahr wf_id statu appnr crdat
    FROM ZFIT_BDE_001
    INTO CORRESPONDING FIELDS OF TABLE  lt_aprvby
    FOR ALL ENTRIES IN it_bkpf
     WHERE belnr EQ it_bkpf-belnr
    AND bukrs EQ it_bkpf-bukrs
*AND gjahr EQ it_bkpf-gjahr
    AND statu LIKE 'A%'
          .


    SELECT bukrs belnr gjahr wf_id statu appnr crdat
    FROM ZFIT_BDE_001
    INTO CORRESPONDING FIELDS OF TABLE lt_pstdby
    FOR ALL ENTRIES IN it_bkpf
     WHERE belnr EQ it_bkpf-belnr
    AND bukrs EQ it_bkpf-bukrs
*AND gjahr EQ it_bkpf-gjahr
    AND statu EQ 'P'.

*    SELECT belnr, bukrs, gjahr, blart, bldat, budat, bstat, waers, ppnam, usnam, xblnr, bktxt
*FROM bkpf
*INTO CORRESPONDING FIELDS OF TABLE it_fbkpf
*FOR ALL ENTRIES IN it_bkpf
**WHERE bukrs IN it_bkpf-bukrs
*WHERE belnr EQ it_bkpf-belnr
**AND gjahr IN  p_gjahr
**AND budat IN p_budat
*AND stblg EQ ' '
*AND ( blart EQ 'KR'OR blart EQ 'RE' ).



  ENDIF.

  DELETE ADJACENT DUPLICATES FROM it_acdoca COMPARING belnr.
  SORT it_acdoca ASCENDING BY belnr.
  SORT it_vbsegk ASCENDING BY belnr.

ENDFORM.

FORM f_collect_data .

  DATA :
*         zstat TYPE char20,
*    waers TYPE bkpf-waers,
    va_zamnt TYPE p DECIMALS 2,
    va_zvndr TYPE lifnr,
*    va_zname TYPE lfa1-name1,
    va_zdesc TYPE sgtxt,

    vb_zamnt TYPE p DECIMALS 2,
    vb_zvndr TYPE lifnr,
*    vb_zname TYPE lfa1-name1,
    vb_zdesc TYPE sgtxt.

  REFRESH : it_rpt.

  LOOP AT it_bkpf INTO wa_bkpf.
    wa_rpt-numb = sy-tabix.
    wa_rpt-belnr = wa_bkpf-belnr.
    wa_rpt-bukrs = wa_bkpf-bukrs.
    wa_rpt-gjahr = wa_bkpf-gjahr.
    wa_rpt-bldat = wa_bkpf-bldat.
    wa_rpt-budat = wa_bkpf-budat.
    wa_rpt-bstat = wa_bkpf-bstat.
    wa_rpt-waers = wa_bkpf-waers.
    wa_rpt-blart = wa_bkpf-blart.
    wa_rpt-xblnr = wa_bkpf-xblnr.
    wa_rpt-bktxt = wa_bkpf-bktxt.

    IF wa_rpt-bstat EQ 'V'.
*      CLEAR wa_vbsegk.
      READ TABLE it_vbsegk INTO wa_vbsegk WITH KEY belnr = wa_bkpf-belnr bukrs = wa_bkpf-bukrs gjahr = wa_bkpf-gjahr BINARY SEARCH.
      wa_rpt-zstat = 'PARKING'.
*      IF wa_rpt-waers = 'IDR'.
*        wa_rpt-zamnt = wa_vbsegk-wrbtr * 100.
*      ELSE.
      wa_rpt-zamnt = wa_vbsegk-wrbtr.
*      ENDIF.
*      wa_rpt-zamnt = wa_vbsegk-wrbtr.
      wa_rpt-zdesc = wa_vbsegk-sgtxt.
      wa_rpt-zvndr = wa_vbsegk-lifnr.
    ELSEIF wa_rpt-bstat EQ ''.
      CLEAR wa_acdoca.
      READ TABLE it_acdoca INTO wa_acdoca WITH KEY belnr = wa_bkpf-belnr rbukrs = wa_bkpf-bukrs gjahr = wa_bkpf-gjahr BINARY SEARCH.
      wa_rpt-zstat = 'POSTED'.
*      IF wa_rpt-waers = 'IDR'.
*        wa_rpt-zamnt = wa_acdoca-wsl * 100.
*      ELSE.
      wa_rpt-zamnt = wa_acdoca-wsl.
*      ENDIF.
*      wa_rpt-zamnt = wa_acdoca-wsl.
      wa_rpt-zdesc = wa_acdoca-sgtxt.
      wa_rpt-zvndr = wa_acdoca-lifnr.
    ENDIF.

    SELECT name1 FROM lfa1 INTO wa_rpt-zname
      WHERE lifnr = wa_rpt-zvndr.
    ENDSELECT.
    APPEND wa_rpt TO it_rpt.
  ENDLOOP.
  CLEAR : wa_bkpf.
  CLEAR : wa_rpt.



ENDFORM.

FORM f_display_data .
*  PERFORM f_layout_init USING i_layout.
*  PERFORM f_eventtab_build USING i_events[].
*  PERFORM f_print_control.
*  PERFORM f_build_header_list.
*  PERFORM f_build_sort.
*  PERFORM f_build_fieldcat.
*  PERFORM print_alv.

ENDFORM.

FORM f_print_forms.
*  REFRESH it_frm.
*  it_frm[] = it_rpt[].

  REFRESH : lt_header, lt_item, lt_footer.
  LOOP AT it_rpt INTO wa_frm WHERE rowch IS NOT INITIAL.
    PERFORM f_print_get_header.
  ENDLOOP.
  CLEAR wa_frm.

  CHECK lt_header IS NOT INITIAL.
  PERFORM f_call_print.
  CLEAR : ls_control_parameters, ls_output_options.
  ls_control_parameters-preview  = 'X'.
*  "Show Dialog, fill blank
  ls_control_parameters-no_dialog = 'X'.
  ls_output_options-tdimmed = 'X'.
*  ls_output_options-tddest = 'LOCL'.


  IF lt_header IS NOT INITIAL.
    LOOP AT lt_header INTO ls_header.
      AT FIRST.
        ls_output_options-tdnewid   = 'X'.
        ls_control_parameters-no_open   = space .
        ls_control_parameters-no_close  = 'X' .
      ENDAT.
      AT LAST.
*        ls_control_parameters-no_open   = 'X'.
        ls_control_parameters-no_close  = ' ' .
      ENDAT.

      PERFORM f_print_get_item.
      PERFORM f_print_get_footer.
      PERFORM f_print_run.
      ls_control_parameters-no_open   = 'X' .
      ls_output_options-tdnewid   = ' ' .
      "CLEAR ls_header.
      CLEAR ls_footer.
    ENDLOOP.
    CLEAR ls_header.

  ENDIF.

ENDFORM.

FORM f_print_get_header .
  DATA :
    ls_companycode_detail	 TYPE bapi0002_2,
    ls_companycode_address TYPE bapi0002_3,
    ls_word                TYPE spell,
    ls_po_no               TYPE acdoca-ebeln,
    vline                  TYPE i.

  DATA :
      ls_rpt2 TYPE ty_rpt.

*  MOVE-CORRESPONDING: wa_frm TO ls_header.

***  CLEAR ls_rpt2.
***  LOOP AT it_frm INTO ls_rpt2.
***    "internal tab header
  ls_header-numb = wa_frm-numb.
  ls_header-comp_code = wa_frm-bukrs.
  ls_header-zstat = wa_frm-zstat.
  ls_header-blart = wa_frm-blart.

  CALL FUNCTION 'BAPI_COMPANYCODE_GETDETAIL'
    EXPORTING
      companycodeid       = ls_header-comp_code
    IMPORTING
      companycode_detail  = ls_companycode_detail
      companycode_address = ls_companycode_address.

  ls_header-comp_name = ls_companycode_address-name."ls_companycode_detail-comp_name.
  ls_header-vendor_name = wa_frm-zname.
  ls_header-vendor_code = wa_frm-zvndr.
  ls_header-inv_no = wa_frm-bktxt."wa_frm-xblnr.
  ls_header-tax_no = wa_frm-xblnr.
  ls_header-curr = wa_frm-waers.
*    ls_header-tot_amount = <fs_apt>-zamnt.
  wa_frm-zamnt = wa_frm-zamnt * -1.
  WRITE wa_frm-zamnt TO ls_header-tot_amount
  CURRENCY ls_header-curr.
  CONDENSE ls_header-tot_amount NO-GAPS .
  WRITE ls_header-tot_amount.

  CALL FUNCTION 'SPELL_AMOUNT'
    EXPORTING
      amount   = wa_frm-zamnt
      currency = wa_frm-waers
      language = sy-langu
    IMPORTING
      in_words = ls_word.

  ls_header-terbilang = ls_word-word.
  ls_header-doc_no = wa_frm-belnr.
  ls_header-doc_date = wa_frm-bldat.
  ls_header-paym_for = wa_frm-zdesc.


  CLEAR : ls_header-clear_no, ls_header-clear_date.
  SELECT SINGLE augbl augdt
    FROM bsak
    INTO ( ls_header-clear_no,ls_header-clear_date )
*    WHERE lifnr = wa_frm-zvndr
    WHERE belnr = wa_frm-belnr
    AND bukrs = wa_frm-bukrs.

*    AND bldat =  wa_frm-bldat
*    AND bukrs = wa_frm-bukrs
  .
*  ENDSELECT.
*  SELECT
  CLEAR wa_hcdoca.
  DESCRIBE TABLE it_hcdoca LINES vline.
*  READ TABLE it_hcdoca INTO wa_hcdoca WITH KEY belnr = wa_frm-belnr .
  LOOP AT it_hcdoca INTO wa_hcdoca WHERE belnr = wa_frm-belnr.
    IF vline EQ '1'.
      ls_header-po_no = wa_hcdoca-ebeln.
    ELSEIF vline > '1'.
      ls_po_no = wa_hcdoca-ebeln.
      CONCATENATE ls_header-po_no ',' ls_po_no  INTO ls_header-po_no SEPARATED BY space.
      IF ls_header-po_no+0(3) EQ ' , '.
        REPLACE ' , ' IN ls_header-po_no+0(3) WITH ''. " ALL OCCURRENCES OF ',' IN v_ppn_persent WITH ''.
        CONDENSE ls_header-po_no NO-GAPS.
      ENDIF.
    ENDIF.
  ENDLOOP.

  APPEND ls_header TO lt_header.
  CLEAR ls_header.


*  ENDLOOP.


ENDFORM.

FORM f_print_get_item .
  DATA :
    ls_companycode_detail	TYPE bapi0002_2,
    ls_word               TYPE spell.

  DATA :
      ls_rpt2 TYPE ty_rpt.

*  CLEAR ls_rpt2.
  CLEAR ls_rpt2.
*  LOOP AT it_frm INTO ls_rpt2 WHERE numb = ls_header-numb.
  REFRESH : lt_item_tmp, lt_item.

  IF ls_header-zstat EQ 'PARKING'.
*    ls_item-no = sy-tabix.
    SORT it_vbsegs ASCENDING BY buzei.
    LOOP AT it_vbsegs INTO wa_vbsegs WHERE belnr = ls_header-doc_no AND bukrs = ls_header-comp_code.

      ls_item-acc_code = wa_vbsegs-saknr.
      SELECT SINGLE txt50 FROM skat
        INTO ls_item-acc_desc WHERE saknr = wa_vbsegs-saknr.

      ls_item-cost_ctr = wa_vbsegs-kostl.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = wa_vbsegs-ps_psp_pnr
        IMPORTING
          output = ls_item-wbs_code.
      ls_item-paym_dtl = wa_vbsegs-sgtxt.

      ls_item-curr = ls_header-curr.
*      IF ls_item-curr = 'IDR'.
*      ls_item-amount = wa_vbsegs-wrbtr ."* 100.
*      ENDIF.

      IF wa_vbsegs-wrbtr <= -1.
        WRITE wa_vbsegs-wrbtr TO ls_item-amount CURRENCY ls_header-curr.
        SHIFT ls_item-amount RIGHT DELETING TRAILING '-'.
        CONDENSE ls_item-amount.
        CONCATENATE '-' ls_item-amount INTO ls_item-amount.

      ELSE.
        WRITE wa_vbsegs-wrbtr TO ls_item-amount CURRENCY ls_header-curr.
      ENDIF.


      APPEND ls_item TO lt_item_tmp.
      CLEAR ls_item.

    ENDLOOP.
    CLEAR wa_vbsegs.



*
*
  ELSEIF ls_header-zstat EQ 'POSTED'.
    SORT it_acdoca2 ASCENDING BY buzei.
    CLEAR wa_acdoca2.
    LOOP AT it_acdoca2 INTO wa_acdoca2 WHERE belnr = ls_header-doc_no AND rbukrs = ls_header-comp_code AND wsl NE '0'.

      ls_item-acc_code = wa_acdoca2-racct.
      SELECT SINGLE txt50 FROM skat
        INTO ls_item-acc_desc WHERE saknr = wa_acdoca2-racct.

      ls_item-cost_ctr = wa_acdoca2-rcntr.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = wa_acdoca2-ps_psp_pnr
        IMPORTING
          output = ls_item-wbs_code.
      ls_item-paym_dtl = wa_acdoca2-sgtxt.
      ls_item-curr = ls_header-curr.
*      IF ls_item-curr = 'IDR'.
*      ls_item-amount = wa_acdoca2-wsl ."* 100.


      IF wa_acdoca2-wsl <= -1.
        WRITE wa_acdoca2-wsl TO ls_item-amount CURRENCY ls_header-curr.
        SHIFT ls_item-amount RIGHT DELETING TRAILING '-'.
        CONDENSE ls_item-amount.
        CONCATENATE '-' ls_item-amount INTO ls_item-amount.
      ELSE.
        WRITE wa_acdoca2-wsl TO ls_item-amount CURRENCY ls_header-curr.
      ENDIF.

*      ENDIF.
      APPEND ls_item TO lt_item_tmp.
      CLEAR ls_item.
    ENDLOOP.
    CLEAR wa_acdoca2.
  ENDIF.

  LOOP AT lt_item_tmp INTO ls_item_tmp.
    ls_item-no = sy-tabix.
    ls_item-acc_code = ls_item_tmp-acc_code.
    ls_item-acc_desc = ls_item_tmp-acc_desc.
    ls_item-cost_ctr = ls_item_tmp-cost_ctr.
    ls_item-wbs_code = ls_item_tmp-wbs_code.
    ls_item-paym_dtl = ls_item_tmp-paym_dtl.
    ls_item-curr = ls_item_tmp-curr.
    ls_item-amount = ls_item_tmp-amount.
    APPEND ls_item TO lt_item.
    CLEAR ls_item.
  ENDLOOP.
  CLEAR ls_item_tmp.

ENDFORM.

FORM f_print_get_footer.
  DATA :

    lt_person      TYPE TABLE OF bapip0002b WITH HEADER LINE,
    v_crtdby_appnr TYPE ZFIT_BDE_001-appnr,
    v_crtdby_crdat TYPE char10, "zta_fi_002_hist-crdat,
    v_crtdby_name  TYPE nachn,
    v_aprvby_appnr TYPE ZFIT_BDE_001-appnr,
    v_aprvby_crdat TYPE char10, ",zta_fi_002_hist-crdat,
    v_aprvby_name  TYPE nachn,
    v_pstdby_appnr TYPE ZFIT_BDE_001-appnr,
    v_pstdby_crdat TYPE char10, "zta_fi_002_hist-crdat,
    v_pstdby_name  TYPE nachn,
    v_postby       TYPE char50,
    v_apprby       TYPE char50,
    v_crtdby       TYPE char50,
    v_user         TYPE bapiemplb-userid
    .



  CLEAR :
  v_crtdby_appnr, v_crtdby_crdat, v_crtdby_name,
  v_aprvby_appnr, v_aprvby_crdat, v_aprvby_name,
  v_pstdby_appnr, v_pstdby_crdat, v_pstdby_name
  .

  CASE ls_header-blart.
    WHEN 'KR'.
      "created by
      SORT lt_crtdby DESCENDING BY rqsnr.
      DELETE ADJACENT DUPLICATES FROM lt_crtdby COMPARING belnr rqsnr.
      LOOP AT lt_crtdby INTO ls_crtdby WHERE belnr = ls_header-doc_no.
        v_crtdby_appnr = ls_crtdby-rqsnr.
        "v_crtdby_crdat = ls_crtdby-crdat.
        CONCATENATE ls_crtdby-crdat+6(2) ls_crtdby-crdat+4(2) ls_crtdby-crdat+0(4) INTO v_crtdby_crdat SEPARATED BY '.'.
        CALL FUNCTION 'BAPI_EMPLOYEE_GETDATA'
          EXPORTING
            employee_id   = v_crtdby_appnr
          TABLES
            personal_data = lt_person.

        v_crtdby_name = lt_person-last_name.

        IF v_crtdby_appnr IS INITIAL.
          ls_footer-crtd_by = '-'.
        ELSE.
          CONCATENATE v_crtdby_appnr v_crtdby_crdat v_crtdby_name INTO v_crtdby SEPARATED BY space.
          CONCATENATE v_crtdby ls_footer-crtd_by INTO ls_footer-crtd_by SEPARATED BY '                         '.
        ENDIF.

      ENDLOOP.
      CLEAR ls_crtdby.

      "approved by
      SORT lt_aprvby DESCENDING BY appnr.
      DELETE ADJACENT DUPLICATES FROM lt_aprvby COMPARING belnr appnr.
      LOOP AT lt_aprvby INTO ls_aprvby WHERE belnr = ls_header-doc_no.
        v_aprvby_appnr = ls_aprvby-appnr.
        "v_aprvby_crdat = ls_aprvby-crdat.
        CONCATENATE ls_aprvby-crdat+6(2) ls_aprvby-crdat+4(2) ls_aprvby-crdat+0(4) INTO v_aprvby_crdat SEPARATED BY '.'.

        CALL FUNCTION 'BAPI_EMPLOYEE_GETDATA'
          EXPORTING
            employee_id   = v_aprvby_appnr
          TABLES
            personal_data = lt_person.

        v_aprvby_name = lt_person-last_name.

        IF v_aprvby_appnr IS INITIAL.
          ls_footer-appr_by = '-'.
        ELSE.
          CONCATENATE v_aprvby_appnr v_aprvby_crdat v_aprvby_name INTO v_apprby SEPARATED BY space.
          CONCATENATE v_apprby ls_footer-appr_by INTO ls_footer-appr_by SEPARATED BY '                         '.
        ENDIF.

      ENDLOOP.
      CLEAR ls_aprvby.

      "posted by
*      SORT lt_pstdby DESCENDING BY appnr.
*      DELETE ADJACENT DUPLICATES FROM lt_pstdby COMPARING belnr appnr.
*      LOOP AT lt_pstdby INTO ls_pstdby WHERE belnr = ls_header-doc_no.
*        v_pstdby_appnr = ls_pstdby-appnr.
*        "v_pstdby_crdat = ls_pstdby-crdat.
*        CONCATENATE ls_pstdby-crdat+6(2) ls_pstdby-crdat+4(2) ls_pstdby-crdat+0(4) INTO v_pstdby_crdat SEPARATED BY '.'.
*
*        CALL FUNCTION 'BAPI_EMPLOYEE_GETDATA'
*          EXPORTING
*            employee_id   = v_pstdby_appnr
*          TABLES
*            personal_data = lt_person.
*
*        v_pstdby_name = lt_person-last_name.
*
*        IF v_pstdby_appnr IS INITIAL.
*          ls_footer-post_by = '-'.
*        ELSE.
*          CONCATENATE v_pstdby_appnr v_pstdby_crdat v_pstdby_name INTO v_postby  SEPARATED BY space.
*          CONCATENATE v_postby ls_footer-post_by INTO ls_footer-post_by SEPARATED BY '                         '.
*
*        ENDIF.
*      ENDLOOP.
*      CLEAR ls_pstdby..

*      DELETE ADJACENT DUPLICATES FROM it_bkpf COMPARING usnam.
      LOOP AT it_bkpf INTO wa_bkpf WHERE belnr = ls_header-doc_no AND bstat NE 'V'.
        v_user = wa_bkpf-usnam.
        CONCATENATE wa_bkpf-budat+6(2) wa_bkpf-budat+4(2) wa_bkpf-budat+0(4) INTO v_pstdby_crdat SEPARATED BY '.'.

        CALL FUNCTION 'BAPI_EMPLOYEE_GETDATA'
          EXPORTING
            userid        = v_user
          TABLES
            personal_data = lt_person.

        v_pstdby_appnr = lt_person-perno.
        v_pstdby_name = lt_person-last_name.

        IF v_pstdby_appnr IS INITIAL.
          ls_footer-post_by = '-'.
        ELSE.
          CONCATENATE v_pstdby_appnr v_pstdby_crdat v_pstdby_name INTO v_postby  SEPARATED BY space.
          CONCATENATE v_postby ls_footer-post_by INTO ls_footer-post_by SEPARATED BY '                         '.
        ENDIF.
      ENDLOOP.


    WHEN 'RE'.
*      DELETE ADJACENT DUPLICATES FROM it_bkpf COMPARING usnam.
      LOOP AT it_bkpf INTO wa_bkpf WHERE belnr = ls_header-doc_no AND bstat NE 'V'.
        v_user = wa_bkpf-usnam.
        CONCATENATE wa_bkpf-budat+6(2) wa_bkpf-budat+4(2) wa_bkpf-budat+0(4) INTO v_pstdby_crdat SEPARATED BY '.'.

        CALL FUNCTION 'BAPI_EMPLOYEE_GETDATA'
          EXPORTING
            userid        = v_user
          TABLES
            personal_data = lt_person.

        v_pstdby_appnr = lt_person-perno.
        v_pstdby_name = lt_person-last_name.

        IF v_pstdby_appnr IS INITIAL.
          ls_footer-post_by = '-'.
        ELSE.
          CONCATENATE v_pstdby_appnr v_pstdby_crdat v_pstdby_name INTO v_postby  SEPARATED BY space.
          CONCATENATE v_postby ls_footer-post_by INTO ls_footer-post_by SEPARATED BY '                         '.
        ENDIF.
      ENDLOOP.

  ENDCASE.
ENDFORM.

FORM f_print_run .


  CALL FUNCTION fu_name
    EXPORTING
      user_settings      = space
      control_parameters = ls_control_parameters
      output_options     = ls_output_options
      ls_header          = ls_header
      ls_footer          = ls_footer
    IMPORTING
      job_output_options = ls_job_output_options
    TABLES
*     lt_header          = lt_header
      lt_item            = lt_item
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.

FORM f_call_print .

  DATA : ls_control_parameters TYPE  ssfctrlop,
         ls_output_options     TYPE  ssfcompop,
         ls_job_output_options TYPE  ssfcresop.

  DATA: v_formname TYPE tdsfname,
        v_funcname TYPE rs38l_fnam.
*        lt_details TYPE ty_t_details,
*        ls_details LIKE LINE OF lt_details.

  CLEAR : v_formname, v_funcname.
  v_formname =  'ZFIF003'.

  "Print Parameter
  CLEAR : ls_control_parameters, ls_output_options.






  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = fr_name
    IMPORTING
      fm_name            = fu_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_tick
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
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
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_toggle_select
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ABAP_TRUE
*&---------------------------------------------------------------------*
FORM f_toggle_select  USING    pv_select TYPE c.
  DATA ls_display TYPE ty_rpt.
  ls_display-rowch = pv_select.

  MODIFY it_rpt FROM ls_display TRANSPORTING rowch WHERE belnr IS NOT INITIAL .
ENDFORM.
