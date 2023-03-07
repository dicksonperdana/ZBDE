*&---------------------------------------------------------------------*
*& Include          ZABPXIN_ALV_LVC
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.
TYPE-POOLS: icon.

CONSTANTS:
 c_formname_top_of_page TYPE slis_formname VALUE 'F_TOP_OF_PAGE'.

DATA : t_fieldcat         TYPE lvc_t_fcat,
       w_sort             TYPE LINE OF lvc_t_sort,
       t_sort             TYPE lvc_t_sort,
       t_events           TYPE slis_t_event,
       t_list_top_of_page TYPE slis_t_listheader,
       d_layout           TYPE lvc_s_layo,
*       d_f2code           LIKE sy-ucomm, "VALUE  '&ETA',
       d_repid            LIKE sy-repid,
       d_variant          LIKE disvariant,
       d_print            TYPE lvc_s_prnt.

DATA : d_alv_user_command TYPE slis_formname,
       d_alv_pfstatus_set TYPE slis_formname.

DATA : go_container TYPE REF TO cl_gui_custom_container,
       go_grid      TYPE REF TO cl_gui_alv_grid.


*&---------------------------------------------------------------------*
*&      Form  F_FIELDCATS
*&---------------------------------------------------------------------*
FORM f_fieldcats USING fu_fname
                       fu_reftb
                       fu_reffname
                       fu_text
                       fu_len
                       fu_sum
                       fu_curr
                       fu_spot
                       fu_out
                       fu_uom
                       fu_edit
                       fu_just
                       fu_check
                       fu_decimals
                       fu_icon.

  DATA : lt_fieldcat TYPE lvc_s_fcat.
  lt_fieldcat-fieldname      = fu_fname.
  lt_fieldcat-ref_table      = fu_reftb.
  lt_fieldcat-ref_field      = fu_reffname.
  lt_fieldcat-scrtext_l      = fu_text.
  lt_fieldcat-scrtext_m      = fu_text.
  lt_fieldcat-scrtext_s      = fu_text.
  lt_fieldcat-reptext        = fu_text.
  lt_fieldcat-intlen         = fu_len.
  lt_fieldcat-do_sum         = fu_sum.
  lt_fieldcat-cfieldname     = fu_curr.
  lt_fieldcat-hotspot        = fu_spot.
  lt_fieldcat-no_out         = fu_out.
  lt_fieldcat-qfieldname     = fu_uom.
  lt_fieldcat-edit           = fu_edit.
  lt_fieldcat-just           = fu_just.
  lt_fieldcat-checkbox       = fu_check.
  lt_fieldcat-decimals_o     = fu_decimals.
  lt_fieldcat-decimals       = fu_decimals.
  lt_fieldcat-icon           = fu_icon.
*  lt_fieldcat-datatype       = fu_datatype.

  APPEND lt_fieldcat TO t_fieldcat.
  CLEAR: lt_fieldcat.
ENDFORM. " F_FIELDCATS


*&---------------------------------------------------------------------*
*&      Form  F_BUILD_SORTFIELD
*&---------------------------------------------------------------------*
FORM f_build_sortfield USING fu_sort TYPE lvc_t_sort
                              fu_field
                              fu_up
                              fu_group
                              fu_total.

  DATA: ld_sort TYPE lvc_s_sort.

  CLEAR ld_sort.
  ld_sort-fieldname = fu_field.
  ld_sort-up        = fu_up.
  ld_sort-subtot    = fu_total.
  ld_sort-group     = fu_group.
  APPEND ld_sort TO fu_sort.
ENDFORM. " F_BUILD_SORTFIELD

*&---------------------------------------------------------------------*
*&      Form  F_EVENTTAB_BUILD
*&---------------------------------------------------------------------*
FORM f_eventtab_build USING fu_events TYPE slis_t_event.
  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = fu_events.
  READ TABLE fu_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.

  IF sy-subrc = 0.
    MOVE c_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO fu_events.
  ENDIF.
ENDFORM. " F_EVENTTAB_BUILD

*&---------------------------------------------------------------------*
*&      Form  F_COMMENT_BUILD_MAIN_HEADER
*&---------------------------------------------------------------------*
FORM f_comment_build_main_header USING fu_top_of_page TYPE slis_t_listheader
                                        fu_header.
  DATA: ls_line TYPE slis_listheader.

  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  not used for this type
  ls_line-info = fu_header.
  APPEND ls_line TO fu_top_of_page.

ENDFORM. " F_COMMENT_BUILD_MAIN_HEADER


*&---------------------------------------------------------------------*
*&      Form  F_COMMENT_BUILD_HEADER
*&---------------------------------------------------------------------*
FORM f_comment_build_header USING fu_top_of_page TYPE slis_t_listheader
                                   fu_key
                                   fu_info.

  DATA: ls_line TYPE slis_listheader.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = fu_key.
  ls_line-info = fu_info.
  APPEND ls_line TO fu_top_of_page.

ENDFORM. " F_COMMENT_BUILD_HEADER


*&---------------------------------------------------------------------*
*&      Form  F_BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM f_build_layout USING fu_layout TYPE lvc_s_layo
                          fu_cekboxfield
                          fu_stylefname.

  fu_layout-zebra             = ''.

  IF NOT fu_cekboxfield IS INITIAL.
    fu_layout-box_fname     = fu_cekboxfield.
  ENDIF.

  IF NOT fu_stylefname IS INITIAL.
    fu_layout-stylefname       = fu_stylefname.
  ENDIF.

  fu_layout-cwidth_opt = 'X'.

ENDFORM. " F_BUILD_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  F_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM f_top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*     i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = t_list_top_of_page.

ENDFORM. " F_TOP_OF_PAGE


*&---------------------------------------------------------------------*
*&      Form  F_BUILD_PRINT
*&---------------------------------------------------------------------*
FORM f_build_print USING p_print TYPE lvc_s_prnt.
  d_print-prntselinf = 'X'.
ENDFORM. " F_BUILD_PRINT

*&---------------------------------------------------------------------*
*&      Form  f_alv_display
*&---------------------------------------------------------------------*
FORM f_alv_display TABLES ft_data.
  DATA ld_repid  LIKE sy-repid.

  d_repid = sy-repid.

  d_layout-cwidth_opt = 'X'.
**  ps_layout-confirmation_prompt = 'X'.
  d_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = d_repid
      i_callback_pf_status_set = d_alv_pfstatus_set
      i_callback_user_command  = d_alv_user_command
      is_layout_lvc            = d_layout
      it_fieldcat_lvc          = t_fieldcat[]
      it_sort_lvc              = t_sort[]
      i_default                = 'X'
      i_save                   = 'A'
      is_variant               = d_variant
      it_events                = t_events
      is_print_lvc             = d_print
    TABLES
      t_outtab                 = ft_data[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM. "f_alv_display



*&---------------------------------------------------------------------*
*&      Form  f_alvc_display
*&---------------------------------------------------------------------*
FORM f_alvc_display
      TABLES ft_data
      USING fu_containername TYPE c.
  DATA ld_repid  LIKE sy-repid.

  d_repid = sy-repid.

  d_layout-cwidth_opt = 'X'.
  d_layout-zebra      = 'X'.
  d_layout-no_toolbar = 'X'.

  IF go_container IS BOUND.
    go_grid->free( ).
    go_container->free( ).
  ENDIF.

  CREATE OBJECT go_container
    EXPORTING
      container_name = fu_containername.

  CREATE OBJECT go_grid
    EXPORTING
      i_parent = go_container.

  CALL METHOD go_grid->set_table_for_first_display
    EXPORTING
      i_save          = 'A'
      is_variant      = d_variant
      is_layout       = d_layout
    CHANGING
      it_outtab       = ft_data[]
      it_fieldcatalog = t_fieldcat[]
      it_sort         = t_sort[].

ENDFORM. "f_alvc_display
