*&---------------------------------------------------------------------*
*& Include          ZFIR003_TOP
*&---------------------------------------------------------------------*

TABLES : bkpf.
TYPE-POOLS:slis.
INCLUDE: <icon>, rmcs0f0m.

CONSTANTS:
  yes        VALUE 'X',
  no         VALUE space,
  c_vrsio(3) VALUE '000'.

*Common Mandatory and Non Mandatory ALV Variables
DATA:
  i_layout     TYPE slis_layout_alv,
*   i_layout type lvc_s_layo,
  i_events     TYPE slis_t_event,
  i_fieldcat   TYPE slis_t_fieldcat_alv,
  i_print      TYPE slis_print_alv,
  i_list_top   TYPE slis_t_listheader,
  i_list_end   TYPE slis_t_listheader,
  i_sort       TYPE slis_t_sortinfo_alv,
  i_color      TYPE lvc_t_scol WITH HEADER LINE,
  wa_fieldcat  LIKE LINE OF i_fieldcat,
  i_variant    LIKE disvariant,
  i_selfield   TYPE slis_selfield,
  i_repid      TYPE sy-repid,
  fcode        TYPE TABLE OF sy-ucomm,
  ok_code      TYPE sy-ucomm,
  data_count   TYPE i,
  header_count TYPE i,
  v_total      TYPE sy-pagno.

*DATA : d_alv_user_command TYPE slis_formname,
*       d_alv_pfstatus_set TYPE slis_formname.

DATA:
  gs_extract1 LIKE disextract,
  gs_extract2 LIKE disextract.

DATA: ctrl_name TYPE ssfctrlop.

DATA:
  fr_name  TYPE tdsfname VALUE 'ZFIF003',
  fu_name  TYPE rs38l_fnam,
  fu_name2 TYPE rs38l_fnam.
*DATA:
*  s_options         TYPE ssfcompop,
*  s_control         TYPE ssfctrlop,
*  s_job_output_info TYPE  ssfcrescl,
*  ssfcompop         TYPE ssfcompop,
*  ssfctrlop         TYPE ssfctrlop,
*  wa_tabtdr         TYPE sfc_itab.
*    v_device          TYPE  rspopname.


DATA : ls_control_parameters TYPE  ssfctrlop,
       ls_output_options     TYPE  ssfcompop,
       ls_job_output_options TYPE  ssfcresop.


TYPES :
  BEGIN OF ty_bkpf,
    belnr TYPE bkpf-belnr,
    bukrs TYPE bkpf-bukrs,
    gjahr TYPE bkpf-gjahr,
    blart TYPE bkpf-blart,
    bldat TYPE bkpf-bldat,
    budat TYPE bkpf-budat,
    bstat TYPE bkpf-bstat,
    waers TYPE bkpf-waers,
    ppnam TYPE bkpf-ppnam,
    usnam TYPE bkpf-usnam,
    xblnr TYPE bkpf-xblnr,
    bktxt type bkpf-bktxt,
*    usnam TYPE bkpf-usnam,
  END OF ty_bkpf,

  BEGIN OF ty_vbsegk,
    belnr TYPE vbsegk-belnr,
    bukrs TYPE vbsegk-bukrs,
    gjahr TYPE vbsegk-gjahr,
    wrbtr TYPE vbsegk-wrbtr,
    lifnr TYPE vbsegk-lifnr,
    sgtxt TYPE vbsegk-sgtxt,
  END OF ty_vbsegk,

  BEGIN OF ty_acdoca,
    belnr  TYPE acdoca-belnr,
    buzei  type acdoca-buzei,
    rbukrs TYPE acdoca-pbukrs,
    gjahr  TYPE acdoca-gjahr,
    wsl    TYPE acdoca-wsl,
    lifnr  TYPE acdoca-lifnr,
    sgtxt  TYPE acdoca-sgtxt,
*    buzei  type acdoca-buzei,
    ebeln  type acdoca-ebeln,
  END OF ty_acdoca,

  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
  END OF ty_lfa1,

  BEGIN OF ty_rpt,
    rowch TYPE c,
    belnr TYPE bkpf-belnr,
    bukrs TYPE bkpf-bukrs,
    gjahr TYPE bkpf-gjahr,
    blart TYPE bkpf-blart,
    bldat TYPE bkpf-bldat,
    budat TYPE bkpf-budat,
    bstat TYPE bkpf-bstat,
    zstat TYPE char20,
    waers TYPE bkpf-waers,
    zamnt TYPE p DECIMALS 2,
    zvndr TYPE lifnr,
    zname TYPE lfa1-name1,
    zdesc TYPE sgtxt,
    xblnr TYPE bkpf-xblnr,
    numb  TYPE sy-tabix,
    bktxt type bkpf-bktxt,
  END OF ty_rpt,

  BEGIN OF ty_vbsegs,
    belnr      TYPE vbsegs-belnr,
    bukrs      TYPE vbsegs-bukrs,
    gjahr      TYPE vbsegs-gjahr,
    saknr      TYPE vbsegs-saknr,
    kostl      TYPE vbsegs-kostl,
    ps_psp_pnr TYPE vbsegs-ps_psp_pnr,
    sgtxt      TYPE vbsegs-sgtxt,
    wrbtr      TYPE vbsegs-wrbtr,
    buzei      type vbsegs-buzei,
  END OF ty_vbsegs,

  BEGIN OF ty_acdoca2,
    belnr      TYPE acdoca-belnr,
    rbukrs     TYPE acdoca-rbukrs,
    gjahr      TYPE acdoca-gjahr,
    racct      TYPE acdoca-racct,
    rcntr      TYPE acdoca-rcntr,
    ps_psp_pnr TYPE acdoca-ps_psp_pnr,
    sgtxt      TYPE acdoca-sgtxt,
    wsl        TYPE acdoca-wsl,
    buzei      TYPE acdoca-buzei,
  END OF ty_acdoca2,

  BEGIN OF ty_app,
    bukrs TYPE ZFIT_BDE_001-bukrs,
    belnr TYPE ZFIT_BDE_001-belnr,
    gjahr TYPE ZFIT_BDE_001-gjahr,
    wf_id TYPE ZFIT_BDE_001-wf_id,
    statu TYPE ZFIT_BDE_001-statu,
    appnr TYPE ZFIT_BDE_001-appnr,
    crdat TYPE ZFIT_BDE_001-crdat,
    rqsnr TYPE ZFIT_BDE_001-rqsnr,
  END OF ty_app.


DATA :
  lt_crtdby TYPE TABLE OF ty_app,
  ls_crtdby TYPE ty_app,
  lt_aprvby TYPE TABLE OF ty_app,
  ls_aprvby TYPE ty_app,
  lt_pstdby TYPE TABLE OF ty_app,
  ls_pstdby TYPE ty_app
  .

DATA : it_rpt     TYPE TABLE OF ty_rpt,
       wa_rpt     TYPE ty_rpt,
       it_frm     TYPE TABLE OF ty_rpt,
       wa_frm     TYPE ty_rpt,
       it_bkpf    TYPE TABLE OF ty_bkpf,
       wa_bkpf    TYPE ty_bkpf,
       it_vbsegk  TYPE TABLE OF ty_vbsegk,
       wa_vbsegk  TYPE ty_vbsegk,
       it_acdoca  TYPE TABLE OF ty_acdoca,
       wa_acdoca  TYPE ty_acdoca,
       it_lfa1    TYPE TABLE OF ty_lfa1,
       wa_lfa1    TYPE ty_lfa1,
       it_acdoca2 TYPE TABLE OF ty_acdoca2,
       wa_acdoca2 TYPE ty_acdoca2,
       it_vbsegs  TYPE TABLE OF ty_vbsegs,
       wa_vbsegs  TYPE ty_vbsegs,
       it_hcdoca  type TABLE OF ty_acdoca,
       wa_hcdoca  type ty_acdoca,
       it_fbkpf    TYPE TABLE OF ty_bkpf,
       wa_fbkpf    TYPE ty_bkpf.

"print
DATA :
  lt_header   TYPE TABLE OF ZFIST_BDE_002,
  ls_header   TYPE ZFIST_BDE_002,
  ls_headd    TYPE ZFIST_BDE_002,
  lt_item     TYPE TABLE OF ZFIST_BDE_002d,
  lt_item_tmp TYPE TABLE OF ZFIST_BDE_002d,
  ls_item     TYPE ZFIST_BDE_002d,
  ls_item_tmp TYPE ZFIST_BDE_002d,
  ls_footer   TYPE ZFIST_BDE_002f,
  gs_footer   TYPE ZFIST_BDE_002f,
  lt_footer   TYPE TABLE OF ZFIST_BDE_002f.

SELECTION-SCREEN BEGIN OF BLOCK block01 WITH FRAME TITLE TEXT-001.
** Company Code
  PARAMETERS : p_bukrs TYPE bkpf-bukrs OBLIGATORY.
  SELECT-OPTIONS : " p_bukrs FOR bkpf-bukrs NO INTERVALS OBLIGATORY,
                   p_belnr FOR bkpf-belnr,
                   p_gjahr FOR bkpf-gjahr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK block01.

SELECTION-SCREEN BEGIN OF BLOCK block02 WITH FRAME TITLE TEXT-001.
** Company Code
  SELECT-OPTIONS : p_budat FOR bkpf-budat,
                   p_ppnam FOR bkpf-ppnam NO INTERVALS,
                   p_usnam FOR bkpf-usnam NO INTERVALS.
SELECTION-SCREEN END OF BLOCK block02.
