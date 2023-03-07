**&---------------------------------------------------------------------*
**& Include          ZPSI001_TOP
**&---------------------------------------------------------------------*
**-----------------------------------------------------------------------
** TYPE POOLS : {global types declaration}
**-----------------------------------------------------------------------
** TABLES: {table declaration}
*TABLES : t001w, icon, acdoca.
TABLES: bkpf.
*-----------------------------------------------------------------------
* CONSTANTS : C_ {constants declaration}
*-----------------------------------------------------------------------
* TYPES : TYPE_ {types declaration}
TYPES: BEGIN OF ty_display,
         rowch   TYPE c,
         belnr   TYPE bkpf-belnr,
         bukrs   TYPE bkpf-bukrs,
         gjahr   TYPE bkpf-gjahr,
         blart   TYPE bkpf-blart,
         bldat   TYPE bkpf-bldat,
         budat   TYPE bkpf-budat,
         waers   TYPE bkpf-waers,
         wsl     TYPE acdoca-wsl,
         sgtxt   TYPE acdoca-sgtxt,
         lifnr   TYPE acdoca-lifnr,
         name1   TYPE lfa1-name1,
         hbkid   TYPE acdoca-hbkid,
         hktid   TYPE acdoca-hktid,
         banka   TYPE bnka-banka,
         bankn   TYPE t012k-bankn,
         payment TYPE string,
         usnam   TYPE bkpf-usnam,
         cpudt   TYPE bkpf-cpudt,
       END OF ty_display.

TYPES: BEGIN OF ty_bkpf,
         belnr TYPE bkpf-belnr,
         bukrs TYPE bkpf-bukrs,
         gjahr TYPE bkpf-gjahr,
         blart TYPE bkpf-blart,
         bldat TYPE bkpf-bldat,
         budat TYPE bkpf-budat,
         waers TYPE bkpf-waers,
         usnam TYPE bkpf-usnam,
         cpudt TYPE bkpf-cpudt,
         bktxt TYPE bkpf-bktxt,
       END OF ty_bkpf.

TYPES: BEGIN OF ty_bsak,
         bukrs TYPE bsak_view-bukrs,
         lifnr TYPE bsak_view-lifnr,
         umsks TYPE bsak_view-umsks,
         umskz TYPE bsak_view-umskz,
         augdt TYPE bsak_view-augdt,
         augbl TYPE bsak_view-augbl,
         zuonr TYPE bsak_view-zuonr,
         gjahr TYPE bsak_view-gjahr,
         belnr TYPE bsak_view-belnr,
         buzei TYPE bsak_view-buzei,
         blart TYPE bsak_view-blart,
         sgtxt TYPE bsak_view-sgtxt,
         wrbtr TYPE bsak_view-wrbtr,
         wmwst TYPE bsak_view-wmwst,
         waers TYPE bsak_view-waers,
         shkzg TYPE bsak_view-shkzg,
       END OF ty_bsak.

TYPES: BEGIN OF ty_acdoca,
         rldnr          TYPE acdoca-rldnr,
         rbukrs         TYPE acdoca-rbukrs,
         gjahr          TYPE acdoca-gjahr,
         belnr          TYPE acdoca-belnr,
         docln          TYPE acdoca-docln,
         wsl            TYPE acdoca-wsl,
         sgtxt          TYPE acdoca-sgtxt,
         lifnr          TYPE acdoca-lifnr,
         koart          TYPE acdoca-koart,
         glaccount_type TYPE acdoca-glaccount_type,
         hbkid          TYPE acdoca-hbkid,
         hktid          TYPE acdoca-hktid,
       END OF ty_acdoca.

TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
       END OF ty_lfa1.

TYPES: BEGIN OF ty_t012,
         bukrs TYPE t012-bukrs,
         hbkid TYPE t012-hbkid,
         banks TYPE banks,
         bankl TYPE t012-bankl,
       END OF ty_t012.

TYPES: BEGIN OF ty_t012k,
         bukrs TYPE t012K-bukrs,
         hbkid TYPE t012K-hbkid,
         hKTID TYPE t012K-hktid,
         bankn TYPE t012k-bankn,
       END OF ty_t012k.

TYPES: BEGIN OF ty_bnka,
         banks TYPE bnka-banks,
         bankl TYPE bnka-bankl,
         banka TYPE bnka-banka,
       END OF ty_bnka.


TYPES: BEGIN OF ty_zta_fi_regup,
         laufd TYPE zta_fi_regup-laufd,
         laufi TYPE zta_fi_regup-laufi,
         ident TYPE zta_fi_regup-ident,
         xvorl TYPE zta_fi_regup-xvorl,
         zbukr TYPE zta_fi_regup-zbukr,
         lifnr TYPE zta_fi_regup-lifnr,
         kunnr TYPE zta_fi_regup-kunnr,
         empfg TYPE zta_fi_regup-empfg,
         vblnr TYPE zta_fi_regup-vblnr,
         bukrs TYPE zta_fi_regup-bukrs,
         belnr TYPE zta_fi_regup-belnr,
         gjahr TYPE zta_fi_regup-gjahr,
         buzei TYPE zta_fi_regup-buzei,
       END OF ty_zta_fi_regup.

TYPES: BEGIN OF ty_zta_fi_022,
         laufd TYPE zta_fi_022-laufd,
         laufi TYPE zta_fi_022-laufi,
         ident TYPE zta_fi_022-ident,
         bukrs TYPE zta_fi_022-bukrs,
         schma TYPE zta_fi_022-schma,
         lvl   TYPE zta_fi_022-lvl,
         plans TYPE zta_fi_022-plans,
         crtby TYPE zta_fi_022-crtby,
         crton TYPE zta_fi_022-crton,
       END OF ty_zta_fi_022.

TYPES: BEGIN OF ty_pa0105,
         pernr TYPE pa0105-pernr,
         subty TYPE pa0105-subty,
         objps TYPE pa0105-objps,
         sprps TYPE pa0105-sprps,
         endda TYPE pa0105-endda,
         begda TYPE pa0105-begda,
         seqnr TYPE pa0105-seqnr,
         usrty TYPE pa0105-usrty,
         usrid TYPE pa0105-usrid,
       END OF ty_pa0105.

TYPES : ty_r_belnr TYPE RANGE OF bkpf-belnr,
        ty_r_budat TYPE RANGE OF bkpf-budat,
        ty_r_bukrs TYPE RANGE OF bkpf-buKRS,
        ty_r_gjahr TYPE RANGE OF bkpf-gjahr.

TYPES : ty_t_display      TYPE STANDARD TABLE OF ty_display,
        ty_t_bkpf         TYPE STANDARD TABLE OF ty_bkpf,
        ty_t_bsak         TYPE STANDARD TABLE OF ty_bsak,
        ty_t_acdoca       TYPE STANDARD TABLE OF ty_acdoca,
        ty_t_lfa1         TYPE STANDARD TABLE OF ty_lfa1,
        ty_t_t012         TYPE STANDARD TABLE OF ty_t012,
        ty_t_t012k        TYPE STANDARD TABLE OF ty_t012k,
        ty_t_bnka         TYPE STANDARD TABLE OF ty_bnka,
        ty_t_header       TYPE STANDARD TABLE OF ZFI_FST_BDE_001,
        ty_t_details      TYPE STANDARD TABLE OF ZFI_FST_BDE_001A,
        ty_t_zta_fi_regup TYPE STANDARD TABLE OF ty_zta_fi_regup,
        ty_t_zta_fi_022   TYPE STANDARD TABLE OF zta_fi_022,
        ty_t_pa0105       TYPE STANDARD TABLE OF pa0105.

*-----------------------------------------------------------------------
* DATA: D_ {variable declaration}
DATA : gt_bkpf         TYPE ty_t_bkpf,
       gt_bsak         TYPE ty_t_bsak,
       gt_acdoca       TYPE ty_t_acdoca,
       gt_lfa1         TYPE ty_t_lfa1,
       gt_t012         TYPE ty_t_t012,
       gt_t012k        TYPE ty_t_t012k,
       gt_bnka         TYPE ty_t_bnka,
       gt_display      TYPE ty_t_display,
       gt_header       TYPE ty_t_header,
       gt_details      TYPE ty_t_details,
       gt_zta_fi_regup TYPE ty_t_zta_fi_regup,
       gt_zta_fi_022   TYPE ty_t_zta_fi_022,
       gt_pa0105       TYPE ty_t_pa0105.


DATA : ok_code1 TYPE sy-ucomm.

DATA : gv_message TYPE bapiret2.
*-----------------------------------------------------------------------
* DATA: BEGIN OF T_ {structure/itab declaration}
* DATA: END OF
*-----------------------------------------------------------------------
* MAC_  MACRO declaration


*-----------------------------------------------------------------------
* Ranges : R_   {RANGES Declaration}

*-----------------------------------------------------------------------
* Controls : TC_   {CONTROLS Declaration}

*-----------------------------------------------------------------------
* Selection-screens
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
* menu bar button
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
* select-options / parameter
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blok1 WITH FRAME TITLE TEXT-a01.
  SELECT-OPTIONS: s_bukrs FOR bkpf-bukrs OBLIGATORY NO-EXTENSION NO INTERVALS MODIF ID aa1 DEFAULT 'PMMI',
                  s_belnr FOR bkpf-belnr MODIF ID aa1,
                  s_gjahr FOR bkpf-gjahr OBLIGATORY NO-EXTENSION NO INTERVALS MODIF ID aa1 DEFAULT '2022',
                  s_budat FOR bkpf-budat MODIF ID aa1.
SELECTION-SCREEN END OF BLOCK blok1.
