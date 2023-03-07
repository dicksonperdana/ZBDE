*&---------------------------------------------------------------------*
*& Report ZFIF_BDE_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFIF_BDE_002.


INCLUDE zabpxin_alv_lvc.
INCLUDE ZFIF_BDE_002_top.
*INCLUDE zfir003_alv.

INCLUDE ZFIF_BDE_002_f01.
INCLUDE ZFIF_BDE_002_f01b.


START-OF-SELECTION.
  PERFORM f_get_data.
  PERFORM f_collect_data.
 END-OF-SELECTION.

PERFORM f_write_data.

*-----------------------------------------------------------------------
* Events on selection screens
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
* Events in lists
*-----------------------------------------------------------------------
AT LINE-SELECTION.
AT USER-COMMAND.
