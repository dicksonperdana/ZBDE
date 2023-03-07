*&---------------------------------------------------------------------*
*& Report ZPP_E001_DMP
*&---------------------------------------------------------------------*
* Created by minahkim
* Functional Nesya Irianis
*&
*&---------------------------------------------------------------------*
REPORT ZPPC_DMP_001.

INCLUDE ZPPC_DMP_001_TOP.
INCLUDE ZPPC_DMP_001_F01.

START-OF-SELECTION.
  IF P_RAD1 IS NOT INITIAL.
    PERFORM F_GET_RAD1..
    IF IT_TMP[] IS INITIAL.
      PERFORM F_SAVE_RAD1.
    ELSE.
      PERFORM F_UPDATE_RAD1.
    ENDIF.
  ELSEIF P_RAD2 IS NOT INITIAL.
    PERFORM F_GET_RAD2.
    PERFORM F_DISPLAY_RAD2.
  ENDIF.

END-OF-SELECTION.

*********************************************************************
