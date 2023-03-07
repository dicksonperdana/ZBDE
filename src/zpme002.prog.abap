*&---------------------------------------------------------------------*
*& Report ZPME002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpme002.

*&---------------------------------------------------------------------*
*& Report ZPM_E001
*&---------------------------------------------------------------------*
*& Program Upload Measuring Document
*& Functional    : DHA
*& Programmer    : DHA
*& Created Date  : 27.09.2018
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& SAP ABAP standard include
*&---------------------------------------------------------------------*
INCLUDE zpm_abdi01.
INCLUDE zpm_alv01.

*&---------------------------------------------------------------------*
*&  Global Data
*&---------------------------------------------------------------------*
TYPES : BEGIN OF ty_data,
          no                   TYPE i,
          equnr                TYPE equnr,
          tplnr                TYPE tplnr,
          measuring_position   TYPE imrc_psort,
          reading_date         TYPE char10,
          reading_time         TYPE char10,
          recorded_value       TYPE imrc_recdc,
          reader               TYPE imrc_readr,
          short_text           TYPE imrc_mdtxt,
          measurement_document TYPE imrc_mdocm,

          measuring_point      TYPE imrc_point,
          replace_counter      TYPE imrc_exchg,

          status               TYPE c LENGTH 1,
          lampu                TYPE c LENGTH 5,
          pesan                TYPE c LENGTH 100,
        END OF ty_data.

DATA  : BEGIN OF mc_data OCCURS 0,
          equnr(18),
          tplnr(40),
        END OF mc_data.

TYPES : BEGIN OF ty_equi,
          equnr TYPE equnr,
          objnr TYPE j_objnr,
        END OF ty_equi.

TYPES : BEGIN OF ty_iflo,
          tplnr TYPE tplnr,
          objnr TYPE j_objnr,
        END OF ty_iflo.

TYPES : BEGIN OF ty_imptt,
          psort TYPE imrc_psort,
          point TYPE imrc_point,
          mpobj TYPE imrc_mpobj,
        END OF ty_imptt.

DATA : lt_data      TYPE STANDARD TABLE OF ty_data,
       lt_proc      TYPE STANDARD TABLE OF ty_data,
       lt_equi      TYPE STANDARD TABLE OF ty_equi,
       lt_iflo      TYPE STANDARD TABLE OF ty_iflo,
       lt_imptt     TYPE STANDARD TABLE OF ty_imptt,
       lt_imptt2    TYPE STANDARD TABLE OF ty_imptt,
       lt_impttcopy TYPE STANDARD TABLE OF ty_imptt.
DATA : it_exclude TYPE slis_t_extab,
       wa_exclude TYPE slis_extab.
DATA : ls_data      TYPE ty_data,
       ls_proc      TYPE ty_data,
       ls_equi      TYPE ty_equi,
       ls_iflo      TYPE ty_iflo,
       ls_imptt     TYPE ty_imptt,
       ls_imptt2    TYPE ty_imptt,
       ls_impttcopy TYPE ty_imptt,

       v_jumlah     TYPE i.

DATA    : ls_hm_invts TYPE imrg-invts.
DATA    : ld_datlo TYPE imrc_idate,
          ld_timlo TYPE imrc_itime.

DATA    : ls_hm_equi      TYPE equi,
          ls_hm_last_imrg TYPE imrg,
          ls_hm_imrg      TYPE imrg.

DATA    : ls_hm_diff  TYPE rihimrg-pyeac,
          ls_hm_inact TYPE jest-inact,
          ls_hm_atnam TYPE cabn-atnam,
          ls_hm_imptt TYPE imptt.

DATA    : lastread    TYPE qsollwertc,

          lastread_n  TYPE p DECIMALS 3,

          hourmeter_n TYPE p DECIMALS 3,
          deltaread   TYPE imrc_recdc.

DATA    : currdate LIKE imrg-idate,
          lastdate LIKE imrg-idate,
          currtime LIKE imrg-itime,
          lasttime LIKE imrg-itime,
          deltactr LIKE ltak-istwm.

DATA  : hourmeter_m TYPE imrc_recdc.    " Hourmeter Value

DATA    : msg_hm    TYPE string.

*-----------------------------------------------------------------------
* DATA: D_ {variable declaration}
DATA : d_alv_type TYPE i.

DATA : lt_intern TYPE STANDARD TABLE OF alsmex_tabline WITH HEADER LINE.

FIELD-SYMBOLS: <fs_intern> LIKE LINE OF lt_intern,
               <fs_aux>.
DATA: v_index TYPE i.
*-----------------------------------------------------------------------
* Selection-screens
*-----------------------------------------------------------------------
* select-options / parameter
*-----------------------------------------------------------------------
SELECTION-SCREEN: BEGIN OF BLOCK block2 WITH FRAME TITLE TEXT-002.


  "File Input Source (Form)
  PARAMETERS : p_finp LIKE rlgrap-filename.
*PARAMETERS : p_finp LIKE IBIPPARMS-PATH.
*PARAMETERS : p_finp2  LIKE IBIPPARMS-PATH.
  PARAMETERS : rd_win RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND radiobutton,
               rd_mac RADIOBUTTON GROUP rad1.

SELECTION-SCREEN : END OF BLOCK block2.

*-----------------------------------------------------------------------
* Start-Of-Selection
*-----------------------------------------------------------------------
START-OF-SELECTION.


  PERFORM f_init_data.
  PERFORM f_get_data.
  PERFORM f_fill_data.

END-OF-SELECTION.

  IF NOT lt_data[] IS INITIAL.
    PERFORM f_write_data USING d_alv_type.
  ELSE.
    MESSAGE i001(00) WITH 'Data tidak ditemukan.'.
  ENDIF.

*-----------------------------------------------------------------------
* Events on selection screens
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_finp.
  PERFORM f_direct_ws_name_from_pc USING p_finp
                                         'D:\'
                                         'File Name'.

*-----------------------------------------------------------------------
* Events in lists
*-----------------------------------------------------------------------
AT LINE-SELECTION.

AT USER-COMMAND.
  AT pfn.

*&---------------------------------------------------------------------*
*&      Form  F_INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_init_data .
  d_alv_type = 1.
  d_alv_user_command = 'F_USER_COMMAND'.
  d_alv_pfstatus_set = 'F_ALV_STATUS'.

ENDFORM.                    " F_INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data .
  DATA : ld_subrc TYPE sy-subrc,
         ld_tabix TYPE sy-tabix.
  CLEAR : ld_subrc.
  IF rd_win EQ 'X'.
    PERFORM f_get_file_win CHANGING ld_subrc.
  ELSEIF rd_mac EQ 'X'.
    PERFORM f_get_file_mac CHANGING ld_subrc.
  ENDIF.

  IF ld_subrc EQ 0.

    DELETE lt_data WHERE equnr IS INITIAL AND tplnr IS INITIAL.

    CLEAR : ld_tabix.
    LOOP AT lt_data INTO ls_data.


      ld_tabix = sy-tabix.
      IF ls_data-equnr IS NOT INITIAL AND ls_data-tplnr IS INITIAL.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in  = ls_data-equnr
          IMPORTING
            string_out = ls_data-equnr.

      ELSEIF ls_data-equnr IS INITIAL AND ls_data-tplnr IS NOT INITIAL.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in  = ls_data-tplnr
          IMPORTING
            string_out = ls_data-tplnr.

      ENDIF.


      MODIFY lt_data FROM ls_data INDEX ld_tabix.
    ENDLOOP.



    IF NOT lt_data[] IS INITIAL.

      SELECT equnr    " Equipment Number
             objnr
        FROM equi
        INTO CORRESPONDING FIELDS OF TABLE lt_equi
        FOR ALL ENTRIES IN lt_data
        WHERE equnr EQ lt_data-equnr.

      SORT lt_equi BY equnr.

      IF NOT lt_equi[] IS INITIAL.
        SELECT point
               mpobj
               psort
          FROM imptt
          INTO CORRESPONDING FIELDS OF TABLE lt_imptt
          FOR ALL ENTRIES IN lt_equi
          WHERE mpobj EQ lt_equi-objnr AND
*                psort EQ 'HOURMETER' AND
                lvorm EQ ''.

        SORT lt_imptt BY mpobj psort.

      ENDIF.

      SELECT tplnr
             objnr
        FROM iflo
        INTO CORRESPONDING FIELDS OF TABLE lt_iflo
        FOR ALL ENTRIES IN lt_data
        WHERE tplnr EQ lt_data-tplnr.

      SORT lt_iflo BY tplnr.

      IF NOT lt_iflo[] IS INITIAL.
        SELECT point
               mpobj
               psort
          FROM imptt
          INTO CORRESPONDING FIELDS OF TABLE lt_imptt2
          FOR ALL ENTRIES IN lt_iflo
          WHERE mpobj EQ lt_iflo-objnr AND
*                psort EQ 'HOURMETER' AND
                lvorm EQ ''.

*        SORT lt_imptt BY mpobj psort.
        SORT lt_imptt2 BY mpobj psort. "Change by FRZ 13.07.2021
      ENDIF.


    ENDIF.

  ELSE.

    EXIT.

  ENDIF.

ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_GET_FILE_WIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_file_win CHANGING fu_subrc.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_finp
      i_begin_col             = '1'
      i_begin_row             = '2'
      i_end_col               = '256'
      i_end_row               = '65536'
    TABLES
      intern                  = lt_intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  fu_subrc = sy-subrc.
  IF sy-subrc EQ 0.

    SORT lt_intern BY row col.
    LOOP AT lt_intern ASSIGNING <fs_intern>.
      MOVE <fs_intern>-col TO v_index.
      ASSIGN COMPONENT v_index OF STRUCTURE ls_data TO <fs_aux>.
      MOVE <fs_intern>-value TO <fs_aux>.
      AT END OF row.
        APPEND ls_data TO lt_data.
        CLEAR ls_data.
      ENDAT.
    ENDLOOP.

  ELSE.

    WRITE: 'Error ', sy-subrc, 'returned from UPLOAD WINDOWS'.
    SKIP.

  ENDIF.

ENDFORM.                    " F_GET_FILE_WIN
*&---------------------------------------------------------------------*
*&      Form  F_GET_FILE_MAC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_file_mac CHANGING fu_subrc.
  DATA : file_txt TYPE string.

  file_txt = p_finp.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = file_txt
      filetype                = 'ASC'
      has_field_separator     = 'X'
*     HEADER_LENGTH           = 0
*     READ_BY_LINE            = 'X'
*     DAT_MODE                = ' '
*     CODEPAGE                = ' '
*     IGNORE_CERR             = 'X'
*     REPLACEMENT             = '#'
*     CHECK_BOM               = ' '
*     VIRUS_SCAN_PROFILE      =
*     NO_AUTH_CHECK           = ' '
    TABLES
      data_tab                = lt_data
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  fu_subrc = sy-subrc.
  IF sy-subrc NE 0.
    WRITE: 'Error ', sy-subrc, 'returned from UPLOAD MAC'.
    SKIP.
  ENDIF.
ENDFORM.                    " F_GET_FILE_MAC

*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_data .
  DATA : ld_tabix TYPE sy-tabix.

  LOOP AT lt_data INTO ls_data.
    ld_tabix = sy-tabix.
    ls_data-status = 'S'.
    WRITE icon_green_light TO ls_data-lampu.
    IF ls_data-equnr IS NOT INITIAL AND ls_data-tplnr IS INITIAL.
      READ TABLE lt_equi INTO ls_equi WITH KEY equnr = ls_data-equnr
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR : v_jumlah.
        REFRESH : lt_impttcopy.
        lt_impttcopy[] = lt_imptt[].
        DELETE lt_impttcopy WHERE mpobj NE ls_equi-objnr OR psort NE ls_data-measuring_position.
        DESCRIBE TABLE lt_impttcopy LINES v_jumlah.

        IF v_jumlah = 1.
          READ TABLE lt_imptt INTO ls_imptt WITH KEY mpobj = ls_equi-objnr
                                                     psort = ls_data-measuring_position
                                                     BINARY SEARCH.
          IF sy-subrc EQ 0.
            ls_data-measuring_point = ls_imptt-point.
            IF ls_data-measuring_position EQ 'HOURMETER'. " added by Rifki.F 14/01/2022
              PERFORM f_check_hourmeter.
              IF deltaread > deltactr.
                CONCATENATE 'HOURMETER melebihi jam dunia :' deltaread '&' 'HR.' INTO msg_hm.
                REPLACE '&' WITH space INTO msg_hm.
                ls_data-status = 'F'.
                WRITE icon_red_light TO ls_data-lampu.
                ls_data-pesan  = msg_hm.
*            ELSE.
*              ls_data-measuring_point = ls_imptt-point.
              ELSE.
                ls_data-replace_counter = 'X'.
                PERFORM f_check_hourmeter.
              ENDIF.
            ENDIF.


          ENDIF.
        ELSEIF v_jumlah > 1.
          ls_data-status = 'F'.
          WRITE icon_red_light TO ls_data-lampu.
          ls_data-pesan = 'Meas.Point aktif lebih dari satu.'.
        ELSEIF v_jumlah = 0.
          ls_data-status = 'F'.
          WRITE icon_red_light TO ls_data-lampu.
          ls_data-pesan = 'Meas.Point tidak ditemukan'.
        ENDIF.
      ENDIF.



*      IF ls_data-measuring_point IS INITIAL.
*        ls_data-status = 'F'.
*        WRITE icon_red_light TO ls_data-lampu.
*        ls_data-pesan = 'Meas.Point tidak ditemukan'.
*      ENDIF.

    ELSEIF ls_data-tplnr IS NOT INITIAL AND ls_data-equnr IS INITIAL.
      READ TABLE lt_iflo INTO ls_iflo WITH KEY tplnr = ls_data-tplnr
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.

        CLEAR : v_jumlah.
        REFRESH : lt_impttcopy.
        lt_impttcopy[] = lt_imptt2[].
        DELETE lt_impttcopy WHERE mpobj NE ls_iflo-objnr OR psort NE ls_data-measuring_position.
        DESCRIBE TABLE lt_impttcopy LINES v_jumlah.

        IF v_jumlah = 1.
          READ TABLE lt_imptt2 INTO ls_imptt2 WITH KEY mpobj = ls_iflo-objnr
                                                     psort = ls_data-measuring_position
                                                     BINARY SEARCH.
          IF sy-subrc EQ 0.
            ls_data-measuring_point = ls_imptt2-point.
          ENDIF.
        ELSEIF v_jumlah > 1.
          ls_data-status = 'F'.
          WRITE icon_red_light TO ls_data-lampu.
          ls_data-pesan = 'Meas.Point aktif lebih dari satu.'.

        ELSEIF v_jumlah = 0.
          ls_data-status = 'F'.
          WRITE icon_red_light TO ls_data-lampu.
          ls_data-pesan = 'Meas.Point tidak ditemukan'.
        ENDIF.

      ENDIF.

*      IF ls_data-measuring_point IS INITIAL.
*        ls_data-status = 'F'.
*        WRITE icon_red_light TO ls_data-lampu.
*        ls_data-pesan = 'Meas.Point tidak ditemukan'.
*      ENDIF.

    ELSEIF ls_data-equnr IS NOT INITIAL AND ls_data-tplnr IS NOT INITIAL.
      ls_data-status = 'F'.
      WRITE icon_red_light TO ls_data-lampu.
      ls_data-pesan = 'Object yang terisi lebih dari 1 (Equipment & Func. Loc.)'.

    ENDIF.




    MODIFY lt_data FROM ls_data INDEX ld_tabix.
    CLEAR : ls_data.
  ENDLOOP.
ENDFORM.                    " F_FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_D_ALV_TYPE  text
*----------------------------------------------------------------------*
FORM f_write_data  USING    fu_alv_type.
  CLEAR: t_fieldcat, d_print, d_layout.
  REFRESH: t_fieldcat.

  PERFORM f_passing_field USING fu_alv_type.
  PERFORM f_build_print  USING d_print.
  CASE fu_alv_type.
    WHEN 1.
      PERFORM f_build_layout USING d_layout ''.
      PERFORM f_alv_display  TABLES lt_data.
    WHEN 2.
      PERFORM f_build_layout USING d_layout ''.
      PERFORM f_alv_display  TABLES lt_proc.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " F_WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_PASSING_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FU_ALV_TYPE  text
*----------------------------------------------------------------------*
FORM f_passing_field  USING    fu_alv_type.
  CASE fu_alv_type.
    WHEN 1.
      PERFORM f_fieldcats1 USING :
* FieldName       RefTable       RefFieldName     Text
*   Len     sum(X/'')   CurrFieldname   spot(X/'')   Hide   UOM
*   Input Key   Align
        'STATUS'        ''              ''          'Status'
          '6'    ''          ''              ''           ''     ''
          ''    '',
        'LAMPU'        ''              ''          'Indctr'
          '6'    ''          ''              ''           ''     ''
          ''    '',
        'EQUNR'        'EQUI'          'EQUNR'          'Equipment'
          ''    ''          ''              ''           ''     ''
          ''    '',
        'TPLNR'        'IFLO'          'TPLNR'          'Functional Location'
          '30'    ''          ''              ''           ''     ''
          ''    '',
        'MEASURING_POSITION'        'IMPTT'          'PSORT'          'Position'
          '20'    ''          ''              ''           ''     ''
          ''    '',
        'MEASURING_POINT'        ''          ''          'Meas. Point'
          '12'    ''          ''              ''           ''     ''
          ''    '',
        'READING_DATE'        ''          ''          'Date'
          '10'    ''          ''              ''           ''     ''
          ''    '',
        'READING_TIME'        ''          ''          'Time'
          '8'    ''          ''              ''           ''     ''
          ''    '',
        'RECORDED_VALUE'        ''          ''          'Value'
          '22'    ''          ''              ''           ''     ''
          ''    '',
        'READER'        ''          ''          'Read By'
          '10'    ''          ''              ''           ''     ''
          ''    '',
        'SHORT_TEXT'        ''          ''          'Short Text'
          '40'    ''          ''              ''           ''     ''
          ''    '',
        'PESAN'        ''          ''          'Message'
          '100'    ''          ''              ''           ''     ''
          ''    ''.

    WHEN 2.
      PERFORM f_fieldcats1 USING :
* FieldName       RefTable       RefFieldName     Text
*   Len     sum(X/'')   CurrFieldname   spot(X/'')   Hide   UOM
*   Input Key  Align
        'STATUS'        ''              ''          'Status'
          '6'    ''          ''              ''           ''     ''
          ''    '',
        'LAMPU'        ''              ''          'Indctr'
          '6'    ''          ''              ''           ''     ''
          ''    '',
        'EQUNR'        'EQUI'          'EQUNR'          'Equipment'
          ''    ''          ''              ''           ''     ''
          ''    '',
        'TPLNR'        'IFLO'          'TPLNR'          'Functional Location'
          '30'    ''          ''              ''           ''     ''
          ''    '',
        'MEASURING_POSITION'        'IMPTT'          'PSORT'          'Position'
          '20'    ''          ''              ''           ''     ''
          ''    '',
        'MEASURING_POINT'        ''          ''          'Meas. Point'
          '12'    ''          ''              ''           ''     ''
          ''    '',
        'READING_DATE'        ''          ''          'Date'
          '10'    ''          ''              ''           ''     ''
          ''    '',
        'READING_TIME'        ''          ''          'Time'
          '8'    ''          ''              ''           ''     ''
          ''    '',
        'RECORDED_VALUE'        ''          ''          'Value'
          '22'    ''          ''              ''           ''     ''
          ''    '',
        'READER'        ''          ''          'Read By'
          '10'    ''          ''              ''           ''     ''
          ''    '',
        'SHORT_TEXT'        ''          ''          'Short Text'
          '40'    ''          ''              ''           ''     ''
          ''    '',
        'PESAN'        ''          ''          'Message'
          '100'    ''          ''              ''           ''     ''
          ''    ''.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " F_PASSING_FIELD

*&---------------------------------------------------------------------*
*&      Form  f_alv_pfstatus_set
*&---------------------------------------------------------------------*
FORM f_alv_status USING fu_extab TYPE slis_t_extab.         "#EC CALLED
  CASE d_alv_type.
    WHEN 2.
      APPEND 'SIMULATE' TO fu_extab.
      APPEND 'PROCESS' TO fu_extab.
    WHEN OTHERS.
  ENDCASE.

  SET PF-STATUS 'STANDARD_FULSCREEN' EXCLUDING fu_extab.
ENDFORM. "f_alv_status

*&---------------------------------------------------------------------*
*&      Form  f_alV_user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING fu_ucomm TYPE sy-ucomm
                           fu_selfield TYPE slis_selfield.
  DATA : ans TYPE char1.
  CASE fu_ucomm.
    WHEN 'SIMULATE'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Simulation Confirmation'
          text_question         = 'Anda yakin melakukan SIMULATION?'
          text_button_1         = 'Yes'
          icon_button_1         = 'ICON_OKAY'
          text_button_2         = 'No'
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
        IMPORTING
          answer                = ans
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF ans = 1.
        PERFORM f_bapi_process USING ''.
      ENDIF.
    WHEN 'PROCESS'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Process Confirmation'
          text_question         = 'Anda yakin melakukan PROCESS?'
          text_button_1         = 'Yes'
          icon_button_1         = 'ICON_OKAY'
          text_button_2         = 'No'
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
        IMPORTING
          answer                = ans
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF ans = 1.
        PERFORM f_bapi_process USING 'X'.
      ENDIF.
    WHEN 'KEMBALI'.
      CALL TRANSACTION 'ZPM002'.

  ENDCASE.
  fu_selfield-refresh = 'X'.
ENDFORM.                    "f_alv_user_command
*&---------------------------------------------------------------------*
*&      Form  F_BAPI_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_bapi_process USING fu_stat.
  DATA : ld_tabix TYPE sy-tabix.
  DATA : message(255) TYPE c.
  DATA : ld_reading_date TYPE imrc_idate,
         ld_reading_time TYPE imrc_itime.

  DATA : ls_return2 TYPE bapiret2.

** -- BAPI
  REFRESH lt_proc.
  CLEAR : ld_tabix.

  READ TABLE lt_data INTO ls_data WITH KEY status = 'F'.
  IF sy-subrc EQ 0.
    MESSAGE e001(00) WITH 'Masih ada Data error, tidak dapat diproses.'.

  ELSE.
    LOOP AT lt_data INTO ls_data WHERE status = 'S'. "Filter based on Status

      CLEAR : ld_reading_date,
              ld_reading_time, message.

      ld_tabix = sy-tabix.

      CALL FUNCTION 'CONVERT_DATE_INPUT'
        EXPORTING
          input              = ls_data-reading_date
          plausibility_check = 'X'
        IMPORTING
          output             = ld_reading_date.

      CALL FUNCTION 'CONVERT_TIME_INPUT'
        EXPORTING
          input              = ls_data-reading_time
          plausibility_check = 'X'
        IMPORTING
          output             = ld_reading_time.

      CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
        EXPORTING
          measurement_point    = ls_data-measuring_point
          reading_date         = ld_reading_date
          reading_time         = ld_reading_time
          short_text           = ls_data-short_text
          reader               = ls_data-reader
          recorded_value       = ls_data-recorded_value
          commit_work          = fu_stat
          wait_after_commit    = fu_stat
        IMPORTING
          measurement_document = ls_data-measurement_document
        EXCEPTIONS
          no_authority         = 1
          point_not_found      = 2
          index_not_unique     = 3
          type_not_found       = 4
          point_locked         = 5
          point_inactive       = 6
          timestamp_in_future  = 7
          timestamp_duprec     = 8
          unit_unfit           = 9
          value_not_fltp       = 10
          value_overflow       = 11
          value_unfit          = 12
          value_missing        = 13
          code_not_found       = 14
          notif_type_not_found = 15
          notif_prio_not_found = 16
          notif_gener_problem  = 17
          update_failed        = 18
          invalid_time         = 19
          invalid_date         = 20
          OTHERS               = 21.


      IF fu_stat EQ ''.
        PERFORM f_rollback_bapi USING ls_return2.
      ENDIF.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = sy-msgid
          msgnr               = sy-msgno
          msgv1               = sy-msgv1
          msgv2               = sy-msgv2
          msgv3               = sy-msgv3
          msgv4               = sy-msgv4
        IMPORTING
          message_text_output = message.

      ls_data-pesan = message.
      MODIFY lt_data FROM ls_data INDEX ld_tabix.

      MOVE-CORRESPONDING ls_data TO ls_proc.

      IF ls_data-measurement_document IS NOT INITIAL.
        ls_proc-status = 'S'.
        WRITE icon_green_light TO ls_proc-lampu.
      ELSE.
        ls_proc-status = 'F'.
        WRITE icon_red_light TO ls_proc-lampu.
      ENDIF.
      APPEND ls_proc TO lt_proc.

      CLEAR : ls_data, ls_proc.

    ENDLOOP.
  ENDIF.



  IF sy-subrc EQ 0.

    d_alv_type = 2.
    PERFORM f_write_data USING d_alv_type.
    d_alv_type = 1.

  ELSE.

    MESSAGE e001(00) WITH 'No data executed'.

  ENDIF.

ENDFORM.                    " F_BAPI_PROCESS
*&---------------------------------------------------------------------*
*&      Form  F_ROLLBACK_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_RETURN  text
*----------------------------------------------------------------------*
FORM f_rollback_bapi  USING    fs_return STRUCTURE bapiret2.
  CLEAR fs_return.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
    IMPORTING
      return = fs_return.

ENDFORM.                    " F_ROLLBACK_BAPI
*&---------------------------------------------------------------------*
*&      Form  F_COMMIT_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_RETURN  text
*----------------------------------------------------------------------*
FORM f_commit_bapi  USING    fs_return STRUCTURE bapiret2.
  CLEAR fs_return.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait   = 'X'
    IMPORTING
      return = fs_return.

ENDFORM.                    " F_COMMIT_BAPI

*&---------------------------------------------------------------------*
*&      Form  f_check_hourmeter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_check_hourmeter.

  CLEAR ls_hm_invts.

  CALL FUNCTION 'CONVERT_DATE_INPUT'
    EXPORTING
      input              = ls_data-reading_date
      plausibility_check = 'X'
    IMPORTING
      output             = ld_datlo.

  CALL FUNCTION 'CONVERT_TIME_INPUT'
    EXPORTING
      input              = ls_data-reading_time
      plausibility_check = 'X'
    IMPORTING
      output             = ld_timlo.


  CALL FUNCTION 'TIMESTAMP_INVERT_11_DIGITS'
    EXPORTING
      idate         = ld_datlo
      itime         = ld_timlo
    IMPORTING
      timestamp_inv = ls_hm_invts.

  SELECT * FROM imrg INTO ls_hm_imrg
      UP TO 1 ROWS
      WHERE point     = ls_data-measuring_point
        AND invts    >= ls_hm_invts
        AND cancl     = space
        AND cdiff    <> 0
      ORDER BY mandt point invts.
    MOVE ls_hm_imrg TO ls_hm_last_imrg.
    EXIT.
  ENDSELECT.

  IF ls_hm_last_imrg IS NOT INITIAL.
    currdate  = ld_datlo.
    lastdate  = ls_hm_last_imrg-idate.
    currtime  = ld_timlo.
    lasttime   = ls_hm_last_imrg-itime.

    IF ls_hm_last_imrg-idate NE '00000000'.
      CALL FUNCTION 'L_TO_TIME_DIFF'
        EXPORTING
          i_start_date = lastdate
          i_start_time = lasttime
          i_end_date   = currdate
          i_end_time   = currtime
          i_time_uom   = 'H'
        IMPORTING
          e_time_diff  = deltactr.

      CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION' "------------------ Pun10 remarks by FRZ 20/07/2022 start
        EXPORTING
          i_number_of_digits       = '3'
          i_fltp_value             = ls_hm_last_imrg-recdv
          i_value_not_initial_flag = 'X'
          i_screen_fieldlength     = '16'
        IMPORTING
          e_char_field             = lastread."----------------------Pun10 remarks by FRZ 20/07/2022 end

*
*      CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
*        EXPORTING
*          i_number_of_digits       = '3'
*          i_fltp_value             = ls_hm_last_imrg-readg "-------------------------- Pun10 edited by Frz 20/07/2022
*          i_value_not_initial_flag = 'X'
*          i_screen_fieldlength     = '16'
*        IMPORTING
*          e_char_field             = lastread.

      hourmeter_m  = ls_data-recorded_value.

      REPLACE ',' WITH '.' INTO lastread.
      REPLACE ',' WITH '.' INTO hourmeter_m.

      lastread_n  = lastread.
      hourmeter_n = hourmeter_m.
      deltaread   = hourmeter_n - lastread_n.

    ENDIF.

  ENDIF.
ENDFORM.                    "f_check_hourmeter
