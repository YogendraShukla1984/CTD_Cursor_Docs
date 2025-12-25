*&---------------------------------------------------------------------*
*& Report  ZSCM_CTD_ENRICHTRIPDETAILS
*&---------------------------------------------------------------------*
*& Purpose: CTD Trip Details Enrichment Program
*& Author: [Author Name]
*& Creation Date: [Date]
*& Change History:
*& Date       User    Description
*& DD.MM.YYYY USERID  Initial creation
*&---------------------------------------------------------------------*
REPORT zscm_ctd_enrichtripdetails.

*&---------------------------------------------------------------------*
*& Include: ZSCM_CTD_ENRICHTRIPDETAILSTOP
*&---------------------------------------------------------------------*
INCLUDE zscm_ctd_enrichtripdetailstop.

*&---------------------------------------------------------------------*
*& Include: ZSCM_CTD_ENRICHTRIPDETAILSC01
*&---------------------------------------------------------------------*
INCLUDE zscm_ctd_enrichtripdetailsc01.

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
  PARAMETERS: p_from_date TYPE datum OBLIGATORY,
              p_to_date TYPE datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-002.
  SELECT-OPTIONS: s_trip_no FOR gv_trip_no.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-003.
  PARAMETERS: p_testrun TYPE char1 AS CHECKBOX DEFAULT space.
SELECTION-SCREEN END OF BLOCK b03.

*&---------------------------------------------------------------------*
*& Initialization
*&---------------------------------------------------------------------*
INITIALIZATION.
  " Check transaction authorization
  AUTHORITY-CHECK OBJECT 'S_TCODE'
                  ID 'TCD' FIELD 'ZCTD_ENRICH_TRIPDETAILS'.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE w077(s#) WITH 'ZCTD_ENRICH_TRIPDETAILS' DISPLAY LIKE 'E'.
    LEAVE PROGRAM.
  ENDIF.

*&---------------------------------------------------------------------*
*& Selection Screen Validations
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  " Date range validation
  IF p_from_date > p_to_date.
    MESSAGE e001(zctd_msg) WITH p_from_date p_to_date.
  ENDIF.

AT SELECTION-SCREEN ON p_from_date.
  " Validate date format
  IF p_from_date IS INITIAL.
    MESSAGE e002(zctd_msg).
  ENDIF.

AT SELECTION-SCREEN ON p_to_date.
  " Validate date format
  IF p_to_date IS INITIAL.
    MESSAGE e002(zctd_msg).
  ENDIF.

*&---------------------------------------------------------------------*
*& Start of Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  " BEGIN: Cursor Generated Code
  DATA: lo_main_processor TYPE REF TO lcl_main_processor,
        lt_output TYPE gty_trip_leg_output_table,
        lt_log TYPE gty_log_table,
        lo_exception TYPE REF TO cx_root,
        lv_error_text TYPE string,
        lt_trip_numbers TYPE gty_trip_number_table,
        lw_trip_number TYPE gty_trip_number,
        lv_test_run TYPE char1.

  " Convert select-options to trip number table
  IF s_trip_no[] IS NOT INITIAL.
    LOOP AT s_trip_no INTO lw_trip_number.
      APPEND lw_trip_number TO lt_trip_numbers.
    ENDLOOP.
  ENDIF.

  " Set test run flag
  IF p_testrun = abap_true.
    lv_test_run = gc_test_run_x.
  ELSE.
    CLEAR lv_test_run.
  ENDIF.

  " Create main processor instance
  CREATE OBJECT lo_main_processor
    EXPORTING
      iv_from_date = p_from_date
      iv_to_date = p_to_date
      it_trip_numbers = lt_trip_numbers
      iv_test_run = lv_test_run.

  " Execute processing
  TRY.
      CALL METHOD lo_main_processor->execute
        IMPORTING
          et_output = lt_output
          et_log = lt_log.

      " Display results using SALV (simpler for reports)
      IF lt_output IS NOT INITIAL.
        " Use SALV for report display
        DATA: lo_salv TYPE REF TO cl_salv_table,
              lo_functions TYPE REF TO cl_salv_functions,
              lo_display TYPE REF TO cl_salv_display_settings,
              lo_columns TYPE REF TO cl_salv_columns_table,
              lo_column TYPE REF TO cl_salv_column_table.

        TRY.
            CALL METHOD cl_salv_table=>factory
              IMPORTING
                r_salv_table = lo_salv
              CHANGING
                t_table = lt_output.

            " Enable functions
            lo_functions = lo_salv->get_functions( ).
            CALL METHOD lo_functions->set_all
              EXPORTING
                value = abap_true.

            " Set display settings
            lo_display = lo_salv->get_display_settings( ).
            CALL METHOD lo_display->set_striped_pattern
              EXPORTING
                value = abap_true.

            " Optimize column widths
            lo_columns = lo_salv->get_columns( ).
            CALL METHOD lo_columns->set_optimize
              EXPORTING
                value = abap_true.

            " Display
            CALL METHOD lo_salv->display.

          CATCH cx_salv_msg INTO lo_exception.
            lv_error_text = lo_exception->get_text( ).
            MESSAGE lv_error_text TYPE 'I' DISPLAY LIKE 'E'.
        ENDTRY.

      ELSE.
        MESSAGE i001(zctd_msg) TYPE 'I'.
      ENDIF.

      " Display log if available
      IF lt_log IS NOT INITIAL.
        " Log can be displayed in separate ALV or message log
      ENDIF.

    CATCH cx_root INTO lo_exception.
      " Get error text
      lv_error_text = lo_exception->get_text( ).
      
      " Display error
      MESSAGE lv_error_text TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
  " END: Cursor Generated Code

