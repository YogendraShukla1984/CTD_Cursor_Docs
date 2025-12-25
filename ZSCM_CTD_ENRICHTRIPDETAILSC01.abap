*&---------------------------------------------------------------------*
*& Include  ZSCM_CTD_ENRICHTRIPDETAILSC01
*&---------------------------------------------------------------------*
*& Purpose: Local Classes for CTD Trip Details Enrichment
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Class: lcl_main_processor
*&---------------------------------------------------------------------*
CLASS lcl_main_processor DEFINITION.
  PUBLIC SECTION.
    "! Constructor
    "! @parameter iv_from_date | From Date
    "! @parameter iv_to_date | To Date
    "! @parameter it_trip_numbers | Trip Number Range
    "! @parameter iv_test_run | Test Run Flag
    METHODS constructor
      IMPORTING iv_from_date TYPE datum
                iv_to_date TYPE datum
                it_trip_numbers TYPE gty_trip_number_table
                iv_test_run TYPE char1.

    "! Execute main processing
    "! @parameter et_output | Enriched Trip Leg Output
    "! @parameter et_log | Processing Log
    "! @raising cx_root | Processing Error
    METHODS execute
      EXPORTING et_output TYPE gty_trip_leg_output_table
                et_log TYPE gty_log_table
      RAISING cx_root.

  PRIVATE SECTION.
    DATA: mv_from_date TYPE datum,
          mv_to_date TYPE datum,
          mt_trip_numbers TYPE gty_trip_number_table,
          mv_test_run TYPE char1,
          mo_data_selector TYPE REF TO lcl_data_selector,
          mo_enrichment_processor TYPE REF TO lcl_enrichment_processor,
          mo_validator TYPE REF TO lcl_validator.

    METHODS update_database
      IMPORTING it_output TYPE gty_trip_leg_output_table
                it_valid_trips TYPE gty_trip_header_table
      EXPORTING et_log TYPE gty_log_table.
ENDCLASS.

CLASS lcl_main_processor IMPLEMENTATION.
  METHOD constructor.
    mv_from_date = iv_from_date.
    mv_to_date = iv_to_date.
    mt_trip_numbers = it_trip_numbers.
    mv_test_run = iv_test_run.

    " Create component instances
    CREATE OBJECT mo_data_selector.
    CREATE OBJECT mo_enrichment_processor.
    CREATE OBJECT mo_validator.
  ENDMETHOD.

  METHOD execute.
    " BEGIN: Cursor Generated Code
    DATA: lt_trip_headers TYPE gty_trip_header_table,
          lt_trip_legs TYPE gty_trip_leg_input_table,
          lt_enriched_legs TYPE gty_trip_leg_output_table,
          lt_valid_trips TYPE gty_trip_header_table,
          lt_invalid_trips TYPE gty_trip_header_table,
          lt_log_legs TYPE gty_log_table,
          lt_log_enrich TYPE gty_log_table,
          lt_log_valid TYPE gty_log_table,
          lt_log_update TYPE gty_log_table,
          lw_log TYPE gty_log,
          lw_trip_header TYPE gty_trip_header,
          lv_timestamp TYPE timestamp.

    CLEAR: et_output, et_log.

    " Get current timestamp
    GET TIME STAMP FIELD lv_timestamp.

    " Step 1: Fetch trip headers
    CALL METHOD mo_data_selector->fetch_trip_headers
      EXPORTING
        iv_from_date = mv_from_date
        iv_to_date = mv_to_date
        it_trip_numbers = mt_trip_numbers
      IMPORTING
        et_trip_headers = lt_trip_headers
        et_log = lt_log_legs.

    APPEND LINES OF lt_log_legs TO et_log.

    IF lt_trip_headers IS INITIAL.
      " No trips found
      CLEAR lw_log.
      lw_log-msgty = 'I'.
      lw_log-msgid = 'ZCTD_MSG'.
      lw_log-msgno = '001'.
      lw_log-timestamp = lv_timestamp.
      APPEND lw_log TO et_log.
      RETURN.
    ENDIF.

    " Step 2: Fetch trip legs
    CALL METHOD mo_data_selector->fetch_trip_legs
      EXPORTING
        it_trip_headers = lt_trip_headers
      IMPORTING
        et_trip_legs = lt_trip_legs
        et_log = lt_log_legs.

    APPEND LINES OF lt_log_legs TO et_log.

    IF lt_trip_legs IS INITIAL.
      RETURN.
    ENDIF.

    " Step 3: Validate trip legs
    CALL METHOD mo_validator->validate_trip_legs
      IMPORTING
        it_trip_legs = lt_trip_legs
      EXPORTING
        et_valid_trips = lt_valid_trips
        et_invalid_trips = lt_invalid_trips
        et_log = lt_log_valid.

    APPEND LINES OF lt_log_valid TO et_log.

    IF lt_valid_trips IS INITIAL.
      RETURN.
    ENDIF.

    " Step 4: Enrich loaded legs
    CALL METHOD mo_enrichment_processor->enrich_loaded_legs
      IMPORTING
        it_trip_legs = lt_trip_legs
        it_valid_trips = lt_valid_trips
      EXPORTING
        et_enriched_legs = lt_enriched_legs
        et_log = lt_log_enrich.

    APPEND LINES OF lt_log_enrich TO et_log.

    " Step 5: Insert empty legs
    CALL METHOD mo_enrichment_processor->insert_empty_legs
      CHANGING
        ct_trip_legs = lt_enriched_legs
      EXPORTING
        et_log = lt_log_enrich.

    APPEND LINES OF lt_log_enrich TO et_log.

    " Step 6: Validate empty legs
    CALL METHOD mo_validator->validate_empty_legs
      IMPORTING
        it_trip_legs = lt_enriched_legs
      EXPORTING
        et_valid_trips = lt_valid_trips
        et_invalid_trips = lt_invalid_trips
        et_log = lt_log_valid.

    APPEND LINES OF lt_log_valid TO et_log.

    " Step 7: Determine material and region
    CALL METHOD mo_enrichment_processor->determine_material_region
      CHANGING
        ct_trip_legs = lt_enriched_legs
      EXPORTING
        et_log = lt_log_enrich.

    APPEND LINES OF lt_log_enrich TO et_log.

    " Step 8: Update database (if not test run)
    IF mv_test_run <> gc_test_run_x.
      CALL METHOD update_database
        EXPORTING
          it_output = lt_enriched_legs
          it_valid_trips = lt_valid_trips
        IMPORTING
          et_log = lt_log_update.

      APPEND LINES OF lt_log_update TO et_log.
    ENDIF.

      " Set output
      et_output = lt_enriched_legs.

      " Set trip status in output (for display)
      " Sort valid trips for binary search
      SORT lt_valid_trips BY trip_no.
      FIELD-SYMBOLS: <lfs_output> TYPE gty_trip_leg_output.
      LOOP AT et_output ASSIGNING <lfs_output>.
        READ TABLE lt_valid_trips INTO lw_trip_header
          WITH KEY trip_no = <lfs_output>-trip_no
          BINARY SEARCH.
        IF sy-subrc = 0.
          <lfs_output>-trip_status = gc_status_enriched.
        ELSE.
          <lfs_output>-trip_status = gc_status_completed.
        ENDIF.
      ENDLOOP.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD update_database.
    " BEGIN: Cursor Generated Code
    DATA: lw_output TYPE gty_trip_leg_output,
          lw_trip_header TYPE gty_trip_header,
          lw_log TYPE gty_log,
          lv_timestamp TYPE timestamp,
          lv_error_occurred TYPE abap_bool.

    CLEAR: et_log, lv_error_occurred.
    GET TIME STAMP FIELD lv_timestamp.

    " Update trip legs
    LOOP AT it_output INTO lw_output.
      UPDATE zsce_ctd_itm SET
        leg_type = lw_output-leg_type
        source_zone = lw_output-source_zone
        dest_zone = lw_output-dest_zone
        route = lw_output-route
        distance = lw_output-distance
        matnr = lw_output-matnr
        source_region = lw_output-source_region
        dest_region = lw_output-dest_region
        business_id = lw_output-business_id
        subbusiness_id = lw_output-subbusiness_id
        dest_exit_date = lw_output-dest_exit_date
        source_ent_date = lw_output-source_ent_date
        ctd_ruleeng_remarks = lw_output-ctd_ruleeng_remarks
      WHERE trip_no = lw_output-trip_no
        AND counter = lw_output-counter.

      IF sy-subrc <> 0.
        lv_error_occurred = abap_true.
        CLEAR lw_log.
        lw_log-trip_no = lw_output-trip_no.
        lw_log-counter = lw_output-counter.
        lw_log-msgty = 'E'.
        lw_log-msgid = 'ZCTD_MSG'.
        lw_log-msgno = '009'.
        lw_log-timestamp = lv_timestamp.
        APPEND lw_log TO et_log.
      ENDIF.
    ENDLOOP.

    " Update trip headers (status)
    LOOP AT it_valid_trips INTO lw_trip_header.
      UPDATE zsce_ctd_hdr SET
        trip_status = gc_status_enriched
      WHERE trip_no = lw_trip_header-trip_no.

      IF sy-subrc <> 0.
        lv_error_occurred = abap_true.
        CLEAR lw_log.
        lw_log-trip_no = lw_trip_header-trip_no.
        lw_log-msgty = 'E'.
        lw_log-msgid = 'ZCTD_MSG'.
        lw_log-msgno = '010'.
        lw_log-timestamp = lv_timestamp.
        APPEND lw_log TO et_log.
      ENDIF.
    ENDLOOP.

    " Commit or rollback
    IF lv_error_occurred = abap_true.
      ROLLBACK WORK.
    ELSE.
      COMMIT WORK.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class: lcl_data_selector
*&---------------------------------------------------------------------*
CLASS lcl_data_selector DEFINITION.
  PUBLIC SECTION.
    "! Fetch trip headers
    "! @parameter iv_from_date | From Date
    "! @parameter iv_to_date | To Date
    "! @parameter it_trip_numbers | Trip Number Range
    "! @parameter et_trip_headers | Trip Headers
    "! @parameter et_log | Log
    METHODS fetch_trip_headers
      IMPORTING iv_from_date TYPE datum
                iv_to_date TYPE datum
                it_trip_numbers TYPE gty_trip_number_table
      EXPORTING et_trip_headers TYPE gty_trip_header_table
                et_log TYPE gty_log_table.

    "! Fetch trip legs
    "! @parameter it_trip_headers | Trip Headers
    "! @parameter et_trip_legs | Trip Legs
    "! @parameter et_log | Log
    METHODS fetch_trip_legs
      IMPORTING it_trip_headers TYPE gty_trip_header_table
      EXPORTING et_trip_legs TYPE gty_trip_leg_input_table
                et_log TYPE gty_log_table.

  PRIVATE SECTION.
    METHODS validate_trip_date_range
      IMPORTING iv_trip_no TYPE zsce_ctd_hdr-trip_no
                iv_created_date TYPE datum
                iv_from_date TYPE datum
                iv_to_date TYPE datum
      RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.

CLASS lcl_data_selector IMPLEMENTATION.
  METHOD fetch_trip_headers.
    " BEGIN: Cursor Generated Code
    DATA: lt_ctd_hdr_selected TYPE tt_ctd_hdr_selected,
          lw_ctd_hdr_selected TYPE ty_ctd_hdr_selected,
          lw_trip_header TYPE gty_trip_header,
          lw_trip_number TYPE gty_trip_number,
          lt_trip_no_range TYPE RANGE OF zsce_ctd_hdr-trip_no,
          lw_trip_no_range TYPE LINE OF RANGE OF zsce_ctd_hdr-trip_no,
          lw_log TYPE gty_log,
          lv_timestamp TYPE timestamp.

    CLEAR: et_trip_headers, et_log.
    GET TIME STAMP FIELD lv_timestamp.

    " Build trip number range if provided
    IF it_trip_numbers[] IS NOT INITIAL.
      LOOP AT it_trip_numbers INTO lw_trip_number.
        CLEAR lw_trip_no_range.
        lw_trip_no_range-sign = lw_trip_number-sign.
        lw_trip_no_range-option = lw_trip_number-option.
        lw_trip_no_range-low = lw_trip_number-low.
        lw_trip_no_range-high = lw_trip_number-high.
        APPEND lw_trip_no_range TO lt_trip_no_range.
      ENDLOOP.
    ENDIF.

    " SELECT fields: trip_no, lifnr, truck_no, trip_status, created_date
    SELECT trip_no lifnr truck_no trip_status created_date
      FROM zsce_ctd_hdr
      INTO TABLE lt_ctd_hdr_selected
      WHERE trip_status = gc_status_completed
        AND created_date BETWEEN iv_from_date AND iv_to_date
        AND ( lt_trip_no_range[] IS INITIAL OR trip_no IN lt_trip_no_range[] ).

    IF sy-subrc = 0.
      " Convert to output structure and validate date range
      LOOP AT lt_ctd_hdr_selected INTO lw_ctd_hdr_selected.
        " Validate trip date range if specific trips provided
        IF it_trip_numbers[] IS NOT INITIAL.
          IF validate_trip_date_range(
               iv_trip_no = lw_ctd_hdr_selected-trip_no
               iv_created_date = lw_ctd_hdr_selected-created_date
               iv_from_date = iv_from_date
               iv_to_date = iv_to_date ) = abap_false.
            " Log error
            CLEAR lw_log.
            lw_log-trip_no = lw_ctd_hdr_selected-trip_no.
            lw_log-msgty = 'E'.
            lw_log-msgid = 'ZCTD_MSG'.
            lw_log-msgno = '003'.
            lw_log-msgv1 = lw_ctd_hdr_selected-trip_no.
            lw_log-timestamp = lv_timestamp.
            APPEND lw_log TO et_log.
            CONTINUE.
          ENDIF.
        ENDIF.

        " Add to output
        CLEAR lw_trip_header.
        lw_trip_header-trip_no = lw_ctd_hdr_selected-trip_no.
        lw_trip_header-lifnr = lw_ctd_hdr_selected-lifnr.
        lw_trip_header-truck_no = lw_ctd_hdr_selected-truck_no.
        lw_trip_header-trip_status = lw_ctd_hdr_selected-trip_status.
        lw_trip_header-created_date = lw_ctd_hdr_selected-created_date.
        APPEND lw_trip_header TO et_trip_headers.
      ENDLOOP.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD fetch_trip_legs.
    " BEGIN: Cursor Generated Code
    DATA: lt_trip_no TYPE TABLE OF zsce_ctd_hdr-trip_no,
          lw_trip_header TYPE gty_trip_header,
          lt_ctd_itm_selected TYPE TABLE OF zsce_ctd_itm,
          lw_ctd_itm_selected TYPE zsce_ctd_itm,
          lw_trip_leg TYPE gty_trip_leg_input.

    CLEAR: et_trip_legs, et_log.

    " Check if trip headers are provided
    IF it_trip_headers IS INITIAL.
      RETURN.
    ENDIF.

    " Extract trip numbers and deduplicate
    LOOP AT it_trip_headers INTO lw_trip_header.
      APPEND lw_trip_header-trip_no TO lt_trip_no.
    ENDLOOP.

    SORT lt_trip_no.
    DELETE ADJACENT DUPLICATES FROM lt_trip_no.

    " SELECT with FOR ALL ENTRIES
    IF lt_trip_no IS NOT INITIAL.
      SELECT trip_no counter lifnr truck_no shnumber
             source_date dest_date mvt_type area adrnr adrnz
        FROM zsce_ctd_itm
        INTO TABLE lt_ctd_itm_selected
        FOR ALL ENTRIES IN lt_trip_no
        WHERE trip_no = lt_trip_no-table_line.

      IF sy-subrc = 0.
        " Convert to output structure
        LOOP AT lt_ctd_itm_selected INTO lw_ctd_itm_selected.
          CLEAR lw_trip_leg.
          lw_trip_leg-trip_no = lw_ctd_itm_selected-trip_no.
          lw_trip_leg-counter = lw_ctd_itm_selected-counter.
          lw_trip_leg-lifnr = lw_ctd_itm_selected-lifnr.
          lw_trip_leg-truck_no = lw_ctd_itm_selected-truck_no.
          lw_trip_leg-shnumber = lw_ctd_itm_selected-shnumber.
          lw_trip_leg-source_date = lw_ctd_itm_selected-source_date.
          lw_trip_leg-dest_date = lw_ctd_itm_selected-dest_date.
          lw_trip_leg-mvt_type = lw_ctd_itm_selected-mvt_type.
          lw_trip_leg-area = lw_ctd_itm_selected-area.
          lw_trip_leg-adrnr = lw_ctd_itm_selected-adrnr.
          lw_trip_leg-adrnz = lw_ctd_itm_selected-adrnz.
          APPEND lw_trip_leg TO et_trip_legs.
        ENDLOOP.

        " Sort by trip_no and counter
        SORT et_trip_legs BY trip_no counter.
      ENDIF.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD validate_trip_date_range.
    " BEGIN: Cursor Generated Code
    IF iv_created_date BETWEEN iv_from_date AND iv_to_date.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class: lcl_enrichment_processor
*&---------------------------------------------------------------------*
CLASS lcl_enrichment_processor DEFINITION.
  PUBLIC SECTION.
    "! Enrich loaded legs
    "! @parameter it_trip_legs | Trip Legs Input
    "! @parameter it_valid_trips | Valid Trip Headers
    "! @parameter et_enriched_legs | Enriched Trip Legs
    "! @parameter et_log | Log
    METHODS enrich_loaded_legs
      IMPORTING it_trip_legs TYPE gty_trip_leg_input_table
                it_valid_trips TYPE gty_trip_header_table
      EXPORTING et_enriched_legs TYPE gty_trip_leg_output_table
                et_log TYPE gty_log_table.

    "! Insert empty legs
    "! @parameter ct_trip_legs | Trip Legs (CHANGING)
    "! @parameter et_log | Log
    METHODS insert_empty_legs
      CHANGING ct_trip_legs TYPE gty_trip_leg_output_table
      EXPORTING et_log TYPE gty_log_table.

    "! Determine material and region
    "! @parameter ct_trip_legs | Trip Legs (CHANGING)
    "! @parameter et_log | Log
    METHODS determine_material_region
      CHANGING ct_trip_legs TYPE gty_trip_leg_output_table
      EXPORTING et_log TYPE gty_log_table.

  PRIVATE SECTION.
    METHODS fetch_shipment_details
      IMPORTING iv_shnumber TYPE oigss-shnumber
      EXPORTING es_shipment_details TYPE gty_shipment_details
                ev_found TYPE abap_bool.

    METHODS determine_dest_exit_date
      IMPORTING iv_mvt_type TYPE char2
                iv_dest_date TYPE datum
                iv_shnumber TYPE oigss-shnumber
      RETURNING VALUE(rv_exit_date) TYPE datum.

    METHODS determine_source_ent_date
      IMPORTING iv_mvt_type TYPE char2
                iv_source_date TYPE datum
                iv_shnumber TYPE oigss-shnumber
      RETURNING VALUE(rv_ent_date) TYPE datum.

    METHODS fetch_business_info
      IMPORTING iv_shnumber TYPE oigss-shnumber
      EXPORTING ev_business_id TYPE zsce_ctd_itm-business_id
                ev_subbusiness_id TYPE zsce_ctd_itm-subbusiness_id.

    METHODS fetch_material
      IMPORTING iv_shnumber TYPE oigss-shnumber
      RETURNING VALUE(rv_matnr) TYPE matnr.

    METHODS fetch_regions
      IMPORTING iv_adrnr TYPE adrnr
                iv_adrnz TYPE adrnz
      EXPORTING ev_source_region TYPE zsce_ctd_itm-source_region
                ev_dest_region TYPE zsce_ctd_itm-dest_region.

    METHODS fetch_route_distance
      IMPORTING iv_source_zone TYPE zsce_ctd_itm-source_zone
                iv_dest_zone TYPE zsce_ctd_itm-dest_zone
      EXPORTING ev_route TYPE zsce_ctd_itm-route
                ev_distance TYPE zsce_ctd_itm-distance
                ev_found TYPE abap_bool.

    DATA: mt_shipment_details TYPE gty_shipment_details_table,
          mt_oigss_data TYPE tt_oigss_selected,
          mt_oigsi_data TYPE tt_oigsi_selected,
          mt_lips_data TYPE tt_lips_selected,
          mt_adrc_data TYPE tt_adrc_selected,
          mt_yttstx0001_data TYPE tt_yttstx0001_selected,
          mt_yttstx0002_data TYPE tt_yttstx0002_selected,
          mt_trolz_data TYPE tt_trolz_selected,
          mt_tvro_data TYPE tt_tvro_selected.
ENDCLASS.

CLASS lcl_enrichment_processor IMPLEMENTATION.
  METHOD enrich_loaded_legs.
    " BEGIN: Cursor Generated Code
    DATA: lw_trip_leg TYPE gty_trip_leg_input,
          lw_enriched_leg TYPE gty_trip_leg_output,
          lw_trip_header TYPE gty_trip_header,
          lw_shipment_details TYPE gty_shipment_details,
          lt_shnumber TYPE TABLE OF oigss-shnumber,
          lw_shnumber TYPE oigss-shnumber,
          lw_oigss TYPE ty_oigss_selected,
          lv_found TYPE abap_bool,
          lv_dest_exit_date TYPE datum,
          lv_source_ent_date TYPE datum,
          lv_business_id TYPE zsce_ctd_itm-business_id,
          lv_subbusiness_id TYPE zsce_ctd_itm-subbusiness_id,
          lv_timestamp TYPE timestamp,
          lw_valid_trip TYPE gty_trip_header.

    CLEAR: et_enriched_legs, et_log.
    GET TIME STAMP FIELD lv_timestamp.

    " Collect shipment numbers for FOR ALL ENTRIES
    LOOP AT it_trip_legs INTO lw_trip_leg WHERE shnumber <> space.
      APPEND lw_trip_leg-shnumber TO lt_shnumber.
    ENDLOOP.

    SORT lt_shnumber.
    DELETE ADJACENT DUPLICATES FROM lt_shnumber.

    " Fetch shipment details
    IF lt_shnumber IS NOT INITIAL.
      " SELECT fields: shnumber, route, distance, source_zone, dest_zone, adrnr, adrnz
      SELECT shnumber route distance source_zone dest_zone adrnr adrnz
        FROM oigss
        INTO TABLE mt_oigss_data
        FOR ALL ENTRIES IN lt_shnumber
        WHERE shnumber = lt_shnumber-table_line
          AND tstyp = gc_oigss_tstyp.

      IF sy-subrc = 0.
        SORT mt_oigss_data BY shnumber.
      ENDIF.
    ENDIF.

    " Process each trip leg (only for valid trips)
    LOOP AT it_trip_legs INTO lw_trip_leg.
      " Check if trip is valid
      READ TABLE it_valid_trips INTO lw_valid_trip
        WITH KEY trip_no = lw_trip_leg-trip_no.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR lw_enriched_leg.

      " Copy basic fields
      lw_enriched_leg-trip_no = lw_trip_leg-trip_no.
      lw_enriched_leg-counter = lw_trip_leg-counter.
      lw_enriched_leg-lifnr = lw_trip_leg-lifnr.
      lw_enriched_leg-truck_no = lw_trip_leg-truck_no.
      lw_enriched_leg-shnumber = lw_trip_leg-shnumber.
      lw_enriched_leg-source_date = lw_trip_leg-source_date.
      lw_enriched_leg-dest_date = lw_trip_leg-dest_date.
      lw_enriched_leg-area = lw_trip_leg-area.
      lw_enriched_leg-mvt_type = lw_trip_leg-mvt_type.

      " Classify leg type
      IF lw_trip_leg-shnumber <> space.
        lw_enriched_leg-leg_type = gc_leg_type_loaded.

        " Fetch shipment details
        READ TABLE mt_oigss_data INTO lw_oigss
          WITH KEY shnumber = lw_trip_leg-shnumber
          BINARY SEARCH.

        IF sy-subrc = 0.
          lw_enriched_leg-route = lw_oigss-route.
          lw_enriched_leg-distance = lw_oigss-distance.
          lw_enriched_leg-source_zone = lw_oigss-source_zone.
          lw_enriched_leg-dest_zone = lw_oigss-dest_zone.

          " Determine dates
          lv_dest_exit_date = determine_dest_exit_date(
            iv_mvt_type = lw_trip_leg-mvt_type
            iv_dest_date = lw_trip_leg-dest_date
            iv_shnumber = lw_trip_leg-shnumber ).

          lv_source_ent_date = determine_source_ent_date(
            iv_mvt_type = lw_trip_leg-mvt_type
            iv_source_date = lw_trip_leg-source_date
            iv_shnumber = lw_trip_leg-shnumber ).

          lw_enriched_leg-dest_exit_date = lv_dest_exit_date.
          lw_enriched_leg-source_ent_date = lv_source_ent_date.

          " Fetch business info
          CALL METHOD fetch_business_info
            EXPORTING
              iv_shnumber = lw_trip_leg-shnumber
            IMPORTING
              ev_business_id = lv_business_id
              ev_subbusiness_id = lv_subbusiness_id.

          lw_enriched_leg-business_id = lv_business_id.
          lw_enriched_leg-subbusiness_id = lv_subbusiness_id.
        ENDIF.
      ELSE.
        lw_enriched_leg-leg_type = gc_leg_type_empty.
      ENDIF.

      APPEND lw_enriched_leg TO et_enriched_legs.
    ENDLOOP.

    " Sort by trip_no and counter
    SORT et_enriched_legs BY trip_no counter.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD insert_empty_legs.
    " BEGIN: Cursor Generated Code
    DATA: lt_temp_legs TYPE gty_trip_leg_output_table,
          lw_current_leg TYPE gty_trip_leg_output,
          lw_previous_leg TYPE gty_trip_leg_output,
          lw_empty_leg TYPE gty_trip_leg_output,
          lw_trolz TYPE ty_trolz_selected,
          lw_tvro TYPE ty_tvro_selected,
          lv_counter TYPE i,
          lv_route_found TYPE abap_bool,
          lv_distance_found TYPE abap_bool,
          lv_source_zone TYPE zsce_ctd_itm-source_zone,
          lv_dest_zone TYPE zsce_ctd_itm-dest_zone.

    CLEAR: et_log.

    " Sort legs by trip_no and counter
    SORT ct_trip_legs BY trip_no counter.

    " Process each trip
    DATA: lv_current_trip TYPE zsce_ctd_hdr-trip_no,
          lv_previous_trip TYPE zsce_ctd_hdr-trip_no.

    CLEAR lv_current_trip.
    CLEAR lv_previous_trip.

    LOOP AT ct_trip_legs INTO lw_current_leg.
      " Check if new trip
      IF lw_current_leg-trip_no <> lv_previous_trip.
        lv_previous_trip = lw_current_leg-trip_no.
        CLEAR lw_previous_leg.
        CONTINUE.
      ENDIF.

      " Check if empty leg needed (per BRD: check area field)
      " Insert empty leg when: Destination Area of preceding leg is blank,
      " or Destination Area ≠ Source Area of subsequent leg
      IF lw_previous_leg-area IS INITIAL OR
         lw_previous_leg-area <> lw_current_leg-area.

        " Create empty leg
        CLEAR lw_empty_leg.
        lw_empty_leg-trip_no = lw_current_leg-trip_no.
        lw_empty_leg-counter = lw_previous_leg-counter + 1.
        lw_empty_leg-lifnr = lw_current_leg-lifnr.
        lw_empty_leg-truck_no = lw_current_leg-truck_no.
        lw_empty_leg-leg_type = gc_leg_type_empty.
        lw_empty_leg-source_zone = lw_previous_leg-dest_zone.
        lw_empty_leg-dest_zone = lw_current_leg-source_zone.
        lw_empty_leg-source_date = lw_previous_leg-dest_exit_date.
        lw_empty_leg-dest_date = lw_current_leg-source_ent_date.

        " Fetch route and distance
        CALL METHOD fetch_route_distance
          EXPORTING
            iv_source_zone = lw_empty_leg-source_zone
            iv_dest_zone = lw_empty_leg-dest_zone
          IMPORTING
            ev_route = lw_empty_leg-route
            ev_distance = lw_empty_leg-distance
            ev_found = lv_route_found.

        " Adjust counters for subsequent legs
        FIELD-SYMBOLS: <lfs_leg> TYPE gty_trip_leg_output.
        LOOP AT ct_trip_legs ASSIGNING <lfs_leg>
          WHERE trip_no = lw_current_leg-trip_no
            AND counter > lw_previous_leg-counter.
          <lfs_leg>-counter = <lfs_leg>-counter + 1.
        ENDLOOP.

        " Insert empty leg
        INSERT lw_empty_leg INTO ct_trip_legs INDEX sy-tabix.
      ENDIF.

      lw_previous_leg = lw_current_leg.
    ENDLOOP.

    " Re-sort after insertions
    SORT ct_trip_legs BY trip_no counter.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD determine_material_region.
    " BEGIN: Cursor Generated Code
    DATA: lt_shnumber TYPE TABLE OF oigss-shnumber,
          lw_shnumber TYPE oigss-shnumber,
          lt_adrnr TYPE TABLE OF adrnr,
          lw_adrnr TYPE adrnr,
          lw_leg TYPE gty_trip_leg_output,
          lw_oigsi TYPE ty_oigsi_selected,
          lw_lips TYPE ty_lips_selected,
          lw_adrc TYPE ty_adrc_selected,
          lv_matnr TYPE matnr,
          lv_source_region TYPE zsce_ctd_itm-source_region,
          lv_dest_region TYPE zsce_ctd_itm-dest_region,
          lt_vbeln TYPE TABLE OF lips-vbeln,
          lw_vbeln TYPE lips-vbeln,
          lw_oigss TYPE ty_oigss_selected.

    CLEAR: et_log.

    " Collect shipment numbers and addresses for FOR ALL ENTRIES
    LOOP AT ct_trip_legs INTO lw_leg WHERE leg_type = gc_leg_type_loaded.
      IF lw_leg-shnumber <> space.
        APPEND lw_leg-shnumber TO lt_shnumber.
      ENDIF.
    ENDLOOP.

    SORT lt_shnumber.
    DELETE ADJACENT DUPLICATES FROM lt_shnumber.

    " Fetch OIGSI data (shipment to delivery mapping)
    IF lt_shnumber IS NOT INITIAL.
      SELECT shnumber doc_number
        FROM oigsi
        INTO TABLE mt_oigsi_data
        FOR ALL ENTRIES IN lt_shnumber
        WHERE shnumber = lt_shnumber-table_line.

      IF sy-subrc = 0.
        SORT mt_oigsi_data BY shnumber doc_number.
      ENDIF.
    ENDIF.

    " Collect delivery numbers
    LOOP AT mt_oigsi_data INTO lw_oigsi.
      APPEND lw_oigsi-doc_number TO lt_vbeln.
    ENDLOOP.

    SORT lt_vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_vbeln.

    " Fetch LIPS data (delivery to material mapping)
    IF lt_vbeln IS NOT INITIAL.
      SELECT vbeln matnr
        FROM lips
        INTO TABLE mt_lips_data
        FOR ALL ENTRIES IN lt_vbeln
        WHERE vbeln = lt_vbeln-table_line.

      IF sy-subrc = 0.
        SORT mt_lips_data BY vbeln.
      ENDIF.
    ENDIF.

    " Collect addresses
    LOOP AT ct_trip_legs INTO lw_leg WHERE leg_type = gc_leg_type_loaded.
      IF lw_leg-source_zone IS NOT INITIAL.
        " Get address from OIGSS
        READ TABLE mt_oigss_data INTO lw_oigss
          WITH KEY shnumber = lw_leg-shnumber
          BINARY SEARCH.
        IF sy-subrc = 0 AND lw_oigss-adrnr <> space.
          APPEND lw_oigss-adrnr TO lt_adrnr.
        ENDIF.
      ENDIF.
      IF lw_leg-dest_zone IS NOT INITIAL.
        READ TABLE mt_oigss_data INTO lw_oigss
          WITH KEY shnumber = lw_leg-shnumber
          BINARY SEARCH.
        IF sy-subrc = 0 AND lw_oigss-adrnz <> space.
          APPEND lw_oigss-adrnz TO lt_adrnr.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT lt_adrnr.
    DELETE ADJACENT DUPLICATES FROM lt_adrnr.

    " Fetch ADRC data (address to region mapping)
    IF lt_adrnr IS NOT INITIAL.
      SELECT addrnumber region
        FROM adrc
        INTO TABLE mt_adrc_data
        FOR ALL ENTRIES IN lt_adrnr
        WHERE addrnumber = lt_adrnr-table_line.

      IF sy-subrc = 0.
        SORT mt_adrc_data BY addrnumber.
      ENDIF.
    ENDIF.

    " Update legs with material and region
    FIELD-SYMBOLS: <lfs_leg> TYPE gty_trip_leg_output.

    LOOP AT ct_trip_legs ASSIGNING <lfs_leg> WHERE leg_type = gc_leg_type_loaded.
      " Get material
      IF <lfs_leg>-shnumber <> space.
        READ TABLE mt_oigsi_data INTO lw_oigsi
          WITH KEY shnumber = <lfs_leg>-shnumber
          BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE mt_lips_data INTO lw_lips
            WITH KEY vbeln = lw_oigsi-doc_number
            BINARY SEARCH.
          IF sy-subrc = 0.
            <lfs_leg>-matnr = lw_lips-matnr.
          ENDIF.
        ENDIF.
      ENDIF.

      " Get regions
      READ TABLE mt_oigss_data INTO lw_oigss
        WITH KEY shnumber = <lfs_leg>-shnumber
        BINARY SEARCH.
      IF sy-subrc = 0.
        " Source region
        IF lw_oigss-adrnr <> space.
          READ TABLE mt_adrc_data INTO lw_adrc
            WITH KEY addrnumber = lw_oigss-adrnr
            BINARY SEARCH.
          IF sy-subrc = 0.
            <lfs_leg>-source_region = lw_adrc-region.
          ENDIF.
        ENDIF.

        " Destination region
        IF lw_oigss-adrnz <> space.
          READ TABLE mt_adrc_data INTO lw_adrc
            WITH KEY addrnumber = lw_oigss-adrnz
            BINARY SEARCH.
          IF sy-subrc = 0.
            <lfs_leg>-dest_region = lw_adrc-region.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD determine_dest_exit_date.
    " BEGIN: Cursor Generated Code
    DATA: lw_yttstx0001 TYPE ty_yttstx0001_selected.

    " For SO: use DEST_DATE
    IF iv_mvt_type = gc_mvt_type_so.
      rv_exit_date = iv_dest_date.
    " For PO/STO: get from YTTSTX0001
    ELSEIF iv_mvt_type = gc_mvt_type_po OR iv_mvt_type = gc_mvt_type_sto.
      " Fetch YTTSTX0001 data if not already loaded
      IF mt_yttstx0001_data IS INITIAL.
        SELECT shnumber trk_purpos mg_exit_dt pp_entr_dt
          FROM yttstx0001
          INTO TABLE mt_yttstx0001_data
          WHERE trk_purpos = gc_yttstx0001_trk_purpos.

        IF sy-subrc = 0.
          SORT mt_yttstx0001_data BY shnumber.
        ENDIF.
      ENDIF.

      READ TABLE mt_yttstx0001_data INTO lw_yttstx0001
        WITH KEY shnumber = iv_shnumber
        BINARY SEARCH.
      IF sy-subrc = 0.
        rv_exit_date = lw_yttstx0001-mg_exit_dt.
      ELSE.
        rv_exit_date = iv_dest_date.
      ENDIF.
    ELSE.
      rv_exit_date = iv_dest_date.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD determine_source_ent_date.
    " BEGIN: Cursor Generated Code
    DATA: lw_yttstx0001 TYPE ty_yttstx0001_selected,
          lw_yttstx0002 TYPE ty_yttstx0002_selected,
          lv_report_no TYPE yttstx0002-report_no.

    " For PO: use SOURCE_DATE
    IF iv_mvt_type = gc_mvt_type_po.
      rv_ent_date = iv_source_date.
    " For SO/STO: get from YTTSTX0001 via YTTSTX0002
    ELSEIF iv_mvt_type = gc_mvt_type_so OR iv_mvt_type = gc_mvt_type_sto.
      " Fetch YTTSTX0002 data if not already loaded
      IF mt_yttstx0002_data IS INITIAL.
        SELECT shnumber report_no
          FROM yttstx0002
          INTO TABLE mt_yttstx0002_data.

        IF sy-subrc = 0.
          SORT mt_yttstx0002_data BY shnumber.
        ENDIF.
      ENDIF.

      READ TABLE mt_yttstx0002_data INTO lw_yttstx0002
        WITH KEY shnumber = iv_shnumber
        BINARY SEARCH.
      IF sy-subrc = 0.
        lv_report_no = lw_yttstx0002-report_no.

        " Fetch YTTSTX0001 data if not already loaded
        IF mt_yttstx0001_data IS INITIAL.
          SELECT shnumber trk_purpos mg_exit_dt pp_entr_dt
            FROM yttstx0001
            INTO TABLE mt_yttstx0001_data.

          IF sy-subrc = 0.
            SORT mt_yttstx0001_data BY shnumber.
          ENDIF.
        ENDIF.

        " For SO/STO: Get PP_ENTR_DT from YTTSTX0001 using shnumber
        " Note: BRD mentions using report_no from YTTSTX0002, but YTTSTX0001
        " is keyed by shnumber. Using shnumber directly for lookup.
        READ TABLE mt_yttstx0001_data INTO lw_yttstx0001
          WITH KEY shnumber = iv_shnumber
          BINARY SEARCH.
        IF sy-subrc = 0.
          rv_ent_date = lw_yttstx0001-pp_entr_dt.
        ELSE.
          " Fallback to source_date if not found
          rv_ent_date = iv_source_date.
        ENDIF.
      ELSE.
        rv_ent_date = iv_source_date.
      ENDIF.
    ELSE.
      rv_ent_date = iv_source_date.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD fetch_business_info.
    " BEGIN: Cursor Generated Code
    DATA: lv_business_id TYPE zsce_ctd_itm-business_id,
          lv_subbusiness_id TYPE zsce_ctd_itm-subbusiness_id.

    CLEAR: ev_business_id, ev_subbusiness_id.

    " Call function module
    CALL FUNCTION 'Z_SCM_GET_BUSINESS'
      EXPORTING
        iv_shnumber = iv_shnumber
      IMPORTING
        ev_business_id = lv_business_id
        ev_subbusiness_id = lv_subbusiness_id
      EXCEPTIONS
        error = 1
        OTHERS = 2.

    IF sy-subrc = 0.
      ev_business_id = lv_business_id.
      ev_subbusiness_id = lv_subbusiness_id.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD fetch_material.
    " BEGIN: Cursor Generated Code
    DATA: lw_oigsi TYPE ty_oigsi_selected,
          lw_lips TYPE ty_lips_selected.

    CLEAR rv_matnr.

    " Get delivery number from OIGSI
    READ TABLE mt_oigsi_data INTO lw_oigsi
      WITH KEY shnumber = iv_shnumber
      BINARY SEARCH.
    IF sy-subrc = 0.
      " Get material from LIPS
      READ TABLE mt_lips_data INTO lw_lips
        WITH KEY vbeln = lw_oigsi-doc_number
        BINARY SEARCH.
      IF sy-subrc = 0.
        rv_matnr = lw_lips-matnr.
      ENDIF.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD fetch_regions.
    " BEGIN: Cursor Generated Code
    DATA: lw_adrc TYPE ty_adrc_selected.

    CLEAR: ev_source_region, ev_dest_region.

    " Get source region
    IF iv_adrnr <> space.
      READ TABLE mt_adrc_data INTO lw_adrc
        WITH KEY addrnumber = iv_adrnr
        BINARY SEARCH.
      IF sy-subrc = 0.
        ev_source_region = lw_adrc-region.
      ENDIF.
    ENDIF.

    " Get destination region
    IF iv_adrnz <> space.
      READ TABLE mt_adrc_data INTO lw_adrc
        WITH KEY addrnumber = iv_adrnz
        BINARY SEARCH.
      IF sy-subrc = 0.
        ev_dest_region = lw_adrc-region.
      ENDIF.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD fetch_route_distance.
    " BEGIN: Cursor Generated Code
    DATA: lw_trolz TYPE ty_trolz_selected,
          lw_tvro TYPE ty_tvro_selected.

    CLEAR: ev_route, ev_distance, ev_found.

    " Fetch route from TROLZ (per BRD: VSBED='03', TRAGR='0006')
    " Note: TROLZ doesn't have zone fields, so we get route based on
    " shipping conditions. For empty legs, we use the first matching route.
    IF mt_trolz_data IS INITIAL.
      SELECT vsbed tragr route
        FROM trolz
        INTO TABLE mt_trolz_data
        WHERE vsbed = gc_trolz_vsbed
          AND tragr = gc_trolz_tragr.

      IF sy-subrc = 0.
        SORT mt_trolz_data BY vsbed tragr route.
      ENDIF.
    ENDIF.

    " Get route (use first match for empty legs)
    " Note: If multiple routes exist, this uses the first one.
    " Business logic may need adjustment if route selection is zone-specific.
    IF mt_trolz_data IS NOT INITIAL.
      READ TABLE mt_trolz_data INTO lw_trolz INDEX 1.
      IF sy-subrc = 0.
        ev_route = lw_trolz-route.

        " Fetch distance from TVRO
        IF mt_tvro_data IS INITIAL.
          SELECT route distance
            FROM tvro
            INTO TABLE mt_tvro_data.

          IF sy-subrc = 0.
            SORT mt_tvro_data BY route.
          ENDIF.
        ENDIF.

        " Get distance for the route
        IF mt_tvro_data IS NOT INITIAL.
          READ TABLE mt_tvro_data INTO lw_tvro
            WITH KEY route = ev_route
            BINARY SEARCH.
          IF sy-subrc = 0.
            ev_distance = lw_tvro-distance.
            ev_found = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD fetch_shipment_details.
    " BEGIN: Cursor Generated Code
    DATA: lw_oigss TYPE ty_oigss_selected.

    CLEAR: es_shipment_details, ev_found.

    READ TABLE mt_oigss_data INTO lw_oigss
      WITH KEY shnumber = iv_shnumber
      BINARY SEARCH.
    IF sy-subrc = 0.
      es_shipment_details-shnumber = lw_oigss-shnumber.
      es_shipment_details-route = lw_oigss-route.
      es_shipment_details-distance = lw_oigss-distance.
      es_shipment_details-source_zone = lw_oigss-source_zone.
      es_shipment_details-dest_zone = lw_oigss-dest_zone.
      es_shipment_details-adrnr = lw_oigss-adrnr.
      es_shipment_details-adrnz = lw_oigss-adrnz.
      ev_found = abap_true.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class: lcl_validator
*&---------------------------------------------------------------------*
CLASS lcl_validator DEFINITION.
  PUBLIC SECTION.
    "! Validate trip legs
    "! @parameter it_trip_legs | Trip Legs Input
    "! @parameter et_valid_trips | Valid Trip Headers
    "! @parameter et_invalid_trips | Invalid Trip Headers
    "! @parameter et_log | Log
    METHODS validate_trip_legs
      IMPORTING it_trip_legs TYPE gty_trip_leg_input_table
      EXPORTING et_valid_trips TYPE gty_trip_header_table
                et_invalid_trips TYPE gty_trip_header_table
                et_log TYPE gty_log_table.

    "! Validate empty legs
    "! @parameter it_trip_legs | Trip Legs
    "! @parameter et_valid_trips | Valid Trip Headers
    "! @parameter et_invalid_trips | Invalid Trip Headers
    "! @parameter et_log | Log
    METHODS validate_empty_legs
      IMPORTING it_trip_legs TYPE gty_trip_leg_output_table
      EXPORTING et_valid_trips TYPE gty_trip_header_table
                et_invalid_trips TYPE gty_trip_header_table
                et_log TYPE gty_log_table.

  PRIVATE SECTION.
    METHODS validate_leg_dates
      IMPORTING iv_source_date TYPE datum
                iv_dest_date TYPE datum
      RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.

CLASS lcl_validator IMPLEMENTATION.
  METHOD validate_trip_legs.
    " BEGIN: Cursor Generated Code
    DATA: lt_trip_no TYPE TABLE OF zsce_ctd_hdr-trip_no,
          lw_trip_no TYPE zsce_ctd_hdr-trip_no,
          lw_trip_leg TYPE gty_trip_leg_input,
          lw_trip_header TYPE gty_trip_header,
          lw_log TYPE gty_log,
          lv_timestamp TYPE timestamp,
          lv_trip_valid TYPE abap_bool.

    CLEAR: et_valid_trips, et_invalid_trips, et_log.
    GET TIME STAMP FIELD lv_timestamp.

    " Get unique trip numbers
    LOOP AT it_trip_legs INTO lw_trip_leg.
      APPEND lw_trip_leg-trip_no TO lt_trip_no.
    ENDLOOP.

    SORT lt_trip_no.
    DELETE ADJACENT DUPLICATES FROM lt_trip_no.

    " Validate each trip
    LOOP AT lt_trip_no INTO lw_trip_no.
      lv_trip_valid = abap_true.

      " Check all legs for this trip
      LOOP AT it_trip_legs INTO lw_trip_leg WHERE trip_no = lw_trip_no.
        " Validate dates
        IF validate_leg_dates(
             iv_source_date = lw_trip_leg-source_date
             iv_dest_date = lw_trip_leg-dest_date ) = abap_false.
          lv_trip_valid = abap_false.

          " Log error
          CLEAR lw_log.
          lw_log-trip_no = lw_trip_no.
          lw_log-counter = lw_trip_leg-counter.
          lw_log-msgty = 'E'.
          lw_log-msgid = 'ZCTD_MSG'.
          lw_log-msgno = '004'.
          lw_log-msgv1 = lw_trip_no.
          lw_log-timestamp = lv_timestamp.
          APPEND lw_log TO et_log.
        ENDIF.
      ENDLOOP.

      " Create trip header entry
      CLEAR lw_trip_header.
      lw_trip_header-trip_no = lw_trip_no.

      IF lv_trip_valid = abap_true.
        APPEND lw_trip_header TO et_valid_trips.
      ELSE.
        APPEND lw_trip_header TO et_invalid_trips.
      ENDIF.
    ENDLOOP.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD validate_empty_legs.
    " BEGIN: Cursor Generated Code
    DATA: lt_trip_no TYPE TABLE OF zsce_ctd_hdr-trip_no,
          lw_trip_no TYPE zsce_ctd_hdr-trip_no,
          lw_trip_leg TYPE gty_trip_leg_output,
          lw_trip_header TYPE gty_trip_header,
          lw_log TYPE gty_log,
          lv_timestamp TYPE timestamp,
          lv_trip_valid TYPE abap_bool.

    CLEAR: et_valid_trips, et_invalid_trips, et_log.
    GET TIME STAMP FIELD lv_timestamp.

    " Get unique trip numbers
    LOOP AT it_trip_legs INTO lw_trip_leg.
      APPEND lw_trip_leg-trip_no TO lt_trip_no.
    ENDLOOP.

    SORT lt_trip_no.
    DELETE ADJACENT DUPLICATES FROM lt_trip_no.

    " Validate each trip
    LOOP AT lt_trip_no INTO lw_trip_no.
      lv_trip_valid = abap_true.

      " Check all empty legs for this trip
      " Use ASSIGNING to modify actual table, not local copy
      FIELD-SYMBOLS: <lfs_trip_leg> TYPE gty_trip_leg_output.
      LOOP AT it_trip_legs ASSIGNING <lfs_trip_leg>
        WHERE trip_no = lw_trip_no
          AND leg_type = gc_leg_type_empty.

        " Validate route and distance
        IF <lfs_trip_leg>-route IS INITIAL OR <lfs_trip_leg>-distance IS INITIAL.
          lv_trip_valid = abap_false.

          " Set remarks in actual table
          <lfs_trip_leg>-ctd_ruleeng_remarks = 'Empty Leg Route / Distance missing'.
          <lfs_trip_leg>-validation_error = abap_true.

          " Log error
          CLEAR lw_log.
          lw_log-trip_no = lw_trip_no.
          lw_log-counter = <lfs_trip_leg>-counter.
          lw_log-msgty = 'E'.
          lw_log-msgid = 'ZCTD_MSG'.
          lw_log-msgno = '005'.
          lw_log-msgv1 = lw_trip_no.
          lw_log-timestamp = lv_timestamp.
          APPEND lw_log TO et_log.
        ENDIF.
      ENDLOOP.

      " Create trip header entry
      CLEAR lw_trip_header.
      lw_trip_header-trip_no = lw_trip_no.

      IF lv_trip_valid = abap_true.
        APPEND lw_trip_header TO et_valid_trips.
      ELSE.
        APPEND lw_trip_header TO et_invalid_trips.
      ENDIF.
    ENDLOOP.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD validate_leg_dates.
    " BEGIN: Cursor Generated Code
    IF iv_source_date IS INITIAL OR iv_dest_date IS INITIAL.
      rv_valid = abap_false.
    ELSE.
      rv_valid = abap_true.
    ENDIF.
    " END: Cursor Generated Code
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class: lcl_alv_display
*&---------------------------------------------------------------------*
CLASS lcl_alv_display DEFINITION.
  PUBLIC SECTION.
    "! Constructor
    "! @parameter io_container | Container for ALV
    METHODS constructor
      IMPORTING io_container TYPE REF TO cl_gui_container.

    "! Display ALV
    "! @parameter it_data | Data to Display
    METHODS display
      IMPORTING it_data TYPE gty_trip_leg_output_table.

  PRIVATE SECTION.
    DATA: mo_alv_grid TYPE REF TO cl_gui_alv_grid,
          mo_container TYPE REF TO cl_gui_container,
          mt_fieldcat TYPE lvc_t_fcat,
          mt_output TYPE gty_trip_leg_output_table,
          ms_layout TYPE lvc_s_layo.

    METHODS: build_field_catalog,
            apply_color_coding,
            set_layout.
ENDCLASS.

CLASS lcl_alv_display IMPLEMENTATION.
  METHOD constructor.
    mo_container = io_container.

    " Create ALV grid
    CREATE OBJECT mo_alv_grid
      EXPORTING
        i_parent = mo_container.
  ENDMETHOD.

  METHOD display.
    " BEGIN: Cursor Generated Code
    mt_output = it_data.

    " Build field catalog
    CALL METHOD build_field_catalog.

    " Set layout
    CALL METHOD set_layout.

    " Display ALV
    CALL METHOD mo_alv_grid->set_table_for_first_display
      EXPORTING
        is_layout = ms_layout
      CHANGING
        it_outtab = mt_output
        it_fieldcatalog = mt_fieldcat.

    " Apply color coding
    CALL METHOD apply_color_coding.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD build_field_catalog.
    " BEGIN: Cursor Generated Code
    DATA: lw_fieldcat TYPE lvc_s_fcat.

    CLEAR mt_fieldcat.

    " LIFNR
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'LIFNR'.
    lw_fieldcat-scrtext_l = 'Vendor'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " TRUCK_NO
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'TRUCK_NO'.
    lw_fieldcat-scrtext_l = 'Truck Number'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " TRIP_NO
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'TRIP_NO'.
    lw_fieldcat-scrtext_l = 'Trip Number'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " COUNTER
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'COUNTER'.
    lw_fieldcat-scrtext_l = 'Leg Counter'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " LEG_TYPE
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'LEG_TYPE'.
    lw_fieldcat-scrtext_l = 'Leg Type'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " SHNUMBER
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'SHNUMBER'.
    lw_fieldcat-scrtext_l = 'Shipment Number'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " SOURCE_DATE
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'SOURCE_DATE'.
    lw_fieldcat-scrtext_l = 'Source Date'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " DEST_DATE
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'DEST_DATE'.
    lw_fieldcat-scrtext_l = 'Destination Date'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " SOURCE_ZONE
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'SOURCE_ZONE'.
    lw_fieldcat-scrtext_l = 'Source Zone'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " DEST_ZONE
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'DEST_ZONE'.
    lw_fieldcat-scrtext_l = 'Destination Zone'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " ROUTE
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'ROUTE'.
    lw_fieldcat-scrtext_l = 'Route'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " DISTANCE - Right justified
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'DISTANCE'.
    lw_fieldcat-scrtext_l = 'Distance'.
    lw_fieldcat-just = 'R'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " MATNR
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'MATNR'.
    lw_fieldcat-scrtext_l = 'Material Number'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " SOURCE_REGION
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'SOURCE_REGION'.
    lw_fieldcat-scrtext_l = 'Source Region'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " DEST_REGION
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'DEST_REGION'.
    lw_fieldcat-scrtext_l = 'Destination Region'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " BUSINESS_ID
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'BUSINESS_ID'.
    lw_fieldcat-scrtext_l = 'Business ID'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " SUBBUSINESS_ID
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'SUBBUSINESS_ID'.
    lw_fieldcat-scrtext_l = 'Sub Business ID'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " TRIP_STATUS
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'TRIP_STATUS'.
    lw_fieldcat-scrtext_l = 'Trip Status'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.

    " CTD_RULEENG_REMARKS
    CLEAR lw_fieldcat.
    lw_fieldcat-fieldname = 'CTD_RULEENG_REMARKS'.
    lw_fieldcat-scrtext_l = 'Remarks'.
    lw_fieldcat-just = 'L'.
    APPEND lw_fieldcat TO mt_fieldcat.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD apply_color_coding.
    " BEGIN: Cursor Generated Code
    DATA: lt_color TYPE lvc_t_scol,
          lw_color TYPE lvc_s_scol,
          lv_index TYPE i.

    FIELD-SYMBOLS: <lfs_output> TYPE gty_trip_leg_output.

    LOOP AT mt_output ASSIGNING <lfs_output>
      WHERE validation_error = abap_true.

      CLEAR lt_color.
      CLEAR lw_color.
      lw_color-fname = 'LEG_TYPE'.
      lw_color-color-col = 6.  " Red
      lw_color-color-int = 0.
      lw_color-color-inv = 0.
      APPEND lw_color TO lt_color.

      " Calculate row index
      lv_index = sy-tabix.

      " Set color for row
      CALL METHOD mo_alv_grid->set_row_color
        EXPORTING
          it_color = lt_color
          iv_row = lv_index.
    ENDLOOP.
    " END: Cursor Generated Code
  ENDMETHOD.

  METHOD set_layout.
    " BEGIN: Cursor Generated Code
    CLEAR ms_layout.
    ms_layout-zebra = abap_true.
    ms_layout-cwidth_opt = abap_true.
    ms_layout-sel_mode = 'A'.
    " END: Cursor Generated Code
  ENDMETHOD.
ENDCLASS.

