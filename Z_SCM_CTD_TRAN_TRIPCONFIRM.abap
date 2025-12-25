*&---------------------------------------------------------------------*
*& Function Module  Z_SCM_CTD_TRAN_TRIPCONFIRM
*&---------------------------------------------------------------------*
*& Purpose: Transporter Trip Details Confirm
*& 
*& This Function Module enables Transporter (Vendor) users to confirm
*& CTD trip details. It allows vendors to update source and destination
*& dates for empty legs, enter vendor remarks, and confirm the trip.
*& Upon confirmation, the trip status is updated to '05' and the trip
*& becomes visible to RIL Operations.
*&
*& Author: [Author Name]
*& Creation Date: [Date]
*& Change History:
*& Date       User    Description
*& DD.MM.YYYY USERID  Initial creation
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code
FUNCTION z_scm_ctd_tran_tripconfirm.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRIP_NO) TYPE CHAR14
*"     VALUE(IT_LEG_UPDATE) TYPE GTY_LEG_UPDATE_TABLE
*"  EXPORTING
*"     VALUE(EV_STATUS) TYPE CHAR1
*"     VALUE(EV_MESSAGE) TYPE STRING
*"     VALUE(ET_RETURN) TYPE BAPIRET2_T
*"  EXCEPTIONS
*"     TRIP_NOT_FOUND
*"     INVALID_TRIP_STATUS
*"     ALREADY_CONFIRMED
*"     UNAUTHORIZED_ACCESS
*"     NO_LEG_UPDATES
*"     INVALID_LEG_TYPE
*"     LEG_NOT_FOUND
*"     LOADED_LEG_UPDATE_NOT_ALLOWED
*"     INVALID_DATE_RANGE
*"     TRIP_NO_MISMATCH
*"     COMMIT_ERROR
*"     DATABASE_ERROR
*"----------------------------------------------------------------------

  " Local data declarations (all at beginning)
  DATA: lw_trip_hdr TYPE gty_trip_hdr_confirm,
        lt_leg_validate TYPE gty_leg_validate_table,
        lw_leg_validate TYPE gty_leg_validate,
        lw_leg_update TYPE gty_leg_update,
        lv_vendor_no TYPE lifnr,
        lo_exception TYPE REF TO cx_root,
        lv_message TYPE string,
        lw_return TYPE bapiret2.

  " Initialize output
  CLEAR: ev_status, ev_message, et_return.
  ev_status = gc_status_error.

  " Step 1: Input Validation
  " Validate Trip Number
  IF iv_trip_no IS INITIAL.
    ev_status = gc_status_error.
    ev_message = 'Trip number is mandatory'.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '001'
      WITH 'Trip number is mandatory'
      RAISING trip_not_found.
  ENDIF.

  " Validate Leg Update Table
  IF it_leg_update IS INITIAL.
    ev_status = gc_status_error.
    ev_message = 'No leg updates provided'.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '002'
      WITH 'No leg updates provided'
      RAISING no_leg_updates.
  ENDIF.

  " Validate each record in IT_LEG_UPDATE
  LOOP AT it_leg_update INTO lw_leg_update.
    " Validate trip number consistency
    IF lw_leg_update-trip_no <> iv_trip_no.
      ev_status = gc_status_error.
      ev_message = 'Trip number mismatch in leg update table'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '003'
        WITH 'Trip number mismatch'
        RAISING trip_no_mismatch.
    ENDIF.

    " Validate leg type
    IF lw_leg_update-leg_type <> gc_leg_type_empty.
      ev_status = gc_status_error.
      ev_message = 'Invalid leg type. Only empty legs can be updated'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '004'
        WITH 'Invalid leg type'
        RAISING invalid_leg_type.
    ENDIF.

    " Validate date range
    IF lw_leg_update-source_date > lw_leg_update-destination_date.
      ev_status = gc_status_error.
      ev_message = 'Source date must be <= Destination date'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '005'
        WITH 'Invalid date range'
        RAISING invalid_date_range.
    ENDIF.
  ENDLOOP.

  " Step 2: Verify Trip Exists and Get Header Data
  SELECT SINGLE trip_no lifnr trip_status
    FROM zsce_ctd_hdr
    INTO lw_trip_hdr
    WHERE trip_no = iv_trip_no.

  IF sy-subrc <> 0.
    ev_status = gc_status_error.
    ev_message = 'Trip number not found'.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '001'
      WITH 'Trip number not found'
      RAISING trip_not_found.
  ENDIF.

  " Step 3: Authorization Check
  " RFC Authorization Check (mandatory for RFC-enabled FM)
  AUTHORITY-CHECK OBJECT 'S_RFC'
    ID 'RFC_TYPE' FIELD 'FUNC'
    ID 'RFC_NAME' FIELD 'Z_SCM_CTD_TRAN_TRIPCONFIRM'
    ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    ev_status = gc_status_error.
    ev_message = 'No RFC authorization'.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '006'
      WITH 'No RFC authorization'
      RAISING unauthorized_access.
  ENDIF.

  " Get current user's vendor number
  " TODO: Implement vendor number retrieval from user context
  " This may require custom FM or user master data lookup
  " Example:
  " CALL FUNCTION 'Z_GET_USER_VENDOR'
  "   EXPORTING
  "     iv_user = sy-uname
  "   IMPORTING
  "     ev_vendor = lv_vendor_no.
  "
  " For now, placeholder - to be implemented based on actual user context
  " lv_vendor_no = sy-uname.  " Placeholder - adjust based on actual implementation
  "
  " Note: If vendor number is passed as parameter or retrieved differently,
  " update this section accordingly

  " Verify trip belongs to vendor
  IF lw_trip_hdr-lifnr <> lv_vendor_no.
    ev_status = gc_status_error.
    ev_message = 'User does not have access to this trip'.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '007'
      WITH 'Unauthorized access'
      RAISING unauthorized_access.
  ENDIF.

  " Step 4: Trip Status Validation
  IF lw_trip_hdr-trip_status <> gc_status_pending.
    IF lw_trip_hdr-trip_status = gc_status_confirmed.
      ev_status = gc_status_error.
      ev_message = 'Trip has already been confirmed'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '008'
        WITH 'Trip already confirmed'
        RAISING already_confirmed.
    ELSE.
      ev_status = gc_status_error.
      ev_message = 'Trip status is not valid for confirmation'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '009'
        WITH 'Invalid trip status'
        RAISING invalid_trip_status.
    ENDIF.
  ENDIF.

  " Step 5: Validate Leg Updates
  " Fetch existing legs for validation
  SELECT trip_no counter leg_type
    FROM zsce_ctd_itm
    INTO TABLE lt_leg_validate
    WHERE trip_no = iv_trip_no.

  IF sy-subrc <> 0.
    " No legs found - this is valid (trip may have no legs yet)
    " But if IT_LEG_UPDATE is not empty, it's an error
    IF it_leg_update IS NOT INITIAL.
      ev_status = gc_status_error.
      ev_message = 'Legs not found in system'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '010'
        WITH 'Legs not found'
        RAISING leg_not_found.
    ENDIF.
  ENDIF.

  " Validate each leg update
  LOOP AT it_leg_update INTO lw_leg_update.
    " Check if leg exists
    READ TABLE lt_leg_validate INTO lw_leg_validate
      WITH KEY trip_no = lw_leg_update-trip_no
               counter = lw_leg_update-counter.
    
    IF sy-subrc <> 0.
      ev_status = gc_status_error.
      CONCATENATE 'Leg not found: Trip' lw_leg_update-trip_no
                  'Counter' lw_leg_update-counter
        INTO lv_message SEPARATED BY space.
      ev_message = lv_message.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '010'
        WITH 'Leg not found'
        RAISING leg_not_found.
    ENDIF.

    " Check if leg is empty (in database)
    IF lw_leg_validate-leg_type <> gc_leg_type_empty.
      ev_status = gc_status_error.
      CONCATENATE 'Loaded leg cannot be updated: Counter' lw_leg_update-counter
        INTO lv_message SEPARATED BY space.
      ev_message = lv_message.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '011'
        WITH 'Loaded leg update not allowed'
        RAISING loaded_leg_update_not_allowed.
    ENDIF.
  ENDLOOP.

  " Step 6: Update Empty Leg Details
  TRY.
      LOOP AT it_leg_update INTO lw_leg_update.
        " Update source date, destination date, and vendor remarks
        UPDATE zsce_ctd_itm
          SET source_date = lw_leg_update-source_date
              dest_date = lw_leg_update-destination_date
              vendor_remarks = lw_leg_update-vendor_remarks
          WHERE trip_no = lw_leg_update-trip_no
            AND counter = lw_leg_update-counter.

        " Check SY-SUBRC after UPDATE
        IF sy-subrc <> 0.
          ROLLBACK WORK.
          ev_status = gc_status_error.
          CONCATENATE 'Error updating leg: Counter' lw_leg_update-counter
            INTO lv_message SEPARATED BY space.
          ev_message = lv_message.
          MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '012'
            WITH 'Database error updating leg'
            RAISING database_error.
        ENDIF.
      ENDLOOP.

    CATCH cx_root INTO lo_exception.
      ROLLBACK WORK.
      ev_status = gc_status_error.
      ev_message = 'Database error occurred'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '012'
        WITH 'Database error occurred'
        RAISING database_error.
  ENDTRY.

  " Step 7: Update Trip Header
  TRY.
      " Update trip header status and confirmation info
      " Note: Field names (confirm_date, confirm_time, confirm_by) to be
      " confirmed based on actual ZSCE_CTD_HDR table structure
      UPDATE zsce_ctd_hdr
        SET trip_status = gc_status_confirmed
            confirm_date = sy-datum
            confirm_time = sy-uzeit
            confirm_by = sy-uname
        WHERE trip_no = iv_trip_no.

      " Check SY-SUBRC after UPDATE
      IF sy-subrc <> 0.
        ROLLBACK WORK.
        ev_status = gc_status_error.
        ev_message = 'Error updating trip header'.
        MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '012'
          WITH 'Database error updating trip header'
          RAISING database_error.
      ENDIF.

    CATCH cx_root INTO lo_exception.
      ROLLBACK WORK.
      ev_status = gc_status_error.
      ev_message = 'Database error occurred'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '012'
        WITH 'Database error occurred'
        RAISING database_error.
  ENDTRY.

  " Step 8: Commit Transaction
  TRY.
      COMMIT WORK AND WAIT.

    CATCH cx_root INTO lo_exception.
      ROLLBACK WORK.
      ev_status = gc_status_error.
      ev_message = 'Error committing transaction'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '013'
        WITH 'Commit error'
        RAISING commit_error.
  ENDTRY.

  " Step 9: Populate Output
  ev_status = gc_status_success.
  ev_message = 'Trip & Empty Leg details confirmed successfully.'.

  " Populate return table with success message
  CLEAR: lw_return.
  lw_return-type = 'S'.
  lw_return-id = 'ZCTD'.
  lw_return-number = '014'.
  lw_return-message_v1 = iv_trip_no.
  lw_return-message = 'Trip confirmed successfully'.
  APPEND lw_return TO et_return.

  " Cleanup
  CLEAR: lw_trip_hdr.
  CLEAR: lt_leg_validate.
  CLEAR: lw_leg_validate.
  CLEAR: lw_leg_update.
  CLEAR: lv_vendor_no.
  CLEAR: lv_message.
  CLEAR: lw_return.

ENDFUNCTION.
" END: Cursor Generated Code

