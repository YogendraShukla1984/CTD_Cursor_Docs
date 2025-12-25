*&---------------------------------------------------------------------*
*& Function Module  ZSCM_CTD_GETTRIPHDR
*&---------------------------------------------------------------------*
*& Purpose: Get CTD Trip Header Details with Role-Based Filtering
*& 
*& This Function Module retrieves CTD Trip Header details from SAP
*& based on user search criteria. It supports both Transporter/Vendor
*& users (external portal) and RIL Operations users (internal users)
*& by applying role-based data filtering and authorization logic.
*&
*& Author: [Author Name]
*& Creation Date: [Date]
*& Change History:
*& Date       User    Description
*& DD.MM.YYYY USERID  Initial creation
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code
FUNCTION zscm_ctd_gettriphdr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_USER_TYPE) TYPE CHAR1
*"     VALUE(IV_FROM_DATE) TYPE DATS
*"     VALUE(IV_TO_DATE) TYPE DATS
*"     VALUE(IV_USER_ID) TYPE CHAR10
*"     VALUE(IV_TRUCK_NO) TYPE CHAR20 OPTIONAL
*"  EXPORTING
*"     VALUE(ET_TRIP_HDR) TYPE GTY_TRIP_HDR_TABLE
*"  EXCEPTIONS
*"     INVALID_USER_TYPE
*"     INVALID_DATE_RANGE
*"     INVALID_USER_ID
*"     INVALID_TRUCK_NO
*"     NO_AUTHORITY
*"     DATABASE_ERROR
*"----------------------------------------------------------------------

  " Local data declarations (all at beginning)
  DATA: lt_trip_hdr TYPE gty_trip_hdr_table,
        lt_business_auth TYPE TABLE OF char10,
        lt_subbusiness_auth TYPE TABLE OF char10,
        lo_exception TYPE REF TO cx_root.

  " Clear output table
  CLEAR: et_trip_hdr.

  " Step 1: Input Validation
  " Validate User Type
  IF iv_user_type IS INITIAL.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '001'
      WITH 'User type is mandatory'
      RAISING invalid_user_type.
  ENDIF.

  IF iv_user_type <> gc_user_type_vendor AND
     iv_user_type <> gc_user_type_ril.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '001'
      WITH 'Invalid user type. Must be V or R'
      RAISING invalid_user_type.
  ENDIF.

  " Validate Date Range
  IF iv_from_date IS INITIAL OR iv_to_date IS INITIAL.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '002'
      WITH 'Date range is mandatory'
      RAISING invalid_date_range.
  ENDIF.

  IF iv_from_date > iv_to_date.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '002'
      WITH 'From date must be <= To date'
      RAISING invalid_date_range.
  ENDIF.

  " Validate User ID
  IF iv_user_id IS INITIAL.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '003'
      WITH 'User ID is mandatory'
      RAISING invalid_user_id.
  ENDIF.

  " Validate Truck Number (if provided)
  " Note: IS NOT INITIAL check is sufficient for optional parameter
  " Additional format validation can be added if required

  " Step 2: Authorization Check
  " RFC Authorization Check (mandatory for RFC-enabled FM)
  AUTHORITY-CHECK OBJECT 'S_RFC'
    ID 'RFC_TYPE' FIELD 'FUNC'
    ID 'RFC_NAME' FIELD 'ZSCM_CTD_GETTRIPHDR'
    ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '005'
      WITH 'No RFC authorization'
      RAISING no_authority.
  ENDIF.

  " Step 3: Get Authorization Data (for RIL Operations users)
  IF iv_user_type = gc_user_type_ril.
    " For RIL Operations, retrieve authorized Business/Sub-Business
    " TODO: Implement Business/Sub-Business authorization retrieval
    " This may require custom FM or table lookup based on actual
    " authorization mechanism in the system
    " Example placeholder:
    " CALL FUNCTION 'Z_GET_AUTH_BUSINESS'
    "   EXPORTING
    "     iv_user = iv_user_id
    "   IMPORTING
    "     et_business = lt_business_auth
    "     et_subbusiness = lt_subbusiness_auth.
    "
    " IF lt_business_auth IS INITIAL.
    "   " No authorization, return empty table (not an error)
    "   CLEAR: et_trip_hdr.
    "   RETURN.
    " ENDIF.
    "
    " For now, if authorization retrieval is not implemented,
    " user will get empty results (safe default)
    CLEAR: lt_business_auth.
    CLEAR: lt_subbusiness_auth.
  ENDIF.

  " Step 4: Execute SELECT based on user type
  TRY.
      IF iv_user_type = gc_user_type_vendor.
        " Vendor user: Filter by LIFNR (Vendor Number)
        IF iv_truck_no IS NOT INITIAL.
          " With truck number filter
          SELECT trip_no lifnr truck_no trip_status
                 created_date created_time created_by
                 business subbusiness
            FROM zsce_ctd_hdr
            INTO TABLE lt_trip_hdr
            WHERE created_date BETWEEN iv_from_date AND iv_to_date
              AND trip_status IN (gc_status_pending, gc_status_rejected)
              AND lifnr = iv_user_id
              AND truck_no = iv_truck_no.
        ELSE.
          " Without truck number filter
          SELECT trip_no lifnr truck_no trip_status
                 created_date created_time created_by
                 business subbusiness
            FROM zsce_ctd_hdr
            INTO TABLE lt_trip_hdr
            WHERE created_date BETWEEN iv_from_date AND iv_to_date
              AND trip_status IN (gc_status_pending, gc_status_rejected)
              AND lifnr = iv_user_id.
        ENDIF.

      ELSEIF iv_user_type = gc_user_type_ril.
        " RIL Operations: Filter by authorized Business/Sub-Business
        IF lt_business_auth IS INITIAL.
          " No authorization, return empty table (not an error)
          CLEAR: et_trip_hdr.
          RETURN.
        ENDIF.

        IF iv_truck_no IS NOT INITIAL.
          " With truck number filter
          SELECT trip_no lifnr truck_no trip_status
                 created_date created_time created_by
                 business subbusiness
            FROM zsce_ctd_hdr
            INTO TABLE lt_trip_hdr
            WHERE created_date BETWEEN iv_from_date AND iv_to_date
              AND trip_status IN (gc_status_pending, gc_status_rejected)
              AND business IN lt_business_auth
              AND subbusiness IN lt_subbusiness_auth
              AND truck_no = iv_truck_no.
        ELSE.
          " Without truck number filter
          SELECT trip_no lifnr truck_no trip_status
                 created_date created_time created_by
                 business subbusiness
            FROM zsce_ctd_hdr
            INTO TABLE lt_trip_hdr
            WHERE created_date BETWEEN iv_from_date AND iv_to_date
              AND trip_status IN (gc_status_pending, gc_status_rejected)
              AND business IN lt_business_auth
              AND subbusiness IN lt_subbusiness_auth.
        ENDIF.
      ENDIF.

      " Check SY-SUBRC after SELECT
      " sy-subrc = 0: Records found
      " sy-subrc = 4: No records found (valid scenario, not an error)
      " sy-subrc <> 0 and <> 4: Database error
      IF sy-subrc <> 0 AND sy-subrc <> 4.
        MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '006'
          WITH 'Database error occurred'
          RAISING database_error.
      ENDIF.

    CATCH cx_root INTO lo_exception.
      " Handle any unexpected exceptions
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '006'
        WITH 'Database error occurred'
        RAISING database_error.
  ENDTRY.

  " Step 5: Populate output table
  et_trip_hdr = lt_trip_hdr.

  " Cleanup
  CLEAR: lt_trip_hdr.
  CLEAR: lt_business_auth.
  CLEAR: lt_subbusiness_auth.

ENDFUNCTION.
" END: Cursor Generated Code

