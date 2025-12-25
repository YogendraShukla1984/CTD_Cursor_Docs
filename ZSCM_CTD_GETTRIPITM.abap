*&---------------------------------------------------------------------*
*& Function Module  ZSCM_CTD_GETTRIPITM
*&---------------------------------------------------------------------*
*& Purpose: Get CTD Trip Item/Leg Details with Role-Based Filtering
*& 
*& This Function Module retrieves CTD Trip Item/Leg details from SAP
*& for a specific trip. It supports both Transporter/Vendor users
*& (external portal) and RIL Operations users (internal users) by
*& applying role-based data filtering and authorization logic.
*&
*& Author: [Author Name]
*& Creation Date: [Date]
*& Change History:
*& Date       User    Description
*& DD.MM.YYYY USERID  Initial creation
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code
FUNCTION zscm_ctd_gettripitm.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRIP_NO) TYPE CHAR14
*"     VALUE(IV_USER_TYPE) TYPE CHAR1
*"     VALUE(IV_USER_ID) TYPE CHAR12
*"  EXPORTING
*"     VALUE(ET_TRIP_ITM) TYPE GTY_TRIP_ITM_TABLE
*"  EXCEPTIONS
*"     TRIP_NOT_FOUND
*"     INVALID_USER_TYPE
*"     INVALID_USER_ID
*"     UNAUTHORIZED_ACCESS
*"     NO_AUTHORITY
*"     DATABASE_ERROR
*"----------------------------------------------------------------------

  " Local data declarations (all at beginning)
  DATA: lt_trip_itm TYPE gty_trip_itm_table,
        lw_trip_hdr TYPE gty_trip_hdr_auth,
        lt_business_auth TYPE TABLE OF char10,
        lt_subbusiness_auth TYPE TABLE OF char10,
        lo_exception TYPE REF TO cx_root.

  " Clear output table
  CLEAR: et_trip_itm.

  " Step 1: Input Validation
  " Validate Trip Number
  IF iv_trip_no IS INITIAL.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '001'
      WITH 'Trip number is mandatory'
      RAISING trip_not_found.
  ENDIF.

  " Validate User Type
  IF iv_user_type IS INITIAL.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '002'
      WITH 'User type is mandatory'
      RAISING invalid_user_type.
  ENDIF.

  IF iv_user_type <> gc_user_type_vendor AND
     iv_user_type <> gc_user_type_ril.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '002'
      WITH 'Invalid user type. Must be V or R'
      RAISING invalid_user_type.
  ENDIF.

  " Validate User ID
  IF iv_user_id IS INITIAL.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '003'
      WITH 'User ID is mandatory'
      RAISING invalid_user_id.
  ENDIF.

  " Step 2: Verify Trip Exists and Get Header Data
  SELECT SINGLE trip_no lifnr business subbusiness
    FROM zsce_ctd_hdr
    INTO lw_trip_hdr
    WHERE trip_no = iv_trip_no.

  IF sy-subrc <> 0.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '001'
      WITH 'Trip number not found'
      RAISING trip_not_found.
  ENDIF.

  " Step 3: Authorization Check
  " RFC Authorization Check (mandatory for RFC-enabled FM)
  AUTHORITY-CHECK OBJECT 'S_RFC'
    ID 'RFC_TYPE' FIELD 'FUNC'
    ID 'RFC_NAME' FIELD 'ZSCM_CTD_GETTRIPITM'
    ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '005'
      WITH 'No RFC authorization'
      RAISING no_authority.
  ENDIF.

  " Role-based Authorization
  IF iv_user_type = gc_user_type_vendor.
    " For vendor users, verify trip belongs to vendor
    IF lw_trip_hdr-lifnr <> iv_user_id.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '004'
        WITH 'User does not have access to this trip'
        RAISING unauthorized_access.
    ENDIF.

  ELSEIF iv_user_type = gc_user_type_ril.
    " For RIL Operations, check Business/Sub-Business authorization
    " Retrieve authorized Business codes for user
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
    "   MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '005'
    "     WITH 'No Business authorization'
    "     RAISING no_authority.
    " ENDIF.
    "
    " " Check if trip's business is authorized
    " SORT lt_business_auth.
    " READ TABLE lt_business_auth WITH KEY table_line = lw_trip_hdr-business
    "   TRANSPORTING NO FIELDS
    "   BINARY SEARCH.
    " IF sy-subrc <> 0.
    "   MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '005'
    "     WITH 'No authorization for this trip'
    "     RAISING no_authority.
    " ENDIF.
    "
    " " Check if trip's subbusiness is authorized
    " SORT lt_subbusiness_auth.
    " READ TABLE lt_subbusiness_auth WITH KEY table_line = lw_trip_hdr-subbusiness
    "   TRANSPORTING NO FIELDS
    "   BINARY SEARCH.
    " IF sy-subrc <> 0.
    "   MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '005'
    "     WITH 'No authorization for this trip'
    "     RAISING no_authority.
    " ENDIF.
    
    " For now, if authorization retrieval is not implemented,
    " user will get authorization error (safe default)
    CLEAR: lt_business_auth.
    CLEAR: lt_subbusiness_auth.
  ENDIF.

  " Step 4: Fetch Item/Leg Records
  TRY.
      SELECT trip_no counter lifnr truck_no shnumber
             source_date dest_date mvt_type area
             adrnr adrnz source_zone dest_zone
             route distance matnr
             source_region dest_region
             business_id subbusiness_id
             dest_exit_date source_ent_date
             ctd_ruleeng_remarks
        FROM zsce_ctd_itm
        INTO TABLE lt_trip_itm
        WHERE trip_no = iv_trip_no.

      " Check SELECT result
      " sy-subrc = 0: Records found
      " sy-subrc = 4: No records found (valid scenario, not an error)
      " sy-subrc <> 0 and <> 4: Database error
      IF sy-subrc <> 0 AND sy-subrc <> 4.
        MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '006'
          WITH 'Database error occurred'
          RAISING database_error.
      ENDIF.

      " Step 5: Sort Records by COUNTER (ascending) to maintain leg sequence
      IF lt_trip_itm IS NOT INITIAL.
        SORT lt_trip_itm BY counter.
      ENDIF.

    CATCH cx_root INTO lo_exception.
      " Handle any unexpected exceptions
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '006'
        WITH 'Database error occurred'
        RAISING database_error.
  ENDTRY.

  " Step 6: Populate output table
  et_trip_itm = lt_trip_itm.

  " Cleanup
  CLEAR: lt_trip_itm.
  CLEAR: lw_trip_hdr.
  CLEAR: lt_business_auth.
  CLEAR: lt_subbusiness_auth.

ENDFUNCTION.
" END: Cursor Generated Code

