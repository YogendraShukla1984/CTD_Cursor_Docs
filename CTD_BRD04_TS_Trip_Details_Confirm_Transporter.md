# Technical Specification
## Transporter Trip Details Confirm (CTD)

---

**Document Information**

| Field | Value |
|-------|-------|
| **Document Type** | Technical Specification |
| **Function Module Name** | Z_SCM_CTD_TRAN_TRIPCONFIRM |
| **Function Group** | ZSCM_CTD (or appropriate function group) |
| **Version** | 1.0 |
| **Date** | [Current Date] |
| **Author** | [Author Name] |
| **Target System** | SAP ECC 6.0 / NetWeaver 7.31 |
| **Syntax Level** | abap_731 |
| **Related FS** | CTD_BRD04_FS_Trip_Details_Confirm_Transporter |

---

## 1. Technical Overview

### 1.1 System Requirements
- **SAP Version:** ECC 6.0 / NetWeaver 7.31
- **Syntax Level:** abap_731 (strict compatibility required)
- **Database:** SAP HANA or AnyDB
- **Function Module Type:** Remote-Enabled Function Module (RFC-enabled)
- **Update Type:** Update Module (performs database updates)

### 1.2 Architecture
- **Design Pattern:** Procedural Function Module (appropriate for RFC/reusable logic)
- **Database Access:** Direct OpenSQL UPDATE statements
- **Authorization:** Built-in authorization checks
- **Error Handling:** Exception-based error handling with rollback
- **Transaction Management:** Single LUW with COMMIT WORK

### 1.3 Coding Standards Compliance
- Follows ABAP Code Rules (NetWeaver 7.31 compatible)
- No inline declarations (DATA() syntax forbidden)
- No constructor operators (NEW, VALUE, CORRESPONDING forbidden)
- No string templates (|text| syntax forbidden)
- No table expressions (itab[key] syntax forbidden)
- All variables declared upfront in DATA section
- Classic OpenSQL syntax (no host variables @)
- Proper SY-SUBRC checks after all database operations
- Mandatory authorization checks
- Proper exception handling with rollback
- UPDATE statements with proper WHERE clauses

---

## 2. Function Module Structure

### 2.1 Function Module Attributes

| Attribute | Value |
|-----------|-------|
| **Name** | Z_SCM_CTD_TRAN_TRIPCONFIRM |
| **Function Group** | ZSCM_CTD |
| **Remote-Enabled** | Yes (RFC-enabled) |
| **Update Module** | No (Normal Function Module with COMMIT) |
| **Short Text** | Transporter Trip Details Confirm |

### 2.2 Function Module Interface

#### 2.2.1 Import Parameters

```abap
IMPORTING
  VALUE(IV_TRIP_NO) TYPE CHAR14              " Trip Number
  VALUE(IT_LEG_UPDATE) TYPE gty_leg_update_table  " Leg update table
```

**Note:** `gty_leg_update_table` must be defined in TOP include or type pool as a specific table type (not generic TABLE).

#### 2.2.2 Export Parameters

```abap
EXPORTING
  VALUE(EV_STATUS) TYPE CHAR1                 " Status: 'S'=Success, 'E'=Error
  VALUE(EV_MESSAGE) TYPE STRING               " Success or error message
  VALUE(ET_RETURN) TYPE BAPIRET2_T            " Return messages table
```

#### 2.2.3 Exceptions

```abap
EXCEPTIONS
  TRIP_NOT_FOUND              " Trip number not found
  INVALID_TRIP_STATUS         " Trip status is not '04'
  ALREADY_CONFIRMED           " Trip already confirmed (status = '05')
  UNAUTHORIZED_ACCESS         " User does not have access to this trip
  NO_LEG_UPDATES              " No leg updates provided
  INVALID_LEG_TYPE            " Invalid leg type (not empty)
  LEG_NOT_FOUND               " Leg not found in system
  LOADED_LEG_UPDATE_NOT_ALLOWED " Loaded leg update attempted
  INVALID_DATE_RANGE          " Source date > Destination date
  TRIP_NO_MISMATCH            " Trip number mismatch
  COMMIT_ERROR                " Commit failed
  DATABASE_ERROR              " Database operation failed
```

---

## 3. Data Dictionary Objects

### 3.1 Type Definitions (TOP Include)

#### 3.1.1 Global Types

**Leg Update Structure:**
```abap
TYPES: BEGIN OF gty_leg_update,
         trip_no TYPE zsce_ctd_itm-trip_no,
         counter TYPE zsce_ctd_itm-counter,
         leg_type TYPE char1,
         source_date TYPE zsce_ctd_itm-source_date,
         destination_date TYPE zsce_ctd_itm-dest_date,
         vendor_remarks TYPE string,
       END OF gty_leg_update.

TYPES: gty_leg_update_table TYPE TABLE OF gty_leg_update.
```

**Trip Header Structure (for validation):**
```abap
TYPES: BEGIN OF gty_trip_hdr_confirm,
         trip_no TYPE zsce_ctd_hdr-trip_no,
         lifnr TYPE zsce_ctd_hdr-lifnr,
         trip_status TYPE zsce_ctd_hdr-trip_status,
       END OF gty_trip_hdr_confirm.
```

**Leg Structure (for validation):**
```abap
TYPES: BEGIN OF gty_leg_validate,
         trip_no TYPE zsce_ctd_itm-trip_no,
         counter TYPE zsce_ctd_itm-counter,
         leg_type TYPE char1,
       END OF gty_leg_validate.

TYPES: gty_leg_validate_table TYPE TABLE OF gty_leg_validate.
```

**Note:** 
- Structure fields must match database table fields
- Use data elements (e.g., `zsce_ctd_itm-trip_no`) not table field references where possible
- Table type must be defined (not generic TABLE) for FM parameter

#### 3.1.2 Constants

```abap
" Leg Type Constants
CONSTANTS: gc_leg_type_empty TYPE char1 VALUE 'E',      " Empty leg
           gc_leg_type_loaded TYPE char1 VALUE 'L'.     " Loaded leg

" Trip Status Constants
CONSTANTS: gc_status_pending TYPE char2 VALUE '04',     " Pending for Transporter Confirmation
           gc_status_confirmed TYPE char2 VALUE '05'.   " Trip Details Confirmed by Vendor

" Status Constants
CONSTANTS: gc_status_success TYPE char1 VALUE 'S',     " Success
           gc_status_error TYPE char1 VALUE 'E'.        " Error
```

**Note:** Never use system variables (sy-*) in CONSTANTS declarations.

---

## 4. Implementation Details

### 4.1 Function Module Code Structure

```abap
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
        lv_message TYPE string.

  " Initialize output
  CLEAR: ev_status, ev_message, et_return.
  ev_status = gc_status_error.

  " Step 1: Input Validation
  " ... validation logic ...

  " Step 2: Verify Trip Exists
  " ... trip validation ...

  " Step 3: Authorization Check
  " ... authorization logic ...

  " Step 4: Trip Status Validation
  " ... status validation ...

  " Step 5: Validate Leg Updates
  " ... leg validation ...

  " Step 6: Update Empty Leg Details
  " ... UPDATE statements ...

  " Step 7: Update Trip Header
  " ... UPDATE statement ...

  " Step 8: Commit Transaction
  " ... COMMIT WORK ...

  " Step 9: Populate Output
  " ... set success status ...

ENDFUNCTION.
```

### 4.2 Step-by-Step Implementation

#### 4.2.1 Step 1: Input Validation

```abap
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
```

#### 4.2.2 Step 2: Verify Trip Exists and Get Header Data

```abap
" Verify trip exists and get header data for validation
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
```

#### 4.2.3 Step 3: Authorization Check

```abap
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
" For now, assume vendor number is passed or retrieved from user context
" Example:
" CALL FUNCTION 'Z_GET_USER_VENDOR'
"   EXPORTING
"     iv_user = sy-uname
"   IMPORTING
"     ev_vendor = lv_vendor_no.
"
" For now, placeholder - to be implemented based on actual user context
" lv_vendor_no = sy-uname.  " Placeholder - adjust based on actual implementation

" Verify trip belongs to vendor
IF lw_trip_hdr-lifnr <> lv_vendor_no.
  ev_status = gc_status_error.
  ev_message = 'User does not have access to this trip'.
  MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '007'
    WITH 'Unauthorized access'
    RAISING unauthorized_access.
ENDIF.
```

**Note:** Vendor number retrieval logic to be implemented based on actual user context mechanism.

#### 4.2.4 Step 4: Trip Status Validation

```abap
" Validate trip status
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
```

#### 4.2.5 Step 5: Validate Leg Updates

```abap
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
```

#### 4.2.6 Step 6: Update Empty Leg Details

```abap
" Update empty leg details
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
    ev_status = gc_status_error.
    ev_message = 'Database error occurred'.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '012'
      WITH 'Database error occurred'
      RAISING database_error.
ENDTRY.
```

**Important Notes:**
- Use UPDATE with WHERE clause (not MODIFY)
- Check SY-SUBRC after each UPDATE
- Update only empty legs (validated in previous step)
- Use TRY-CATCH for error handling

#### 4.2.7 Step 7: Update Trip Header

```abap
" Update trip header status and confirmation info
TRY.
    UPDATE zsce_ctd_hdr
      SET trip_status = gc_status_confirmed
          confirm_date = sy-datum
          confirm_time = sy-uzeit
          confirm_by = sy-uname
      WHERE trip_no = iv_trip_no.

    " Check SY-SUBRC after UPDATE
    IF sy-subrc <> 0.
      ev_status = gc_status_error.
      ev_message = 'Error updating trip header'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '012'
        WITH 'Database error updating trip header'
        RAISING database_error.
    ENDIF.

  CATCH cx_root INTO lo_exception.
    ev_status = gc_status_error.
    ev_message = 'Database error occurred'.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '012'
      WITH 'Database error occurred'
      RAISING database_error.
ENDTRY.
```

**Note:** Field names (confirm_date, confirm_time, confirm_by) to be confirmed based on actual ZSCE_CTD_HDR table structure.

#### 4.2.8 Step 8: Commit Transaction

```abap
" Commit transaction
TRY.
    COMMIT WORK AND WAIT.

  CATCH cx_root INTO lo_exception.
    " Rollback on commit error
    ROLLBACK WORK.
    ev_status = gc_status_error.
    ev_message = 'Error committing transaction'.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '013'
      WITH 'Commit error'
      RAISING commit_error.
ENDTRY.
```

**Important Notes:**
- Use COMMIT WORK AND WAIT for synchronous commit
- Rollback on commit error
- All updates are in single LUW (atomicity)

#### 4.2.9 Step 9: Populate Output

```abap
" Set success status and message
ev_status = gc_status_success.
ev_message = 'Trip & Empty Leg details confirmed successfully.'.

" Populate return table with success message
DATA: lw_return TYPE bapiret2.

CLEAR: lw_return.
lw_return-type = 'S'.
lw_return-id = 'ZCTD'.
lw_return-number = '014'.
lw_return-message_v1 = iv_trip_no.
lw_return-message = 'Trip confirmed successfully'.
APPEND lw_return TO et_return.
```

---

## 5. Error Handling Implementation

### 5.1 Error Handling Pattern with Rollback

```abap
" Error handling with rollback
IF error_condition = abap_true.
  " Rollback any partial updates
  ROLLBACK WORK.
  
  " Set error status
  ev_status = gc_status_error.
  ev_message = 'Error message'.
  
  " Populate return table
  CLEAR: lw_return.
  lw_return-type = 'E'.
  lw_return-id = 'ZCTD'.
  lw_return-number = '001'.
  lw_return-message = 'Error message'.
  APPEND lw_return TO et_return.
  
  " Raise exception
  MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '001'
    WITH 'Error message'
    RAISING exception_name.
ENDIF.
```

### 5.2 Exception Messages

All exceptions should use MESSAGE statement with message class:

**Message Class:** ZCTD (or appropriate message class)
**Message Numbers:**
- 001: Trip number is mandatory / Trip not found
- 002: No leg updates provided
- 003: Trip number mismatch
- 004: Invalid leg type
- 005: Invalid date range
- 006: No RFC authorization
- 007: Unauthorized access
- 008: Trip already confirmed
- 009: Invalid trip status
- 010: Leg not found
- 011: Loaded leg update not allowed
- 012: Database error
- 013: Commit error
- 014: Success message

---

## 6. Performance Optimization

### 6.1 Database Optimization

1. **Index Usage:**
   - Ensure indexes exist on:
     - TRIP_NO (primary key in ZSCE_CTD_HDR)
     - TRIP_NO + COUNTER (primary key in ZSCE_CTD_ITM)

2. **UPDATE Optimization:**
   - Use specific WHERE clauses (primary keys)
   - Update only required fields
   - Minimize number of UPDATE statements (if possible, use single UPDATE with FOR ALL ENTRIES)

3. **Transaction Management:**
   - Single COMMIT at end (not in loop)
   - Minimize lock time
   - Commit quickly after updates

### 6.2 Memory Optimization

1. **Internal Tables:**
   - Use appropriate table types
   - Clear tables after use
   - Avoid unnecessary data copying

2. **Field Selection:**
   - Select only required fields for validation

---

## 7. Security Implementation

### 7.1 Authorization Checks

```abap
" RFC Authorization (mandatory for RFC-enabled FM)
AUTHORITY-CHECK OBJECT 'S_RFC'
  ID 'RFC_TYPE' FIELD 'FUNC'
  ID 'RFC_NAME' FIELD 'Z_SCM_CTD_TRAN_TRIPCONFIRM'
  ID 'ACTVT' FIELD '16'.
IF sy-subrc <> 0.
  RAISE unauthorized_access.
ENDIF.

" Vendor ownership check (implemented in Step 3)
" Verify trip belongs to vendor (LIFNR check)
```

### 7.2 Input Validation

- All input parameters validated
- SQL injection prevention: Use parameterized queries (standard ABAP OpenSQL)
- No dynamic WHERE clause construction from user input

### 7.3 Transaction Security

- All updates in single transaction
- Rollback on any error
- No partial updates committed

---

## 8. Testing Requirements

### 8.1 Unit Testing

1. **Test in SE37:**
   - Test with valid inputs
   - Test with invalid inputs
   - Test error scenarios
   - Test edge cases
   - Verify database updates

2. **Test Data:**
   - Create test data in ZSCE_CTD_HDR (status '04')
   - Create test data in ZSCE_CTD_ITM (empty legs)
   - Test with different scenarios

### 8.2 Integration Testing

1. **RFC Testing:**
   - Test from external system (if applicable)
   - Test from internal SAP programs
   - Test concurrent calls

2. **Authorization Testing:**
   - Test with authorized users
   - Test with unauthorized users

3. **Transaction Testing:**
   - Test rollback on error
   - Test commit on success
   - Test concurrent updates

### 8.3 Performance Testing

1. **Load Testing:**
   - Test with trips having many empty legs (10+)
   - Test concurrent confirmations
   - Test transaction time

2. **Performance Metrics:**
   - Response time < 2 seconds (typical, < 10 legs)
   - Response time < 5 seconds (large trips, < 50 legs)
   - Database time < 50% of total runtime

---

## 9. Code Review Checklist

### 9.1 NetWeaver 7.31 Compatibility
- [ ] No inline declarations (DATA() syntax)
- [ ] No constructor operators (NEW, VALUE, CORRESPONDING)
- [ ] No string templates (|text| syntax)
- [ ] No table expressions (itab[key] syntax)
- [ ] No host variables in SQL (@variable)
- [ ] All variables declared upfront
- [ ] Classic OpenSQL syntax used

### 9.2 Code Quality
- [ ] SY-SUBRC checked after all database operations
- [ ] Proper exception handling
- [ ] Authorization checks implemented
- [ ] Input validation implemented
- [ ] No hard-coded values (use constants)
- [ ] Proper naming conventions (lv_, lt_, lw_ prefixes)
- [ ] UPDATE statements use proper WHERE clauses

### 9.3 Function Module Standards
- [ ] RFC-enabled (if required)
- [ ] Proper parameter types (structures, specific table types)
- [ ] Exception handling implemented
- [ ] Documentation header present
- [ ] Message class used for error messages

### 9.4 Transaction Management
- [ ] COMMIT WORK at end (not in loop)
- [ ] ROLLBACK WORK on errors
- [ ] Single LUW for all updates
- [ ] No partial commits

### 9.5 Security
- [ ] Authorization checks implemented
- [ ] Input validation implemented
- [ ] No SQL injection vulnerabilities
- [ ] Vendor ownership verification

---

## 10. Deployment Checklist

### 10.1 Pre-Deployment
- [ ] Code review completed
- [ ] Unit testing completed
- [ ] Integration testing completed
- [ ] Performance testing completed
- [ ] Documentation updated
- [ ] Message class created/updated

### 10.2 Deployment
- [ ] Function module created in development system
- [ ] Function module tested in development
- [ ] Transport request created
- [ ] Code Inspector checks passed
- [ ] Transport to quality system
- [ ] Testing in quality system
- [ ] Transport to production system

### 10.3 Post-Deployment
- [ ] Production testing completed
- [ ] Monitoring setup (if required)
- [ ] User training completed
- [ ] Documentation published

---

## 11. Maintenance Notes

### 11.1 Future Enhancements
- Consider adding re-confirmation logic (with rollback)
- Consider adding batch confirmation
- Consider adding approval workflow
- Consider adding email notifications

### 11.2 Known Limitations
- One-time confirmation (requires rollback for re-edit)
- No batch confirmation (single trip per call)
- No re-confirmation logic

### 11.3 Dependencies
- ZSCE_CTD_HDR table structure (any changes may require FM update)
- ZSCE_CTD_ITM table structure (any changes may require FM update)
- Vendor number retrieval mechanism - to be confirmed
- Message class ZCTD (or appropriate) - to be created

---

## 12. Complete Function Module Template

```abap
*&---------------------------------------------------------------------*
*& Function Module  Z_SCM_CTD_TRAN_TRIPCONFIRM
*&---------------------------------------------------------------------*
*& Purpose: Transporter Trip Details Confirm
*& Author: [Author Name]
*& Creation Date: [Date]
*&---------------------------------------------------------------------*
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

  " Local data declarations
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
  IF iv_trip_no IS INITIAL.
    ev_status = gc_status_error.
    ev_message = 'Trip number is mandatory'.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '001'
      WITH 'Trip number is mandatory'
      RAISING trip_not_found.
  ENDIF.

  IF it_leg_update IS INITIAL.
    ev_status = gc_status_error.
    ev_message = 'No leg updates provided'.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '002'
      WITH 'No leg updates provided'
      RAISING no_leg_updates.
  ENDIF.

  " Validate each record
  LOOP AT it_leg_update INTO lw_leg_update.
    IF lw_leg_update-trip_no <> iv_trip_no.
      ev_status = gc_status_error.
      ev_message = 'Trip number mismatch'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '003'
        WITH 'Trip number mismatch'
        RAISING trip_no_mismatch.
    ENDIF.

    IF lw_leg_update-leg_type <> gc_leg_type_empty.
      ev_status = gc_status_error.
      ev_message = 'Invalid leg type. Only empty legs can be updated'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '004'
        WITH 'Invalid leg type'
        RAISING invalid_leg_type.
    ENDIF.

    IF lw_leg_update-source_date > lw_leg_update-destination_date.
      ev_status = gc_status_error.
      ev_message = 'Source date must be <= Destination date'.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '005'
        WITH 'Invalid date range'
        RAISING invalid_date_range.
    ENDIF.
  ENDLOOP.

  " Step 2: Verify Trip Exists
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

  " Get vendor number (TODO: Implement based on user context)
  " lv_vendor_no = ... (to be implemented)

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
  SELECT trip_no counter leg_type
    FROM zsce_ctd_itm
    INTO TABLE lt_leg_validate
    WHERE trip_no = iv_trip_no.

  IF sy-subrc <> 0 AND it_leg_update IS NOT INITIAL.
    ev_status = gc_status_error.
    ev_message = 'Legs not found in system'.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '010'
      WITH 'Legs not found'
      RAISING leg_not_found.
  ENDIF.

  LOOP AT it_leg_update INTO lw_leg_update.
    READ TABLE lt_leg_validate INTO lw_leg_validate
      WITH KEY trip_no = lw_leg_update-trip_no
               counter = lw_leg_update-counter.
    
    IF sy-subrc <> 0.
      ev_status = gc_status_error.
      CONCATENATE 'Leg not found: Counter' lw_leg_update-counter
        INTO lv_message SEPARATED BY space.
      ev_message = lv_message.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '010'
        WITH 'Leg not found'
        RAISING leg_not_found.
    ENDIF.

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
        UPDATE zsce_ctd_itm
          SET source_date = lw_leg_update-source_date
              dest_date = lw_leg_update-destination_date
              vendor_remarks = lw_leg_update-vendor_remarks
          WHERE trip_no = lw_leg_update-trip_no
            AND counter = lw_leg_update-counter.

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
      UPDATE zsce_ctd_hdr
        SET trip_status = gc_status_confirmed
            confirm_date = sy-datum
            confirm_time = sy-uzeit
            confirm_by = sy-uname
        WHERE trip_no = iv_trip_no.

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

  CLEAR: lw_return.
  lw_return-type = 'S'.
  lw_return-id = 'ZCTD'.
  lw_return-number = '014'.
  lw_return-message_v1 = iv_trip_no.
  lw_return-message = 'Trip confirmed successfully'.
  APPEND lw_return TO et_return.

ENDFUNCTION.
```

---

## 13. Approval

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Technical Lead | | | |
| ABAP Developer | | | |
| Code Reviewer | | | |
| Project Manager | | | |

---

**Document End**

