# Technical Specification
## Fetch CTD Trip Header Details (Reusable Function Module)

---

**Document Information**

| Field | Value |
|-------|-------|
| **Document Type** | Technical Specification |
| **Function Module Name** | ZSCM_CTD_GETTRIPHDR |
| **Function Group** | ZSCM_CTD (or appropriate function group) |
| **Version** | 1.0 |
| **Date** | [Current Date] |
| **Author** | [Author Name] |
| **Target System** | SAP ECC 6.0 / NetWeaver 7.31 |
| **Syntax Level** | abap_731 |
| **Related FS** | CTD_BRD02_FS_Fetch_CTD_Trip_Header |

---

## 1. Technical Overview

### 1.1 System Requirements
- **SAP Version:** ECC 6.0 / NetWeaver 7.31
- **Syntax Level:** abap_731 (strict compatibility required)
- **Database:** SAP HANA or AnyDB
- **Function Module Type:** Remote-Enabled Function Module (RFC-enabled)

### 1.2 Architecture
- **Design Pattern:** Procedural Function Module (appropriate for RFC/reusable logic)
- **Database Access:** Direct OpenSQL queries
- **Authorization:** Built-in authorization checks
- **Error Handling:** Exception-based error handling

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
- Proper exception handling

---

## 2. Function Module Structure

### 2.1 Function Module Attributes

| Attribute | Value |
|-----------|-------|
| **Name** | ZSCM_CTD_GETTRIPHDR |
| **Function Group** | ZSCM_CTD |
| **Remote-Enabled** | Yes (RFC-enabled) |
| **Update Module** | No |
| **Short Text** | Get CTD Trip Header Details |

### 2.2 Function Module Interface

#### 2.2.1 Import Parameters

```abap
IMPORTING
  VALUE(IV_USER_TYPE) TYPE CHAR1          " User Type: 'V'=Vendor, 'R'=RIL Operations
  VALUE(IV_FROM_DATE) TYPE DATS           " From Date (YYYYMMDD)
  VALUE(IV_TO_DATE) TYPE DATS             " To Date (YYYYMMDD)
  VALUE(IV_USER_ID) TYPE CHAR10           " User ID (Vendor Number or SAP User)
  VALUE(IV_TRUCK_NO) TYPE CHAR20 OPTIONAL " Optional Truck Number
```

#### 2.2.2 Export Parameters

```abap
EXPORTING
  VALUE(ET_TRIP_HDR) TYPE gty_trip_hdr_table  " Output table of trip headers
```

**Note:** `gty_trip_hdr_table` must be defined in TOP include or type pool as a specific table type (not generic TABLE).

#### 2.2.3 Exceptions

```abap
EXCEPTIONS
  INVALID_USER_TYPE      " Invalid user type (not 'V' or 'R')
  INVALID_DATE_RANGE     " From Date > To Date
  INVALID_USER_ID        " Blank or invalid user ID
  INVALID_TRUCK_NO       " Invalid truck number format
  NO_AUTHORITY           " User lacks authorization
  DATABASE_ERROR         " Database operation failed
```

---

## 3. Data Dictionary Objects

### 3.1 Type Definitions (TOP Include)

#### 3.1.1 Global Types

**Trip Header Structure:**
```abap
TYPES: BEGIN OF gty_trip_hdr,
         trip_no TYPE zsce_ctd_hdr-trip_no,
         lifnr TYPE zsce_ctd_hdr-lifnr,
         truck_no TYPE zsce_ctd_hdr-truck_no,
         trip_status TYPE zsce_ctd_hdr-trip_status,
         created_date TYPE zsce_ctd_hdr-created_date,
         created_time TYPE zsce_ctd_hdr-created_time,
         created_by TYPE zsce_ctd_hdr-created_by,
         business TYPE zsce_ctd_hdr-business,
         subbusiness TYPE zsce_ctd_hdr-subbusiness,
         " Add other relevant header fields from ZSCE_CTD_HDR
       END OF gty_trip_hdr.

TYPES: gty_trip_hdr_table TYPE TABLE OF gty_trip_hdr.
```

**Note:** 
- Structure fields must match SELECT statement fields exactly
- Use data elements (e.g., `zsce_ctd_hdr-trip_no`) not table field references where possible
- Table type must be defined (not generic TABLE) for FM parameter

#### 3.1.2 Constants

```abap
" User Type Constants
CONSTANTS: gc_user_type_vendor TYPE char1 VALUE 'V',      " Vendor user
           gc_user_type_ril TYPE char1 VALUE 'R'.          " RIL Operations user

" Trip Status Constants
CONSTANTS: gc_status_pending TYPE char2 VALUE '04',        " Pending for Transporter Confirmation
           gc_status_rejected TYPE char2 VALUE '06'.      " Rejected by RIL Operations
```

**Note:** Never use system variables (sy-*) in CONSTANTS declarations.

---

## 4. Implementation Details

### 4.1 Function Module Code Structure

```abap
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
        lw_trip_hdr TYPE gty_trip_hdr,
        lt_business_auth TYPE TABLE OF char10,
        lt_subbusiness_auth TYPE TABLE OF char10,
        lv_where_clause TYPE string,
        lo_exception TYPE REF TO cx_root.

  " Clear output table
  CLEAR: et_trip_hdr.

  " Step 1: Input Validation
  " ... validation logic ...

  " Step 2: Authorization Check
  " ... authorization logic ...

  " Step 3: Build WHERE Clause
  " ... WHERE clause construction ...

  " Step 4: Execute SELECT
  " ... database query ...

  " Step 5: Populate Output
  " ... populate et_trip_hdr ...

ENDFUNCTION.
```

### 4.2 Step-by-Step Implementation

#### 4.2.1 Step 1: Input Validation

```abap
" Validate User Type
IF iv_user_type IS INITIAL.
  RAISE invalid_user_type.
ENDIF.

IF iv_user_type <> gc_user_type_vendor AND
   iv_user_type <> gc_user_type_ril.
  RAISE invalid_user_type.
ENDIF.

" Validate Date Range
IF iv_from_date IS INITIAL OR iv_to_date IS INITIAL.
  RAISE invalid_date_range.
ENDIF.

IF iv_from_date > iv_to_date.
  RAISE invalid_date_range.
ENDIF.

" Validate User ID
IF iv_user_id IS INITIAL.
  RAISE invalid_user_id.
ENDIF.

" Validate Truck Number (if provided)
IF iv_truck_no IS NOT INITIAL.
  " Additional validation if required
  " For now, just check it's not blank (already done by IS NOT INITIAL)
ENDIF.
```

#### 4.2.2 Step 2: Authorization Check

```abap
" RFC Authorization Check (if FM is RFC-enabled)
AUTHORITY-CHECK OBJECT 'S_RFC'
  ID 'RFC_TYPE' FIELD 'FUNC'
  ID 'RFC_NAME' FIELD 'ZSCM_CTD_GETTRIPHDR'
  ID 'ACTVT' FIELD '16'.
IF sy-subrc <> 0.
  RAISE no_authority.
ENDIF.

" Role-based Authorization
IF iv_user_type = gc_user_type_vendor.
  " For vendor users, authorization is implicit (filtered by LIFNR)
  " No additional check required
ELSEIF iv_user_type = gc_user_type_ril.
  " For RIL Operations, check Business/Sub-Business authorization
  " Retrieve authorized Business codes for user
  " This may require custom authorization object or table lookup
  " Example:
  " CALL FUNCTION 'Z_GET_AUTH_BUSINESS'
  "   EXPORTING
  "     iv_user = iv_user_id
  "   IMPORTING
  "     et_business = lt_business_auth
  "     et_subbusiness = lt_subbusiness_auth.
  " 
  " IF lt_business_auth IS INITIAL.
  "   RAISE no_authority.
  " ENDIF.
ENDIF.
```

**Note:** Business/Sub-Business authorization retrieval logic to be implemented based on actual authorization mechanism (custom table, authorization object, etc.).

#### 4.2.3 Step 3: Build Selection Criteria and Execute SELECT

**For Vendor Users:**
```abap
IF iv_user_type = gc_user_type_vendor.
  " Build WHERE clause for vendor user
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
```

**For RIL Operations Users:**
```abap
ELSEIF iv_user_type = gc_user_type_ril.
  " Build WHERE clause for RIL Operations user
  " Note: lt_business_auth and lt_subbusiness_auth populated in Step 2
  
  IF lt_business_auth IS INITIAL.
    " User has no authorization, return empty table
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
IF sy-subrc <> 0 AND sy-subrc <> 4.
  " sy-subrc = 4 means no records found (valid scenario)
  " sy-subrc <> 0 and <> 4 means error
  RAISE database_error.
ENDIF.
```

**Important Notes:**
- SELECT fields must match structure `gty_trip_hdr` exactly in order
- Use specific field list (not SELECT *)
- Use classic OpenSQL (no host variables @)
- Check SY-SUBRC after SELECT
- sy-subrc = 4 (no records found) is valid, not an error

#### 4.2.4 Step 4: Populate Output Table

```abap
" Copy results to output table
et_trip_hdr = lt_trip_hdr.

" Clear local table (cleanup)
CLEAR: lt_trip_hdr.
```

---

## 5. Error Handling Implementation

### 5.1 Exception Handling Pattern

```abap
" Example: Database error handling
TRY.
    " Database operation
    SELECT ...
    FROM zsce_ctd_hdr
    INTO TABLE lt_trip_hdr
    WHERE ...
    
    IF sy-subrc <> 0 AND sy-subrc <> 4.
      RAISE database_error.
    ENDIF.
    
  CATCH cx_root INTO lo_exception.
    " Log error details (if logging required)
    " Then raise appropriate exception
    RAISE database_error.
ENDTRY.
```

### 5.2 Exception Messages

All exceptions should use MESSAGE statement with message class:

```abap
" In exception handling section
IF condition_fails.
  MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '001' 
    WITH 'Invalid user type' 
    RAISING invalid_user_type.
ENDIF.
```

**Message Class:** ZCTD (or appropriate message class)
**Message Numbers:**
- 001: Invalid user type
- 002: Invalid date range
- 003: Invalid user ID
- 004: Invalid truck number
- 005: No authorization
- 006: Database error

---

## 6. Performance Optimization

### 6.1 Database Optimization

1. **Index Usage:**
   - Ensure indexes exist on:
     - CREATED_DATE
     - TRIP_STATUS
     - LIFNR
     - TRUCK_NO
     - BUSINESS
     - SUBBUSINESS

2. **SELECT Optimization:**
   - Select only required fields (not SELECT *)
   - Use proper WHERE clause conditions
   - Avoid IS NOT INITIAL in WHERE clause (use <> space if needed)
   - Use IN clause for status and authorization lists

3. **Table Access:**
   - Single SELECT statement (no loops with SELECT inside)
   - Use FOR ALL ENTRIES if needed (not applicable here)

### 6.2 Memory Optimization

1. **Internal Tables:**
   - Use appropriate table type (STANDARD TABLE)
   - Clear tables after use
   - Avoid unnecessary data copying

2. **Field Selection:**
   - Select only required fields
   - Structure matches SELECT fields exactly

---

## 7. Security Implementation

### 7.1 Authorization Checks

```abap
" RFC Authorization (mandatory for RFC-enabled FM)
AUTHORITY-CHECK OBJECT 'S_RFC'
  ID 'RFC_TYPE' FIELD 'FUNC'
  ID 'RFC_NAME' FIELD 'ZSCM_CTD_GETTRIPHDR'
  ID 'ACTVT' FIELD '16'.
IF sy-subrc <> 0.
  RAISE no_authority.
ENDIF.

" Role-based data filtering (implemented in WHERE clause)
" Vendor users: Filtered by LIFNR = IV_USER_ID
" RIL Operations: Filtered by authorized Business/Sub-Business
```

### 7.2 Input Validation

- All input parameters validated
- SQL injection prevention: Use parameterized queries (standard ABAP OpenSQL)
- No dynamic WHERE clause construction from user input (use static WHERE with parameters)

---

## 8. Testing Requirements

### 8.1 Unit Testing

1. **Test in SE37:**
   - Test with valid inputs
   - Test with invalid inputs
   - Test error scenarios
   - Test edge cases

2. **Test Data:**
   - Create test data in ZSCE_CTD_HDR
   - Test with different user types
   - Test with different date ranges
   - Test with/without truck number

### 8.2 Integration Testing

1. **RFC Testing:**
   - Test from external system (if applicable)
   - Test from internal SAP programs
   - Test concurrent calls

2. **Authorization Testing:**
   - Test with authorized users
   - Test with unauthorized users
   - Test with different authorization levels

### 8.3 Performance Testing

1. **Load Testing:**
   - Test with large date ranges
   - Test with large result sets
   - Test concurrent calls

2. **Performance Metrics:**
   - Response time < 3 seconds (typical)
   - Response time < 10 seconds (large queries)
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
- [ ] No SELECT * (specific fields only)
- [ ] Structure matches SELECT fields exactly
- [ ] SY-SUBRC checked after all database operations
- [ ] Proper exception handling
- [ ] Authorization checks implemented
- [ ] Input validation implemented
- [ ] No hard-coded values (use constants)
- [ ] Proper naming conventions (lv_, lt_, lw_ prefixes)

### 9.3 Function Module Standards
- [ ] RFC-enabled (if required)
- [ ] Proper parameter types (structures, specific table types)
- [ ] Exception handling implemented
- [ ] Documentation header present
- [ ] Message class used for error messages

### 9.4 Performance
- [ ] Indexed fields used in WHERE clause
- [ ] Only required fields selected
- [ ] No SELECT in loops
- [ ] Efficient WHERE clause construction

### 9.5 Security
- [ ] Authorization checks implemented
- [ ] Input validation implemented
- [ ] No SQL injection vulnerabilities
- [ ] Role-based data filtering implemented

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
- Consider adding pagination for large result sets
- Consider adding caching mechanism
- Consider adding additional filtering options
- Consider adding sorting options

### 11.2 Known Limitations
- No pagination (all matching records returned)
- No caching (always queries database)
- Limited to status '04' and '06' (hard-coded)

### 11.3 Dependencies
- ZSCE_CTD_HDR table structure (any changes may require FM update)
- Authorization mechanism (custom table/object) - to be confirmed
- Message class ZCTD (or appropriate) - to be created

---

## 12. Complete Function Module Template

```abap
*&---------------------------------------------------------------------*
*& Function Module  ZSCM_CTD_GETTRIPHDR
*&---------------------------------------------------------------------*
*& Purpose: Get CTD Trip Header Details with Role-Based Filtering
*& Author: [Author Name]
*& Creation Date: [Date]
*&---------------------------------------------------------------------*
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

  " Local data declarations
  DATA: lt_trip_hdr TYPE gty_trip_hdr_table,
        lt_business_auth TYPE TABLE OF char10,
        lt_subbusiness_auth TYPE TABLE OF char10,
        lo_exception TYPE REF TO cx_root.

  " Clear output
  CLEAR: et_trip_hdr.

  " Step 1: Input Validation
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

  IF iv_user_id IS INITIAL.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '003'
      WITH 'User ID is mandatory'
      RAISING invalid_user_id.
  ENDIF.

  " Step 2: Authorization Check
  AUTHORITY-CHECK OBJECT 'S_RFC'
    ID 'RFC_TYPE' FIELD 'FUNC'
    ID 'RFC_NAME' FIELD 'ZSCM_CTD_GETTRIPHDR'
    ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '005'
      WITH 'No RFC authorization'
      RAISING no_authority.
  ENDIF.

  " Step 3: Get Authorization Data (for RIL Operations)
  IF iv_user_type = gc_user_type_ril.
    " TODO: Implement Business/Sub-Business authorization retrieval
    " This may require custom FM or table lookup
    " For now, placeholder logic
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
  ENDIF.

  " Step 4: Execute SELECT based on user type
  TRY.
      IF iv_user_type = gc_user_type_vendor.
        " Vendor user: Filter by LIFNR
        IF iv_truck_no IS NOT INITIAL.
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
          " No authorization, return empty
          CLEAR: et_trip_hdr.
          RETURN.
        ENDIF.

        IF iv_truck_no IS NOT INITIAL.
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

      " Check SELECT result
      IF sy-subrc <> 0 AND sy-subrc <> 4.
        " sy-subrc = 4 means no records found (valid)
        " Other values indicate error
        MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '006'
          WITH 'Database error occurred'
          RAISING database_error.
      ENDIF.

    CATCH cx_root INTO lo_exception.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '006'
        WITH 'Database error occurred'
        RAISING database_error.
  ENDTRY.

  " Step 5: Populate output
  et_trip_hdr = lt_trip_hdr.

  " Cleanup
  CLEAR: lt_trip_hdr.

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

