# Technical Specification
## Fetch CTD Trip Item/Leg Details (Reusable Function Module)

---

**Document Information**

| Field | Value |
|-------|-------|
| **Document Type** | Technical Specification |
| **Function Module Name** | ZSCM_CTD_GETTRIPITM |
| **Function Group** | ZSCM_CTD (or appropriate function group) |
| **Version** | 1.0 |
| **Date** | [Current Date] |
| **Author** | [Author Name] |
| **Target System** | SAP ECC 6.0 / NetWeaver 7.31 |
| **Syntax Level** | abap_731 |
| **Related FS** | CTD_BRD03_FS_Fetch_CTD_Trip_Item_Details |

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
| **Name** | ZSCM_CTD_GETTRIPITM |
| **Function Group** | ZSCM_CTD |
| **Remote-Enabled** | Yes (RFC-enabled) |
| **Update Module** | No |
| **Short Text** | Get CTD Trip Item/Leg Details |

### 2.2 Function Module Interface

#### 2.2.1 Import Parameters

```abap
IMPORTING
  VALUE(IV_TRIP_NO) TYPE CHAR14          " Trip Number
  VALUE(IV_USER_TYPE) TYPE CHAR1         " User Type: 'V'=Vendor, 'R'=RIL Operations
  VALUE(IV_USER_ID) TYPE CHAR12          " User ID (Vendor Number or SAP User)
```

#### 2.2.2 Export Parameters

```abap
EXPORTING
  VALUE(ET_TRIP_ITM) TYPE gty_trip_itm_table  " Output table of trip items/legs
```

**Note:** `gty_trip_itm_table` must be defined in TOP include or type pool as a specific table type (not generic TABLE).

#### 2.2.3 Exceptions

```abap
EXCEPTIONS
  TRIP_NOT_FOUND       " Trip number not found
  INVALID_USER_TYPE    " Invalid user type (not 'V' or 'R')
  INVALID_USER_ID      " Blank or invalid user ID
  UNAUTHORIZED_ACCESS  " User does not have access to this trip
  NO_AUTHORITY         " User lacks authorization
  DATABASE_ERROR       " Database operation failed
```

---

## 3. Data Dictionary Objects

### 3.1 Type Definitions (TOP Include)

#### 3.1.1 Global Types

**Trip Item/Leg Structure:**
```abap
TYPES: BEGIN OF gty_trip_itm,
         trip_no TYPE zsce_ctd_itm-trip_no,
         counter TYPE zsce_ctd_itm-counter,
         lifnr TYPE zsce_ctd_itm-lifnr,
         truck_no TYPE zsce_ctd_itm-truck_no,
         shnumber TYPE zsce_ctd_itm-shnumber,
         source_date TYPE zsce_ctd_itm-source_date,
         dest_date TYPE zsce_ctd_itm-dest_date,
         mvt_type TYPE zsce_ctd_itm-mvt_type,
         area TYPE zsce_ctd_itm-area,
         adrnr TYPE zsce_ctd_itm-adrnr,
         adrnz TYPE zsce_ctd_itm-adrnz,
         source_zone TYPE zsce_ctd_itm-source_zone,
         dest_zone TYPE zsce_ctd_itm-dest_zone,
         route TYPE zsce_ctd_itm-route,
         distance TYPE zsce_ctd_itm-distance,
         matnr TYPE matnr,
         source_region TYPE zsce_ctd_itm-source_region,
         dest_region TYPE zsce_ctd_itm-dest_region,
         business_id TYPE zsce_ctd_itm-business_id,
         subbusiness_id TYPE zsce_ctd_itm-subbusiness_id,
         dest_exit_date TYPE zsce_ctd_itm-dest_exit_date,
         source_ent_date TYPE zsce_ctd_itm-source_ent_date,
         ctd_ruleeng_remarks TYPE zsce_ctd_itm-ctd_ruleeng_remarks,
         " Add other relevant item/leg fields from ZSCE_CTD_ITM
       END OF gty_trip_itm.

TYPES: gty_trip_itm_table TYPE TABLE OF gty_trip_itm.
```

**Trip Header Structure (for authorization):**
```abap
TYPES: BEGIN OF gty_trip_hdr_auth,
         trip_no TYPE zsce_ctd_hdr-trip_no,
         lifnr TYPE zsce_ctd_hdr-lifnr,
         business TYPE zsce_ctd_hdr-business,
         subbusiness TYPE zsce_ctd_hdr-subbusiness,
       END OF gty_trip_hdr_auth.
```

**Note:** 
- Structure fields must match SELECT statement fields exactly
- Use data elements (e.g., `zsce_ctd_itm-trip_no`) not table field references where possible
- Table type must be defined (not generic TABLE) for FM parameter

#### 3.1.2 Constants

```abap
" User Type Constants
CONSTANTS: gc_user_type_vendor TYPE char1 VALUE 'V',      " Vendor user
           gc_user_type_ril TYPE char1 VALUE 'R'.          " RIL Operations user
```

**Note:** Never use system variables (sy-*) in CONSTANTS declarations.

---

## 4. Implementation Details

### 4.1 Function Module Code Structure

```abap
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
        lw_trip_itm TYPE gty_trip_itm,
        lw_trip_hdr TYPE gty_trip_hdr_auth,
        lt_business_auth TYPE TABLE OF char10,
        lt_subbusiness_auth TYPE TABLE OF char10,
        lo_exception TYPE REF TO cx_root.

  " Clear output table
  CLEAR: et_trip_itm.

  " Step 1: Input Validation
  " ... validation logic ...

  " Step 2: Verify Trip Exists
  " ... trip validation ...

  " Step 3: Authorization Check
  " ... authorization logic ...

  " Step 4: Fetch Item/Leg Records
  " ... database query ...

  " Step 5: Sort Records
  " ... sorting logic ...

  " Step 6: Populate Output
  " ... populate et_trip_itm ...

ENDFUNCTION.
```

### 4.2 Step-by-Step Implementation

#### 4.2.1 Step 1: Input Validation

```abap
" Validate Trip Number
IF iv_trip_no IS INITIAL.
  MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '001'
    WITH 'Trip number is mandatory'
    RAISING trip_not_found.
ENDIF.

" Validate User Type
IF iv_user_type IS INITIAL.
  RAISE invalid_user_type.
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
```

#### 4.2.2 Step 2: Verify Trip Exists and Get Header Data

```abap
" Verify trip exists and get header data for authorization
SELECT SINGLE trip_no lifnr business subbusiness
  FROM zsce_ctd_hdr
  INTO lw_trip_hdr
  WHERE trip_no = iv_trip_no.

IF sy-subrc <> 0.
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
  "   MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '005'
  "     WITH 'No Business authorization'
  "     RAISING no_authority.
  " ENDIF.
  "
  " " Check if trip's business is authorized
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
```

**Note:** Business/Sub-Business authorization retrieval logic to be implemented based on actual authorization mechanism (custom table, authorization object, etc.).

#### 4.2.4 Step 4: Fetch Item/Leg Records and Sort

```abap
" Fetch all item/leg records for the trip
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

    " Check SY-SUBRC after SELECT
    " sy-subrc = 0: Records found
    " sy-subrc = 4: No records found (valid scenario, not an error)
    " sy-subrc <> 0 and <> 4: Database error
    IF sy-subrc <> 0 AND sy-subrc <> 4.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '006'
        WITH 'Database error occurred'
        RAISING database_error.
    ENDIF.

    " Sort by COUNTER (ascending) to maintain leg sequence
    IF lt_trip_itm IS NOT INITIAL.
      SORT lt_trip_itm BY counter.
    ENDIF.

  CATCH cx_root INTO lo_exception.
    " Handle any unexpected exceptions
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '006'
      WITH 'Database error occurred'
      RAISING database_error.
ENDTRY.
```

**Important Notes:**
- SELECT fields must match structure `gty_trip_itm` exactly in order
- Use specific field list (not SELECT *)
- Use classic OpenSQL (no host variables @)
- Check SY-SUBRC after SELECT
- sy-subrc = 4 (no records found) is valid, not an error
- Sort by COUNTER after SELECT to ensure proper sequence

#### 4.2.5 Step 5: Populate Output Table

```abap
" Copy results to output table
et_trip_itm = lt_trip_itm.

" Cleanup
CLEAR: lt_trip_itm.
CLEAR: lw_trip_hdr.
CLEAR: lt_business_auth.
CLEAR: lt_subbusiness_auth.
```

---

## 5. Error Handling Implementation

### 5.1 Exception Handling Pattern

```abap
" Example: Database error handling
TRY.
    " Database operation
    SELECT ...
    FROM zsce_ctd_itm
    INTO TABLE lt_trip_itm
    WHERE trip_no = iv_trip_no.
    
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
    WITH 'Trip number not found' 
    RAISING trip_not_found.
ENDIF.
```

**Message Class:** ZCTD (or appropriate message class)
**Message Numbers:**
- 001: Trip not found
- 002: Invalid user type
- 003: Invalid user ID
- 004: Unauthorized access
- 005: No authorization
- 006: Database error

---

## 6. Performance Optimization

### 6.1 Database Optimization

1. **Index Usage:**
   - Ensure indexes exist on:
     - TRIP_NO (primary key in ZSCE_CTD_ITM)
     - COUNTER (for sorting)
     - TRIP_NO + COUNTER (composite index if needed)

2. **SELECT Optimization:**
   - Select only required fields (not SELECT *)
   - Use proper WHERE clause conditions
   - Use primary key (TRIP_NO) in WHERE clause for optimal performance

3. **Table Access:**
   - Single SELECT statement (no loops with SELECT inside)
   - Sort in memory after SELECT (more efficient than ORDER BY for small-medium result sets)

### 6.2 Memory Optimization

1. **Internal Tables:**
   - Use appropriate table type (STANDARD TABLE)
   - Clear tables after use
   - Avoid unnecessary data copying

2. **Field Selection:**
   - Select only required fields
   - Structure matches SELECT fields exactly

### 6.3 Sorting Strategy

- **Sort in Memory:** After SELECT, sort internal table by COUNTER
- **Reason:** More efficient for typical result sets (< 500 records)
- **Alternative:** Use ORDER BY in SELECT if result sets are very large (> 1000 records)

---

## 7. Security Implementation

### 7.1 Authorization Checks

```abap
" RFC Authorization (mandatory for RFC-enabled FM)
AUTHORITY-CHECK OBJECT 'S_RFC'
  ID 'RFC_TYPE' FIELD 'FUNC'
  ID 'RFC_NAME' FIELD 'ZSCM_CTD_GETTRIPITM'
  ID 'ACTVT' FIELD '16'.
IF sy-subrc <> 0.
  RAISE no_authority.
ENDIF.

" Role-based data filtering (implemented in authorization check)
" Vendor users: Verify trip belongs to vendor (LIFNR check)
" RIL Operations: Verify user has authorization for trip's Business/Sub-Business
```

### 7.2 Input Validation

- All input parameters validated
- SQL injection prevention: Use parameterized queries (standard ABAP OpenSQL)
- No dynamic WHERE clause construction from user input (use static WHERE with parameters)

### 7.3 Trip Ownership Verification

- For Vendor users: Verify trip header LIFNR matches IV_USER_ID
- Prevents unauthorized access to other vendors' trip data

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
   - Create test data in ZSCE_CTD_ITM
   - Test with different user types
   - Test with trips having different numbers of legs

### 8.2 Integration Testing

1. **RFC Testing:**
   - Test from external system (if applicable)
   - Test from internal SAP programs
   - Test concurrent calls

2. **Authorization Testing:**
   - Test with authorized users
   - Test with unauthorized users
   - Test with different authorization levels

3. **Integration with Header FM:**
   - Test calling ZSCM_CTD_GETTRIPHDR first, then this FM
   - Verify data consistency

### 8.3 Performance Testing

1. **Load Testing:**
   - Test with trips having many legs (100+)
   - Test concurrent calls
   - Test with large result sets

2. **Performance Metrics:**
   - Response time < 2 seconds (typical, < 50 legs)
   - Response time < 5 seconds (large trips, < 500 legs)
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
- [ ] Records sorted by COUNTER

### 9.3 Function Module Standards
- [ ] RFC-enabled (if required)
- [ ] Proper parameter types (structures, specific table types)
- [ ] Exception handling implemented
- [ ] Documentation header present
- [ ] Message class used for error messages

### 9.4 Performance
- [ ] Indexed fields used in WHERE clause (TRIP_NO)
- [ ] Only required fields selected
- [ ] No SELECT in loops
- [ ] Efficient WHERE clause construction
- [ ] Sorting implemented (by COUNTER)

### 9.5 Security
- [ ] Authorization checks implemented
- [ ] Input validation implemented
- [ ] No SQL injection vulnerabilities
- [ ] Role-based data filtering implemented
- [ ] Trip ownership verification (for vendors)

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
- Consider adding filtering options (by leg type, material, date range)
- Consider adding pagination for trips with many legs
- Consider adding additional sorting options
- Consider bulk retrieval (multiple trips in one call)

### 11.2 Known Limitations
- No filtering options (returns all legs for trip)
- No pagination (all matching records returned)
- No caching (always queries database)
- Limited to single trip per call

### 11.3 Dependencies
- ZSCE_CTD_HDR table structure (for validation and authorization)
- ZSCE_CTD_ITM table structure (any changes may require FM update)
- Authorization mechanism (custom table/object) - to be confirmed
- Message class ZCTD (or appropriate) - to be created

---

## 12. Complete Function Module Template

```abap
*&---------------------------------------------------------------------*
*& Function Module  ZSCM_CTD_GETTRIPITM
*&---------------------------------------------------------------------*
*& Purpose: Get CTD Trip Item/Leg Details with Role-Based Filtering
*& Author: [Author Name]
*& Creation Date: [Date]
*&---------------------------------------------------------------------*
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

  " Local data declarations
  DATA: lt_trip_itm TYPE gty_trip_itm_table,
        lw_trip_hdr TYPE gty_trip_hdr_auth,
        lt_business_auth TYPE TABLE OF char10,
        lt_subbusiness_auth TYPE TABLE OF char10,
        lo_exception TYPE REF TO cx_root.

  " Clear output
  CLEAR: et_trip_itm.

  " Step 1: Input Validation
  IF iv_trip_no IS INITIAL.
    MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '001'
      WITH 'Trip number is mandatory'
      RAISING trip_not_found.
  ENDIF.

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
    " Verify trip belongs to vendor
    IF lw_trip_hdr-lifnr <> iv_user_id.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '004'
        WITH 'User does not have access to this trip'
        RAISING unauthorized_access.
    ENDIF.

  ELSEIF iv_user_type = gc_user_type_ril.
    " Get Authorization Data (for RIL Operations)
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
    "
    " " Check authorization
    " SORT lt_business_auth.
    " READ TABLE lt_business_auth WITH KEY table_line = lw_trip_hdr-business
    "   TRANSPORTING NO FIELDS
    "   BINARY SEARCH.
    " IF sy-subrc <> 0.
    "   MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '005'
    "     WITH 'No authorization for this trip'
    "     RAISING no_authority.
    " ENDIF.
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
      IF sy-subrc <> 0 AND sy-subrc <> 4.
        " sy-subrc = 4 means no records found (valid)
        " Other values indicate error
        MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '006'
          WITH 'Database error occurred'
          RAISING database_error.
      ENDIF.

      " Step 5: Sort Records by COUNTER
      IF lt_trip_itm IS NOT INITIAL.
        SORT lt_trip_itm BY counter.
      ENDIF.

    CATCH cx_root INTO lo_exception.
      MESSAGE ID 'ZCTD' TYPE 'E' NUMBER '006'
        WITH 'Database error occurred'
        RAISING database_error.
  ENDTRY.

  " Step 6: Populate output
  et_trip_itm = lt_trip_itm.

  " Cleanup
  CLEAR: lt_trip_itm.
  CLEAR: lw_trip_hdr.
  CLEAR: lt_business_auth.
  CLEAR: lt_subbusiness_auth.

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

