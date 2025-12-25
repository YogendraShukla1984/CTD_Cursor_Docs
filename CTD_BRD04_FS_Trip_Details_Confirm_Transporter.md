# Functional Specification
## Transporter Trip Details Confirm (CTD)

---

**Document Information**

| Field | Value |
|-------|-------|
| **Document Type** | Functional Specification |
| **Function Module Name** | Z_SCM_CTD_TRAN_TRIPCONFIRM |
| **Version** | 1.0 |
| **Date** | [Current Date] |
| **Author** | [Author Name] |
| **Related BRD** | CTD_BRD04_Trip_Details_Confirm_Transporter |

---

## 1. Executive Summary

### 1.1 Purpose
This document defines the functional requirements for a Function Module that enables Transporter (Vendor) users to confirm CTD (Converted to Dedicated) trip details. The FM allows vendors to update source and destination dates for empty legs, enter vendor remarks, and confirm the trip, which updates the trip status and makes it visible to RIL Operations.

### 1.2 Business Objective
- Enable Transporter/Vendor users to view complete trip header and leg-level details
- Allow vendors to update source and destination dates for empty legs only
- Enable vendors to enter vendor remarks for trip confirmation
- Update trip status to "Trip Details Confirmed by Vendor" (Status '05')
- Make confirmed trips visible to RIL Operations for further processing
- Ensure data integrity through validation and authorization checks
- Support one-time confirmation with rollback capability for re-editing

### 1.3 Scope
- **In Scope:**
  - Trip validation and authorization checks
  - Update of empty leg source and destination dates
  - Update of vendor remarks
  - Trip status update to '05'
  - Confirmation date/time/user tracking
  - Transaction commit after successful update
  - Error handling and validation

- **Out of Scope:**
  - Update of loaded leg details (read-only)
  - Update of trip header data (except status and confirmation fields)
  - Integration with external systems
  - Batch processing (designed for online/real-time use)
  - Re-confirmation logic (requires rollback first)

---

## 2. Business Process Overview

### 2.1 Process Flow
1. Transporter/Vendor user views trip details (using ZSCM_CTD_GETTRIPHDR and ZSCM_CTD_GETTRIPITM)
2. User updates source and destination dates for empty legs
3. User enters vendor remarks
4. User invokes Function Module Z_SCM_CTD_TRAN_TRIPCONFIRM
5. System validates trip exists and belongs to vendor
6. System validates trip status is '04' (Pending for Transporter Confirmation)
7. System validates all leg updates are for empty legs only
8. System validates date logic (source date <= destination date)
9. System updates empty leg dates in ZSCE_CTD_ITM
10. System updates vendor remarks in ZSCE_CTD_ITM
11. System updates trip status to '05' in ZSCE_CTD_HDR
12. System updates confirmation date, time, and user in ZSCE_CTD_HDR
13. System commits transaction
14. System returns success message to user

### 2.2 User Roles
- **Transporter/Vendor Users:** External portal users confirming their own trips
- **Authorization:** Vendor must own the trip (LIFNR match)

---

## 3. Functional Requirements

### 3.1 Input Parameters

#### 3.1.1 Mandatory Inputs

**Trip Number (IV_TRIP_NO)**
- **Type:** CHAR14
- **Length:** 14 characters
- **Validation:**
  - Must not be blank
  - Must be valid trip number format
  - Trip must exist in ZSCE_CTD_HDR table
- **Description:** Unique identifier for the trip to be confirmed

**Leg Update Table (IT_LEG_UPDATE)**
- **Type:** ZTT_CTD_LEG_UPD (specific table type, not generic)
- **Structure:** Contains only empty leg records to be updated
- **Validation:**
  - Must not be empty
  - All records must have LEG_TYPE = 'E' (Empty)
  - All records must have valid TRIP_NO matching IV_TRIP_NO
  - All records must have valid COUNTER values
  - Source date must be <= Destination date for each record
- **Description:** Table containing empty leg updates (dates and remarks)

#### 3.1.2 Leg Update Structure (ZTT_CTD_LEG_UPD)

**Structure Definition:**
```abap
TYPES: BEGIN OF gty_leg_update,
         trip_no TYPE zsce_ctd_itm-trip_no,
         counter TYPE zsce_ctd_itm-counter,
         leg_type TYPE char1,                    " Must be 'E' (Empty)
         source_date TYPE zsce_ctd_itm-source_date,
         destination_date TYPE zsce_ctd_itm-dest_date,
         vendor_remarks TYPE string,             " Vendor remarks
       END OF gty_leg_update.

TYPES: gty_leg_update_table TYPE TABLE OF gty_leg_update.
```

**Field Descriptions:**

| Field Name | Type | Description |
|------------|------|-------------|
| TRIP_NO | CHAR14 | Trip Number (must match IV_TRIP_NO) |
| COUNTER | NUMC | Leg Sequence Number (must exist in ZSCE_CTD_ITM) |
| LEG_TYPE | CHAR1 | Leg Type (must be 'E' for Empty) |
| SOURCE_DATE | DATS | Updated Source Date for empty leg |
| DESTINATION_DATE | DATS | Updated Destination Date for empty leg |
| VENDOR_REMARKS | STRING | Vendor remarks/comments |

---

### 3.2 Output Parameters

#### 3.2.1 Output Structure

**Status (EV_STATUS)**
- **Type:** CHAR1
- **Values:**
  - 'S' = Success
  - 'E' = Error
- **Description:** Overall status of the operation

**Message (EV_MESSAGE)**
- **Type:** STRING
- **Description:** Success or error message text
- **Success Message:** "Trip & Empty Leg details confirmed successfully."
- **Error Messages:** Specific error messages based on validation failures

**Return Table (ET_RETURN)**
- **Type:** BAPIRET2_T (Standard SAP return table)
- **Description:** Detailed return messages (success, warnings, errors)
- **Structure:** Standard BAPIRET2 structure with message details

---

### 3.3 Validation Rules

#### 3.3.1 Header Level Validations

1. **Trip Existence Validation:**
   - IV_TRIP_NO must exist in ZSCE_CTD_HDR table
   - If trip does not exist, raise error: TRIP_NOT_FOUND

2. **Trip Status Validation:**
   - Trip status must be '04' (Pending for Transporter Confirmation)
   - If status is not '04', raise error: INVALID_TRIP_STATUS
   - Valid statuses for confirmation: Only '04'

3. **Vendor Authorization Validation:**
   - Trip must belong to the logged-in vendor
   - Check: ZSCE_CTD_HDR-LIFNR = Current user's vendor number
   - If vendor does not match, raise error: UNAUTHORIZED_ACCESS
   - Authorization check must be performed before any updates

4. **Trip Confirmation Status:**
   - Trip must not already be confirmed (status ≠ '05')
   - If already confirmed, raise error: ALREADY_CONFIRMED

#### 3.3.2 Item/Leg Level Validations

1. **Leg Update Table Validation:**
   - IT_LEG_UPDATE must not be empty
   - If empty, raise error: NO_LEG_UPDATES

2. **Leg Type Validation:**
   - All records in IT_LEG_UPDATE must have LEG_TYPE = 'E' (Empty)
   - If any record has LEG_TYPE ≠ 'E', raise error: INVALID_LEG_TYPE
   - Loaded legs cannot be updated through this FM

3. **Leg Existence Validation:**
   - Each record in IT_LEG_UPDATE must correspond to an existing leg in ZSCE_CTD_ITM
   - Check: TRIP_NO + COUNTER must exist in ZSCE_CTD_ITM
   - If leg does not exist, raise error: LEG_NOT_FOUND

4. **Leg Type Match Validation:**
   - Each leg in ZSCE_CTD_ITM must be an empty leg (LEG_TYPE = 'E')
   - If leg is loaded (LEG_TYPE = 'L'), raise error: LOADED_LEG_UPDATE_NOT_ALLOWED

5. **Date Validation:**
   - Source date must be <= Destination date for each leg
   - If SOURCE_DATE > DESTINATION_DATE, raise error: INVALID_DATE_RANGE
   - Dates must be valid calendar dates

6. **Trip Number Consistency:**
   - All records in IT_LEG_UPDATE must have TRIP_NO matching IV_TRIP_NO
   - If mismatch found, raise error: TRIP_NO_MISMATCH

---

### 3.4 Processing Logic

#### 3.4.1 Step 1: Input Validation
- Validate IV_TRIP_NO is not blank
- Validate IT_LEG_UPDATE is not empty
- Validate all records in IT_LEG_UPDATE have required fields
- If any validation fails, set EV_STATUS = 'E' and return error

#### 3.4.2 Step 2: Verify Trip Exists and Get Header Data
- Query ZSCE_CTD_HDR table for IV_TRIP_NO
- If trip does not exist, raise error: TRIP_NOT_FOUND
- Store trip header data (LIFNR, TRIP_STATUS) for validation

#### 3.4.3 Step 3: Authorization Check
- Verify trip belongs to current vendor
- Retrieve current user's vendor number (from user context or parameter)
- Compare ZSCE_CTD_HDR-LIFNR with vendor number
- If mismatch, raise error: UNAUTHORIZED_ACCESS

#### 3.4.4 Step 4: Trip Status Validation
- Verify trip status is '04' (Pending for Transporter Confirmation)
- If status is not '04', raise error: INVALID_TRIP_STATUS
- If status is '05' (already confirmed), raise error: ALREADY_CONFIRMED

#### 3.4.5 Step 5: Validate Leg Updates
- Loop through IT_LEG_UPDATE
- For each record:
  - Verify LEG_TYPE = 'E' (Empty)
  - Verify leg exists in ZSCE_CTD_ITM (TRIP_NO + COUNTER)
  - Verify leg in database is empty leg (LEG_TYPE = 'E')
  - Verify SOURCE_DATE <= DESTINATION_DATE
  - Verify TRIP_NO matches IV_TRIP_NO
- If any validation fails, raise appropriate error

#### 3.4.6 Step 6: Update Empty Leg Details
- Loop through IT_LEG_UPDATE
- For each record:
  - Update ZSCE_CTD_ITM-SOURCE_DATE with new source date
  - Update ZSCE_CTD_ITM-DEST_DATE with new destination date
  - Update ZSCE_CTD_ITM-VENDOR_REMARKS with vendor remarks (if field exists)
  - Use UPDATE statement with WHERE clause (TRIP_NO + COUNTER)

#### 3.4.7 Step 7: Update Trip Header
- Update ZSCE_CTD_HDR-TRIP_STATUS to '05' (Trip Details Confirmed by Vendor)
- Update ZSCE_CTD_HDR-CONFIRM_DATE to current date (sy-datum)
- Update ZSCE_CTD_HDR-CONFIRM_TIME to current time (sy-uzeit)
- Update ZSCE_CTD_HDR-CONFIRM_BY to current user (sy-uname)
- Use UPDATE statement with WHERE clause (TRIP_NO)

#### 3.4.8 Step 8: Commit Transaction
- Execute COMMIT WORK to save all changes
- If commit fails, rollback and raise error: COMMIT_ERROR

#### 3.4.9 Step 9: Populate Output
- Set EV_STATUS = 'S' (Success)
- Set EV_MESSAGE = "Trip & Empty Leg details confirmed successfully."
- Populate ET_RETURN with success messages

---

### 3.5 Error Handling

#### 3.5.1 Error Definitions

| Error Code | Error Message | Trigger Condition |
|------------|---------------|-------------------|
| TRIP_NOT_FOUND | Trip number not found in system | IV_TRIP_NO does not exist in ZSCE_CTD_HDR |
| INVALID_TRIP_STATUS | Trip status is not valid for confirmation. Status must be '04' | Trip status ≠ '04' |
| ALREADY_CONFIRMED | Trip has already been confirmed | Trip status = '05' |
| UNAUTHORIZED_ACCESS | User does not have access to this trip | Vendor LIFNR does not match trip's LIFNR |
| NO_LEG_UPDATES | No leg updates provided | IT_LEG_UPDATE is empty |
| INVALID_LEG_TYPE | Invalid leg type. Only empty legs can be updated | LEG_TYPE ≠ 'E' in IT_LEG_UPDATE |
| LEG_NOT_FOUND | Leg not found in system | TRIP_NO + COUNTER does not exist in ZSCE_CTD_ITM |
| LOADED_LEG_UPDATE_NOT_ALLOWED | Loaded legs cannot be updated | Leg in database has LEG_TYPE = 'L' |
| INVALID_DATE_RANGE | Source date must be <= Destination date | SOURCE_DATE > DESTINATION_DATE |
| TRIP_NO_MISMATCH | Trip number mismatch in leg update table | TRIP_NO in IT_LEG_UPDATE ≠ IV_TRIP_NO |
| COMMIT_ERROR | Error occurred while committing changes | COMMIT WORK fails |
| DATABASE_ERROR | Database error occurred | Database operation fails |

#### 3.5.2 Error Handling Behavior
- All errors must be caught and handled
- Transaction must be rolled back on error
- EV_STATUS must be set to 'E' on error
- EV_MESSAGE must contain descriptive error message
- ET_RETURN must contain detailed error information
- No partial updates should remain in database

---

### 3.6 Success Criteria

#### 3.6.1 Successful Execution
- Function Module completes without errors
- All empty leg dates updated in ZSCE_CTD_ITM
- Vendor remarks updated in ZSCE_CTD_ITM
- Trip status updated to '05' in ZSCE_CTD_HDR
- Confirmation date, time, and user updated in ZSCE_CTD_HDR
- Transaction committed successfully
- EV_STATUS = 'S'
- EV_MESSAGE contains success message
- ET_RETURN contains success messages

#### 3.6.2 Response Structure
- **Success:** EV_STATUS = 'S', EV_MESSAGE = success message, ET_RETURN contains success messages
- **Error:** EV_STATUS = 'E', EV_MESSAGE = error message, ET_RETURN contains error details

---

### 3.7 Edge Cases

#### 3.7.1 Multiple Empty Legs Update
- **Scenario:** User updates multiple empty legs in single call
- **Handling:** All legs updated in single transaction, all succeed or all fail (atomicity)

#### 3.7.2 Partial Leg Updates
- **Scenario:** User updates only some empty legs (not all)
- **Handling:** Only provided legs are updated, others remain unchanged (valid scenario)

#### 3.7.3 Trip Already Confirmed
- **Scenario:** User tries to confirm trip that is already confirmed (status = '05')
- **Handling:** Raise error ALREADY_CONFIRMED, no updates performed

#### 3.7.4 Concurrent Confirmation Attempt
- **Scenario:** Multiple users try to confirm same trip simultaneously
- **Handling:** Database locking ensures only one succeeds, others get error

#### 3.7.5 No Empty Legs in Trip
- **Scenario:** Trip has no empty legs, but user sends empty IT_LEG_UPDATE
- **Handling:** Raise error NO_LEG_UPDATES (if table is empty) or validate that all legs exist

#### 3.7.6 Date Validation Edge Cases
- **Scenario:** Source date = Destination date (same day)
- **Handling:** Valid scenario, allow update
- **Scenario:** Dates in the past
- **Handling:** Allow (business rule - no restriction on past dates)

---

### 3.8 Dependencies

#### 3.8.1 Database Tables
- **ZSCE_CTD_HDR:** Primary table for trip header (read and update)
- **ZSCE_CTD_ITM:** Primary table for trip items/legs (read and update)

#### 3.8.2 Authorization Objects
- **S_RFC:** For RFC authorization check (if FM is RFC-enabled)
- **Vendor Authorization:** Implicit (trip ownership check)

#### 3.8.3 External Services
- None (pure database operations)

#### 3.8.4 Other Function Modules
- **ZSCM_CTD_GETTRIPHDR:** For fetching trip header (called before confirmation)
- **ZSCM_CTD_GETTRIPITM:** For fetching trip items/legs (called before confirmation)

---

### 3.9 Performance Requirements

#### 3.9.1 Response Time
- **Target:** < 2 seconds for typical updates (trip with < 10 empty legs)
- **Maximum:** < 5 seconds for large updates (trip with < 50 empty legs)

#### 3.9.2 Throughput
- **Target:** Support concurrent calls from multiple vendors
- **Expected Load:** 5-20 concurrent confirmations during peak hours

#### 3.9.3 Resource Constraints
- **Database Time:** < 50% of total runtime
- **Transaction Time:** Minimize lock time (commit quickly)
- **Memory:** Efficient use of internal tables

---

### 3.10 Security Requirements

#### 3.10.1 Authentication
- **Required:** Yes (SAP user authentication)
- **Method:** Standard SAP authentication (user must be logged in)

#### 3.10.2 Authorization
- **Required:** Yes (vendor ownership check)
- **Vendor Users:** Can only confirm trips belonging to their vendor number
- **Enforcement:** Authorization checks performed before any updates

#### 3.10.3 Data Privacy
- **PII Masking:** Not required (business data, not personal data)
- **Data Encryption:** Standard SAP database encryption
- **Audit Logging:** Confirmation date, time, and user are tracked

#### 3.10.4 Input Validation
- All input parameters must be validated
- SQL injection prevention: Use parameterized queries (standard ABAP OpenSQL)
- No dynamic WHERE clause construction from user input

---

### 3.11 Test Scenarios

#### 3.11.1 Happy Path Tests

**AT-001: Vendor User - Confirm Trip with Single Empty Leg**
- **Input:** IV_TRIP_NO='TRIP001', IT_LEG_UPDATE with 1 empty leg update
- **Expected:** Trip status updated to '05', empty leg dates updated, success message returned
- **Validation:** All updates successful, EV_STATUS='S', transaction committed

**AT-002: Vendor User - Confirm Trip with Multiple Empty Legs**
- **Input:** IV_TRIP_NO='TRIP002', IT_LEG_UPDATE with 3 empty leg updates
- **Expected:** All empty legs updated, trip status updated to '05', success returned
- **Validation:** All legs updated correctly, EV_STATUS='S'

**AT-003: Vendor User - Confirm Trip with Vendor Remarks**
- **Input:** IV_TRIP_NO='TRIP003', IT_LEG_UPDATE with vendor remarks
- **Expected:** Vendor remarks updated, trip confirmed successfully
- **Validation:** Remarks saved correctly, EV_STATUS='S'

#### 3.11.2 Error Path Tests

**AT-004: Trip Not Found**
- **Input:** IV_TRIP_NO='INVALID' (non-existent trip)
- **Expected:** Error TRIP_NOT_FOUND, EV_STATUS='E', no updates performed

**AT-005: Invalid Trip Status**
- **Input:** IV_TRIP_NO='TRIP005' (status = '03', not '04')
- **Expected:** Error INVALID_TRIP_STATUS, EV_STATUS='E', no updates performed

**AT-006: Already Confirmed**
- **Input:** IV_TRIP_NO='TRIP006' (status = '05', already confirmed)
- **Expected:** Error ALREADY_CONFIRMED, EV_STATUS='E', no updates performed

**AT-007: Unauthorized Access**
- **Input:** IV_TRIP_NO='TRIP007' (belongs to different vendor)
- **Expected:** Error UNAUTHORIZED_ACCESS, EV_STATUS='E', no updates performed

**AT-008: Loaded Leg Update Attempt**
- **Input:** IT_LEG_UPDATE contains leg with LEG_TYPE='L' (loaded)
- **Expected:** Error INVALID_LEG_TYPE or LOADED_LEG_UPDATE_NOT_ALLOWED, EV_STATUS='E'

**AT-009: Invalid Date Range**
- **Input:** IT_LEG_UPDATE with SOURCE_DATE > DESTINATION_DATE
- **Expected:** Error INVALID_DATE_RANGE, EV_STATUS='E', no updates performed

**AT-010: Leg Not Found**
- **Input:** IT_LEG_UPDATE with COUNTER that doesn't exist
- **Expected:** Error LEG_NOT_FOUND, EV_STATUS='E', no updates performed

#### 3.11.3 Edge Case Tests

**AT-011: Same Day Dates**
- **Input:** IT_LEG_UPDATE with SOURCE_DATE = DESTINATION_DATE
- **Expected:** Update successful (valid scenario), EV_STATUS='S'

**AT-012: Partial Leg Updates**
- **Input:** Trip has 5 empty legs, user updates only 2
- **Expected:** Only 2 legs updated, trip confirmed, EV_STATUS='S'

**AT-013: Concurrent Confirmation**
- **Input:** Two users try to confirm same trip simultaneously
- **Expected:** One succeeds, other gets error (database locking)

---

## 4. Integration Points

### 4.1 Calling Programs
- **Transporter Portal:** External portal calling via RFC
- **SAP Programs:** Internal SAP programs/reports

### 4.2 Data Flow
```
Calling Program
    ↓
Z_SCM_CTD_TRAN_TRIPCONFIRM (FM)
    ↓
Validation & Authorization Checks
    ↓
Update ZSCE_CTD_ITM (Empty Legs)
    ↓
Update ZSCE_CTD_HDR (Status, Confirmation Info)
    ↓
COMMIT WORK
    ↓
Return Success/Error
    ↓
Calling Program
```

### 4.3 Relationship with Other Function Modules
- **ZSCM_CTD_GETTRIPHDR:** Called before confirmation to view trip header
- **ZSCM_CTD_GETTRIPITM:** Called before confirmation to view trip legs

---

## 5. Future Enhancements (Out of Scope)

- Re-confirmation logic (with rollback)
- Batch confirmation (multiple trips)
- Approval workflow
- Email notifications
- Integration with external systems

---

## 6. Approval

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Business Analyst | | | |
| Functional Lead | | | |
| Technical Lead | | | |
| Project Manager | | | |

---

**Document End**

