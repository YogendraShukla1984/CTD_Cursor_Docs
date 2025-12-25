# Functional Specification
## Fetch CTD Trip Item/Leg Details (Reusable Function Module)

---

**Document Information**

| Field | Value |
|-------|-------|
| **Document Type** | Functional Specification |
| **Function Module Name** | ZSCM_CTD_GETTRIPITM |
| **Version** | 1.0 |
| **Date** | [Current Date] |
| **Author** | [Author Name] |
| **Related BRD** | CTD_BRD03_Fetch_CTD_Trip_Item_Details |

---

## 1. Executive Summary

### 1.1 Purpose
This document defines the functional requirements for a reusable Function Module that retrieves CTD (Converted to Dedicated) Trip Item/Leg details from SAP for a specific trip. The FM is designed to support both Transporter/Vendor users (external portal) and RIL Operations users (internal users) by applying role-based data filtering and authorization logic within the same FM.

### 1.2 Business Objective
- Provide a single, reusable interface for fetching CTD Trip Item/Leg data for a specific trip
- Support role-based data access for different user types (Vendor vs RIL Operations)
- Enable efficient data retrieval for both Transporter Portal UI and RIL Operations monitoring screens
- Ensure data security through built-in authorization checks
- Optimize performance by fetching item/leg data on-demand (separate from header data)
- Return items sorted by leg sequence (COUNTER) for proper display order

### 1.3 Scope
- **In Scope:**
  - Retrieval of CTD Trip Item/Leg records for a specific trip number
  - Role-based data filtering (Vendor vs RIL Operations)
  - Authorization-based data restriction
  - Sorting by leg sequence (COUNTER)
  - Output in structured table format with all leg details

- **Out of Scope:**
  - Trip Header data retrieval (separate FM ZSCM_CTD_GETTRIPHDR required)
  - Data modification/updates
  - Trip creation
  - Integration with external systems beyond data retrieval
  - Batch processing (designed for online/real-time use)
  - Filtering by date range or other criteria (trip-specific only)

---

## 2. Business Process Overview

### 2.1 Process Flow
1. Calling program/UI invokes Function Module ZSCM_CTD_GETTRIPITM
2. System validates input parameters (trip number, user type, user ID)
3. System validates that the trip exists in ZSCE_CTD_HDR
4. System determines user context (Vendor 'V' or RIL Operations 'R')
5. System applies authorization check based on user type:
   - For Vendor users: Verify trip belongs to the vendor
   - For RIL Operations: Verify user has authorization for trip's Business/Sub-Business
6. System fetches all trip item/leg records from ZSCE_CTD_ITM for the specified trip
7. System sorts records by COUNTER (leg sequence)
8. System populates output table with sorted records
9. System returns results to calling program

### 2.2 User Roles
- **Transporter/Vendor Users:** External portal users accessing their own trip item data
- **RIL Operations Users:** Internal users with Business/Sub-Business authorization
- **Authorization:** Role-based access control enforced within FM

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
- **Description:** Unique identifier for the trip whose item/leg details are to be retrieved

**User Type (IV_USER_TYPE)**
- **Type:** CHAR1
- **Length:** 1 character
- **Values:** 
  - 'V' = Transporter/Vendor user
  - 'R' = RIL Operations user
- **Validation:**
  - Must not be blank
  - Must be 'V' or 'R' only
- **Description:** Determines which authorization logic and data filtering to apply

**User ID (IV_USER_ID)**
- **Type:** CHAR12
- **Length:** 12 characters
- **Validation:**
  - Must not be blank
  - For Vendor users (IV_USER_TYPE = 'V'): Must be valid Vendor Number (LIFNR)
  - For RIL Operations (IV_USER_TYPE = 'R'): Must be valid SAP User ID
- **Description:** 
  - For Vendor users: Vendor Number (LIFNR) to verify trip ownership
  - For RIL Operations: SAP User ID for authorization check

---

### 3.2 Output Structure

#### 3.2.1 Output Table (ET_TRIP_ITM)

**Table Type:** Standard Table
**Structure:** Contains trip item/leg fields from ZSCE_CTD_ITM

| Field Name | Type | Description |
|------------|------|-------------|
| TRIP_NO | CHAR14 | Trip Number (Primary Key) |
| COUNTER | NUMC | Leg Sequence Number (Primary Key) |
| LIFNR | CHAR10 | Vendor Number |
| TRUCK_NO | CHAR20 | Truck Number |
| SHNUMBER | CHAR10 | Shipment Number |
| SOURCE_DATE | DATS | Source Date |
| DEST_DATE | DATS | Destination Date |
| MVT_TYPE | CHAR2 | Movement Type (SO/PO/STO) |
| AREA | CHAR10 | Area Code |
| ADRNR | CHAR10 | Source Address Number |
| ADRNZ | CHAR10 | Destination Address Number |
| SOURCE_ZONE | CHAR10 | Source Zone |
| DEST_ZONE | CHAR10 | Destination Zone |
| ROUTE | CHAR6 | Route Code |
| DISTANCE | QUAN | Distance (in kilometers) |
| MATNR | MATNR | Material Number |
| SOURCE_REGION | CHAR10 | Source Region |
| DEST_REGION | CHAR10 | Destination Region |
| BUSINESS_ID | CHAR10 | Business ID |
| SUBBUSINESS_ID | CHAR10 | Sub-Business ID |
| DEST_EXIT_DATE | DATS | Destination Exit Date |
| SOURCE_ENT_DATE | DATS | Source Entry Date |
| CTD_RULEENG_REMARKS | STRING | CTD Rule Engine Remarks |
| VENDOR_REMARKS | STRING | Vendor Remarks (if applicable) |
| RIL_REMARKS | STRING | RIL Operations Remarks (if applicable) |

**Note:** Exact field list to be confirmed based on ZSCE_CTD_ITM table structure. All relevant item/leg-level fields should be included.

**Sorting:** Records are sorted by COUNTER (ascending) to maintain leg sequence order.

---

### 3.3 Validation Rules

#### 3.3.1 Input Validation
1. **Trip Number Validation:**
   - IV_TRIP_NO must not be blank
   - IV_TRIP_NO must exist in ZSCE_CTD_HDR table
   - If trip does not exist, raise exception: TRIP_NOT_FOUND

2. **User Type Validation:**
   - IV_USER_TYPE must be 'V' or 'R'
   - If invalid, raise exception: INVALID_USER_TYPE

3. **User ID Validation:**
   - IV_USER_ID must not be blank
   - For Vendor users: Validate against LFA1 table (if required)
   - If invalid, raise exception: INVALID_USER_ID

#### 3.3.2 Business Validation
1. **Trip Ownership Check (Vendor Users):**
   - For Vendor users: Verify trip belongs to specified vendor
   - Check: ZSCE_CTD_HDR-LIFNR = IV_USER_ID for the trip
   - If trip does not belong to vendor, raise exception: UNAUTHORIZED_ACCESS

2. **Authorization Check (RIL Operations):**
   - For RIL Operations: Verify user has authorization for trip's Business/Sub-Business
   - Retrieve trip's Business/Sub-Business from ZSCE_CTD_HDR
   - Verify user has authorization for these values
   - If unauthorized, raise exception: NO_AUTHORITY

3. **Data Existence:**
   - If no item/leg records found for the trip, return empty table (not an error)
   - This is a valid business scenario (trip may have no legs yet)

---

### 3.4 Processing Logic

#### 3.4.1 Step 1: Input Validation
- Validate IV_TRIP_NO is not blank
- Validate IV_USER_TYPE is 'V' or 'R'
- Validate IV_USER_ID is not blank
- If any validation fails, raise appropriate exception

#### 3.4.2 Step 2: Verify Trip Exists
- Query ZSCE_CTD_HDR table for IV_TRIP_NO
- If trip does not exist (sy-subrc <> 0), raise exception: TRIP_NOT_FOUND
- Store trip header data (for authorization checks)

#### 3.4.3 Step 3: Apply Authorization Check

**For Vendor Users (IV_USER_TYPE = 'V'):**
- Retrieve LIFNR from trip header (ZSCE_CTD_HDR-LIFNR)
- Compare with IV_USER_ID
- If LIFNR <> IV_USER_ID, raise exception: UNAUTHORIZED_ACCESS
- This ensures vendor can only see their own trip items

**For RIL Operations Users (IV_USER_TYPE = 'R'):**
- Retrieve BUSINESS and SUBBUSINESS from trip header
- Retrieve authorized Business codes for user (from authorization object or custom table)
- Retrieve authorized Sub-Business codes for user
- Verify trip's BUSINESS is in authorized list
- Verify trip's SUBBUSINESS is in authorized list
- If not authorized, raise exception: NO_AUTHORITY

#### 3.4.4 Step 4: Fetch Trip Item/Leg Records
- Perform SELECT statement on ZSCE_CTD_ITM table
- WHERE condition: TRIP_NO = IV_TRIP_NO
- Select all relevant item/leg fields
- Store results in internal table

#### 3.4.5 Step 5: Sort Records
- Sort internal table by COUNTER (ascending)
- This ensures legs are returned in sequence order

#### 3.4.6 Step 6: Populate Output Table
- Loop through sorted records
- Map fields to output structure ET_TRIP_ITM
- Append to output table
- Clear output table before populating (ensure clean output)

---

### 3.5 Error Handling

#### 3.5.1 Exception Definitions

| Exception Name | Error Code | Error Message | Trigger Condition |
|----------------|------------|---------------|-------------------|
| TRIP_NOT_FOUND | E001 | Trip number not found in system | IV_TRIP_NO does not exist in ZSCE_CTD_HDR |
| INVALID_USER_TYPE | E002 | Invalid user type. Must be 'V' or 'R' | IV_USER_TYPE not 'V' or 'R' |
| INVALID_USER_ID | E003 | Invalid user ID provided | IV_USER_ID is blank or invalid |
| UNAUTHORIZED_ACCESS | E004 | User does not have access to this trip | Vendor user accessing trip that doesn't belong to them |
| NO_AUTHORITY | E005 | User does not have authorization to access this trip | RIL Operations user lacks Business/Sub-Business authorization |
| DATABASE_ERROR | E006 | Database error occurred while fetching data | Database operation fails |

#### 3.5.2 Error Handling Behavior
- All exceptions must be raised using RAISE statement
- Calling program should handle exceptions appropriately
- No data should be returned in ET_TRIP_ITM if exception is raised
- Error messages should be descriptive and user-friendly

---

### 3.6 Success Criteria

#### 3.6.1 Successful Execution
- Function Module completes without exceptions
- Output table ET_TRIP_ITM is populated with matching records (may be empty if no items found)
- All records in output are filtered according to user role and authorization
- Records are sorted by COUNTER (ascending)
- Performance: Response time < 2 seconds for typical queries

#### 3.6.2 Response Structure
- **Success:** ET_TRIP_ITM contains trip item/leg records sorted by COUNTER
- **No Data Found:** ET_TRIP_ITM is empty (not an error condition - trip may have no legs)
- **Error:** Exception is raised, ET_TRIP_ITM is empty

---

### 3.7 Edge Cases

#### 3.7.1 No Item/Leg Records Found
- **Scenario:** Trip exists but has no item/leg records in ZSCE_CTD_ITM
- **Handling:** Return empty table ET_TRIP_ITM (not an error)
- **Note:** This is a valid business scenario, not an error

#### 3.7.2 Trip Belongs to Different Vendor
- **Scenario:** Vendor user tries to access trip belonging to another vendor
- **Handling:** Raise exception UNAUTHORIZED_ACCESS
- **Note:** Security check to prevent unauthorized data access

#### 3.7.3 User Has No Authorization
- **Scenario:** RIL Operations user has no authorized Business/Sub-Business for the trip
- **Handling:** Raise exception NO_AUTHORITY
- **Note:** Authorization check to ensure data security

#### 3.7.4 Multiple Legs with Same Counter
- **Scenario:** Data integrity issue - multiple legs with same COUNTER value
- **Handling:** Return all records (sorting will maintain order, but duplicates may exist)
- **Note:** Data quality issue should be addressed at source

#### 3.7.5 Trip Status Change During Query
- **Scenario:** Trip status changes while query is executing
- **Handling:** Use consistent read (if required) or accept eventual consistency
- **Note:** Authorization check uses trip header data at query time

---

### 3.8 Dependencies

#### 3.8.1 Database Tables
- **ZSCE_CTD_HDR:** Primary source table for trip header data (for validation and authorization)
- **ZSCE_CTD_ITM:** Primary source table for trip item/leg data
- **LFA1:** (Optional) For vendor validation if required
- **Custom Authorization Table:** (If applicable) For Business/Sub-Business authorization

#### 3.8.2 Authorization Objects
- **S_RFC:** For RFC authorization check (if FM is RFC-enabled)
- **Custom Authorization Object:** (If applicable) For Business/Sub-Business authorization
  - Object: Z_CTD_BUSINESS (example)
  - Fields: BUSINESS, SUBBUSINESS, ACTVT

#### 3.8.3 External Services
- None (pure database retrieval)

#### 3.8.4 Other Function Modules
- **ZSCM_CTD_GETTRIPHDR:** For fetching trip header data (separate call)
- **Z_GET_AUTH_BUSINESS:** (If applicable) For retrieving Business/Sub-Business authorization

---

### 3.9 Performance Requirements

#### 3.9.1 Response Time
- **Target:** < 2 seconds for typical queries (trip with < 50 legs)
- **Maximum:** < 5 seconds for large trips (trip with < 500 legs)

#### 3.9.2 Throughput
- **Target:** Support concurrent calls from multiple users
- **Expected Load:** 10-50 concurrent calls during peak hours

#### 3.9.3 Resource Constraints
- **Database Time:** < 50% of total runtime
- **Memory:** Efficient use of internal tables (avoid SELECT *)
- **Network:** Minimize data transfer (select only required fields)

#### 3.9.4 Optimization Strategies
- Use indexed fields in WHERE clause (TRIP_NO is primary key)
- Select only required fields (not SELECT *)
- Use proper WHERE clause conditions
- Sort in database if possible, otherwise sort in memory

---

### 3.10 Security Requirements

#### 3.10.1 Authentication
- **Required:** Yes (SAP user authentication)
- **Method:** Standard SAP authentication (user must be logged in)

#### 3.10.2 Authorization
- **Required:** Yes (role-based authorization)
- **Vendor Users:** Can only access items for trips belonging to their vendor number
- **RIL Operations:** Can only access items for trips with authorized Business/Sub-Business
- **Enforcement:** Authorization checks performed within FM

#### 3.10.3 Data Privacy
- **PII Masking:** Not required (business data, not personal data)
- **Data Encryption:** Standard SAP database encryption
- **Audit Logging:** (Optional) Log access for sensitive data

#### 3.10.4 Input Validation
- All input parameters must be validated
- SQL injection prevention: Use parameterized queries (standard ABAP)
- XSS prevention: Not applicable (no UI in FM)

---

### 3.11 Test Scenarios

#### 3.11.1 Happy Path Tests

**AT-001: Vendor User - Valid Trip with Items**
- **Input:** IV_TRIP_NO='TRIP001', IV_USER_TYPE='V', IV_USER_ID='0000012345'
- **Expected:** Returns all item/leg records for trip TRIP001 belonging to vendor 0000012345, sorted by COUNTER
- **Validation:** All returned records have TRIP_NO='TRIP001' and LIFNR='0000012345', sorted by COUNTER

**AT-002: RIL Operations User - Valid Trip with Items**
- **Input:** IV_TRIP_NO='TRIP001', IV_USER_TYPE='R', IV_USER_ID='USER001'
- **Expected:** Returns all item/leg records for trip TRIP001 if user has authorization
- **Validation:** All returned records have TRIP_NO='TRIP001', sorted by COUNTER

**AT-003: Trip with Multiple Legs**
- **Input:** Standard input with trip having multiple legs (COUNTER 1, 2, 3, etc.)
- **Expected:** Returns all legs sorted by COUNTER in ascending order
- **Validation:** COUNTER values are in ascending order (1, 2, 3, ...)

**AT-004: Trip with No Items**
- **Input:** IV_TRIP_NO='TRIP002' (trip exists but has no items)
- **Expected:** Returns empty table (no exception)
- **Validation:** ET_TRIP_ITM is empty, no exception raised

#### 3.11.2 Error Path Tests

**AT-005: Trip Not Found**
- **Input:** IV_TRIP_NO='INVALID' (non-existent trip)
- **Expected:** Exception TRIP_NOT_FOUND raised
- **Validation:** Exception code and message correct

**AT-006: Invalid User Type**
- **Input:** IV_USER_TYPE='X' (invalid)
- **Expected:** Exception INVALID_USER_TYPE raised
- **Validation:** Exception code and message correct

**AT-007: Blank Trip Number**
- **Input:** IV_TRIP_NO='' (blank)
- **Expected:** Exception TRIP_NOT_FOUND or validation error
- **Validation:** Appropriate error handling

**AT-008: Unauthorized Access (Vendor)**
- **Input:** IV_TRIP_NO='TRIP003', IV_USER_TYPE='V', IV_USER_ID='0000012345' (trip belongs to different vendor)
- **Expected:** Exception UNAUTHORIZED_ACCESS raised
- **Validation:** Exception code and message correct

**AT-009: No Authorization (RIL Operations)**
- **Input:** IV_TRIP_NO='TRIP004', IV_USER_TYPE='R', IV_USER_ID='UNAUTH_USER' (user with no Business authorization)
- **Expected:** Exception NO_AUTHORITY raised
- **Validation:** Exception code and message correct

#### 3.11.3 Edge Case Tests

**AT-010: Trip with Single Leg**
- **Input:** Standard input with trip having only one leg (COUNTER = 1)
- **Expected:** Returns single record
- **Validation:** One record returned with COUNTER = 1

**AT-011: Trip with Duplicate Counters**
- **Input:** Standard input with trip having duplicate COUNTER values (data quality issue)
- **Expected:** Returns all records (including duplicates)
- **Validation:** All records returned, sorted by COUNTER

**AT-012: Large Trip (Many Legs)**
- **Input:** Standard input with trip having 100+ legs
- **Expected:** Returns all legs, performance acceptable
- **Validation:** All records returned, sorted by COUNTER, response time < 5 seconds

---

## 4. Integration Points

### 4.1 Calling Programs
- **Transporter Portal:** External portal calling via RFC
- **RIL Operations Screens:** Internal SAP programs/reports
- **Other Function Modules:** Can be called from other FMs or classes

### 4.2 Data Flow
```
Calling Program
    ↓
ZSCM_CTD_GETTRIPITM (FM)
    ↓
ZSCE_CTD_HDR (Validation & Authorization)
    ↓
ZSCE_CTD_ITM (Fetch Item/Leg Data)
    ↓
ET_TRIP_ITM (Output Table - Sorted by COUNTER)
    ↓
Calling Program
```

### 4.3 Relationship with Other Function Modules
- **ZSCM_CTD_GETTRIPHDR:** Typically called first to get trip header, then this FM called to get items
- **ZSCM_CTD_ENRICHTRIPDETAILS:** May use this FM to retrieve existing leg data

---

## 5. Future Enhancements (Out of Scope)

- Filtering options (e.g., by leg type, date range, material)
- Pagination support for trips with many legs
- Additional sorting options
- Delta retrieval (only changed records since last call)
- Caching mechanism for frequently accessed trips
- Bulk retrieval (multiple trips in one call)

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

