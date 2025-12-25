# Functional Specification
## Fetch CTD Trip Header Details (Reusable Function Module)

---

**Document Information**

| Field | Value |
|-------|-------|
| **Document Type** | Functional Specification |
| **Function Module Name** | ZSCM_CTD_GETTRIPHDR |
| **Version** | 1.0 |
| **Date** | [Current Date] |
| **Author** | [Author Name] |
| **Related BRD** | CTD_BRD02_Fetch_CTD_Trip_Header |

---

## 1. Executive Summary

### 1.1 Purpose
This document defines the functional requirements for a reusable Function Module that retrieves CTD (Converted to Dedicated) Trip Header details from SAP based on user search criteria. The FM is designed to support both Transporter/Vendor users (external portal) and RIL Operations users (internal users) by applying role-based data filtering and authorization logic within the same FM.

### 1.2 Business Objective
- Provide a single, reusable interface for fetching CTD Trip Header data
- Support role-based data access for different user types (Vendor vs RIL Operations)
- Enable efficient data retrieval for both Transporter Portal UI and RIL Operations monitoring screens
- Ensure data security through built-in authorization checks
- Optimize performance by fetching only header-level data (items/legs fetched separately on demand)

### 1.3 Scope
- **In Scope:**
  - Retrieval of CTD Trip Header records based on date range
  - Role-based data filtering (Vendor vs RIL Operations)
  - Authorization-based data restriction
  - Optional filtering by Truck Number
  - Status-based filtering (Status '04' and '06')
  - Output in structured table format

- **Out of Scope:**
  - Trip Item/Leg data retrieval (separate FM required)
  - Data modification/updates
  - Trip creation
  - Integration with external systems beyond data retrieval
  - Batch processing (designed for online/real-time use)

---

## 2. Business Process Overview

### 2.1 Process Flow
1. Calling program/UI invokes Function Module ZSCM_CTD_GETTRIPHDR
2. System validates input parameters (user type, date range, optional truck number)
3. System determines user context (Vendor 'V' or RIL Operations 'R')
4. System fetches trip header records from ZSCE_CTD_HDR based on:
   - Date range (CREATED_DATE between FROM_DATE and TO_DATE)
   - Trip Status ('04' or '06')
   - Optional Truck Number filter
5. System applies role-based data restriction:
   - For Vendor users: Filter by Vendor Number (LIFNR)
   - For RIL Operations: Filter by authorized Business and Sub-Business
6. System populates output table with filtered records
7. System returns results to calling program

### 2.2 User Roles
- **Transporter/Vendor Users:** External portal users accessing their own trip data
- **RIL Operations Users:** Internal users with Business/Sub-Business authorization
- **Authorization:** Role-based access control enforced within FM

---

## 3. Functional Requirements

### 3.1 Input Parameters

#### 3.1.1 Mandatory Inputs

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

**From Date (IV_FROM_DATE)**
- **Type:** DATS (Date)
- **Length:** 8 characters
- **Format:** YYYYMMDD
- **Validation:**
  - Must not be blank
  - Must be valid date
  - Must be less than or equal to To Date
- **Description:** Start date for trip selection based on CREATED_DATE field

**To Date (IV_TO_DATE)**
- **Type:** DATS (Date)
- **Length:** 8 characters
- **Format:** YYYYMMDD
- **Validation:**
  - Must not be blank
  - Must be valid date
  - Must be greater than or equal to From Date
- **Description:** End date for trip selection based on CREATED_DATE field

**User ID (IV_USER_ID)**
- **Type:** CHAR10 (or appropriate vendor/user ID type)
- **Length:** 10 characters
- **Validation:**
  - Must not be blank
  - For Vendor users (IV_USER_TYPE = 'V'): Must be valid Vendor Number (LIFNR)
  - For RIL Operations (IV_USER_TYPE = 'R'): Must be valid SAP User ID
- **Description:** 
  - For Vendor users: Vendor Number (LIFNR) to filter trips
  - For RIL Operations: SAP User ID for authorization check

#### 3.1.2 Optional Inputs

**Truck Number (IV_TRUCK_NO)**
- **Type:** CHAR20 (or appropriate truck number type)
- **Length:** 20 characters
- **Validation:**
  - If provided, must not be blank
  - Must match format of TRUCK_NO field in ZSCE_CTD_HDR
- **Description:** Optional filter to restrict results to specific truck number
- **Default:** If not provided, all trucks are included in results

---

### 3.2 Output Structure

#### 3.2.1 Output Table (ET_TRIP_HDR)

**Table Type:** Standard Table
**Structure:** Contains trip header fields from ZSCE_CTD_HDR

| Field Name | Type | Description |
|------------|------|-------------|
| TRIP_NO | CHAR14 | Trip Number (Primary Key) |
| LIFNR | CHAR10 | Vendor Number |
| TRUCK_NO | CHAR20 | Truck Number |
| TRIP_STATUS | CHAR2 | Trip Status ('04' or '06') |
| CREATED_DATE | DATS | Trip Creation Date |
| CREATED_TIME | TIMS | Trip Creation Time |
| CREATED_BY | CHAR12 | User who created the trip |
| BUSINESS | CHAR10 | Business Code |
| SUBBUSINESS | CHAR10 | Sub-Business Code |
| [Additional fields as per ZSCE_CTD_HDR structure] | | |

**Note:** Exact field list to be confirmed based on ZSCE_CTD_HDR table structure. Only relevant header-level fields should be included.

---

### 3.3 Validation Rules

#### 3.3.1 Input Validation
1. **User Type Validation:**
   - IV_USER_TYPE must be 'V' or 'R'
   - If invalid, raise exception: INVALID_USER_TYPE

2. **Date Range Validation:**
   - IV_FROM_DATE must be <= IV_TO_DATE
   - Both dates must be valid calendar dates
   - If invalid, raise exception: INVALID_DATE_RANGE

3. **User ID Validation:**
   - IV_USER_ID must not be blank
   - For Vendor users: Validate against LFA1 table (if required)
   - If invalid, raise exception: INVALID_USER_ID

4. **Truck Number Validation (if provided):**
   - IV_TRUCK_NO must not be blank if provided
   - If invalid, raise exception: INVALID_TRUCK_NO

#### 3.3.2 Business Validation
1. **Authorization Check:**
   - For Vendor users: Verify user has access to specified vendor
   - For RIL Operations: Verify user has authorization for Business/Sub-Business
   - If unauthorized, raise exception: NO_AUTHORITY

2. **Data Existence:**
   - If no records found matching criteria, return empty table (not an error)
   - Log informational message if no data found

---

### 3.4 Processing Logic

#### 3.4.1 Step 1: Determine User Context
- Based on IV_USER_TYPE parameter:
  - If IV_USER_TYPE = 'V' → Apply Vendor-specific logic
  - If IV_USER_TYPE = 'R' → Apply RIL Operations-specific logic
- Store user context in internal variable for subsequent processing

#### 3.4.2 Step 2: Build Selection Criteria
- Base WHERE clause conditions:
  - CREATED_DATE BETWEEN IV_FROM_DATE AND IV_TO_DATE
  - TRIP_STATUS IN ('04', '06')
  - If IV_TRUCK_NO is provided: TRUCK_NO = IV_TRUCK_NO

#### 3.4.3 Step 3: Apply Role-Based Data Restriction

**For Vendor Users (IV_USER_TYPE = 'V'):**
- Add condition: LIFNR = IV_USER_ID
- This ensures vendor can only see their own trips
- No additional Business/Sub-Business filtering required

**For RIL Operations Users (IV_USER_TYPE = 'R'):**
- Retrieve authorized Business codes for user (from authorization object or custom table)
- Retrieve authorized Sub-Business codes for user
- Add conditions:
  - BUSINESS IN (Authorized Business list)
  - SUBBUSINESS IN (Authorized Sub-Business list)
- If user has no authorized Business/Sub-Business, return empty result

#### 3.4.4 Step 4: Execute Database Query
- Perform SELECT statement on ZSCE_CTD_HDR table
- Use optimized WHERE clause with all conditions
- Select only required fields (not SELECT *)
- Store results in internal table

#### 3.4.5 Step 5: Populate Output Table
- Loop through selected records
- Map fields to output structure ET_TRIP_HDR
- Append to output table
- Clear output table before populating (ensure clean output)

---

### 3.5 Error Handling

#### 3.5.1 Exception Definitions

| Exception Name | Error Code | Error Message | Trigger Condition |
|----------------|------------|---------------|-------------------|
| INVALID_USER_TYPE | E001 | Invalid user type. Must be 'V' or 'R' | IV_USER_TYPE not 'V' or 'R' |
| INVALID_DATE_RANGE | E002 | Invalid date range. From Date must be <= To Date | IV_FROM_DATE > IV_TO_DATE |
| INVALID_USER_ID | E003 | Invalid user ID provided | IV_USER_ID is blank or invalid |
| INVALID_TRUCK_NO | E004 | Invalid truck number format | IV_TRUCK_NO provided but invalid |
| NO_AUTHORITY | E005 | User does not have authorization to access this data | Authorization check fails |
| DATABASE_ERROR | E006 | Database error occurred while fetching data | Database operation fails |

#### 3.5.2 Error Handling Behavior
- All exceptions must be raised using RAISE statement
- Calling program should handle exceptions appropriately
- No data should be returned in ET_TRIP_HDR if exception is raised
- Error messages should be descriptive and user-friendly

---

### 3.6 Success Criteria

#### 3.6.1 Successful Execution
- Function Module completes without exceptions
- Output table ET_TRIP_HDR is populated with matching records (may be empty if no data found)
- All records in output are filtered according to user role and authorization
- Performance: Response time < 3 seconds for typical queries

#### 3.6.2 Response Structure
- **Success:** ET_TRIP_HDR contains filtered trip header records
- **No Data Found:** ET_TRIP_HDR is empty (not an error condition)
- **Error:** Exception is raised, ET_TRIP_HDR is empty

---

### 3.7 Edge Cases

#### 3.7.1 No Records Found
- **Scenario:** No trips match the selection criteria
- **Handling:** Return empty table ET_TRIP_HDR (not an error)
- **Note:** This is a valid business scenario, not an error

#### 3.7.2 User Has No Authorization
- **Scenario:** RIL Operations user has no authorized Business/Sub-Business
- **Handling:** Return empty table ET_TRIP_HDR (not an error, but log for audit)

#### 3.7.3 Large Date Range
- **Scenario:** User selects very large date range (e.g., 1 year)
- **Handling:** 
  - Return all matching records (no artificial limit)
  - Performance may be slower, but acceptable
  - Consider adding warning if result set is very large (>1000 records)

#### 3.7.4 Multiple Trucks for Vendor
- **Scenario:** Vendor has trips for multiple trucks
- **Handling:** Return all trips for vendor (unless IV_TRUCK_NO is specified)

#### 3.7.5 Status Change During Query
- **Scenario:** Trip status changes while query is executing
- **Handling:** Use consistent read (if required) or accept eventual consistency

---

### 3.8 Dependencies

#### 3.8.1 Database Tables
- **ZSCE_CTD_HDR:** Primary source table for trip header data
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
- None (standalone FM)

---

### 3.9 Performance Requirements

#### 3.9.1 Response Time
- **Target:** < 3 seconds for typical queries (date range: 1 month, < 1000 records)
- **Maximum:** < 10 seconds for large queries (date range: 1 year, < 10000 records)

#### 3.9.2 Throughput
- **Target:** Support concurrent calls from multiple users
- **Expected Load:** 10-50 concurrent calls during peak hours

#### 3.9.3 Resource Constraints
- **Database Time:** < 50% of total runtime
- **Memory:** Efficient use of internal tables (avoid SELECT *)
- **Network:** Minimize data transfer (select only required fields)

#### 3.9.4 Optimization Strategies
- Use indexed fields in WHERE clause (CREATED_DATE, TRIP_STATUS, LIFNR, TRUCK_NO)
- Select only required fields (not SELECT *)
- Use proper WHERE clause conditions (avoid IS NOT INITIAL)
- Consider database hints if performance issues arise

---

### 3.10 Security Requirements

#### 3.10.1 Authentication
- **Required:** Yes (SAP user authentication)
- **Method:** Standard SAP authentication (user must be logged in)

#### 3.10.2 Authorization
- **Required:** Yes (role-based authorization)
- **Vendor Users:** Can only access trips for their own vendor number
- **RIL Operations:** Can only access trips for authorized Business/Sub-Business
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

**AT-001: Vendor User - Valid Data**
- **Input:** IV_USER_TYPE='V', IV_USER_ID='0000012345', IV_FROM_DATE='20240101', IV_TO_DATE='20240131', IV_TRUCK_NO=''
- **Expected:** Returns all trips for vendor 0000012345 with status '04' or '06' in date range
- **Validation:** All returned records have LIFNR='0000012345' and status in ('04','06')

**AT-002: RIL Operations User - Valid Data**
- **Input:** IV_USER_TYPE='R', IV_USER_ID='USER001', IV_FROM_DATE='20240101', IV_TO_DATE='20240131', IV_TRUCK_NO=''
- **Expected:** Returns all trips for authorized Business/Sub-Business with status '04' or '06' in date range
- **Validation:** All returned records have BUSINESS/SUBBUSINESS in authorized list

**AT-003: With Truck Number Filter**
- **Input:** IV_USER_TYPE='V', IV_USER_ID='0000012345', IV_FROM_DATE='20240101', IV_TO_DATE='20240131', IV_TRUCK_NO='TRUCK001'
- **Expected:** Returns trips for vendor 0000012345 with truck TRUCK001 only
- **Validation:** All returned records have TRUCK_NO='TRUCK001'

**AT-004: No Data Found**
- **Input:** IV_USER_TYPE='V', IV_USER_ID='0000012345', IV_FROM_DATE='20250101', IV_TO_DATE='20250131'
- **Expected:** Returns empty table (no exception)
- **Validation:** ET_TRIP_HDR is empty, no exception raised

#### 3.11.2 Error Path Tests

**AT-005: Invalid User Type**
- **Input:** IV_USER_TYPE='X' (invalid)
- **Expected:** Exception INVALID_USER_TYPE raised
- **Validation:** Exception code and message correct

**AT-006: Invalid Date Range**
- **Input:** IV_FROM_DATE='20240131', IV_TO_DATE='20240101' (FROM > TO)
- **Expected:** Exception INVALID_DATE_RANGE raised
- **Validation:** Exception code and message correct

**AT-007: Blank User ID**
- **Input:** IV_USER_ID='' (blank)
- **Expected:** Exception INVALID_USER_ID raised
- **Validation:** Exception code and message correct

**AT-008: No Authorization**
- **Input:** IV_USER_TYPE='R', IV_USER_ID='UNAUTH_USER' (user with no Business authorization)
- **Expected:** Exception NO_AUTHORITY raised OR empty table returned
- **Validation:** Appropriate handling based on business rule

#### 3.11.3 Edge Case Tests

**AT-009: Large Date Range**
- **Input:** IV_FROM_DATE='20200101', IV_TO_DATE='20241231' (1 year range)
- **Expected:** Returns all matching records (may be slow but should complete)
- **Validation:** Performance acceptable, all records returned

**AT-010: Single Day Range**
- **Input:** IV_FROM_DATE='20240115', IV_TO_DATE='20240115' (same day)
- **Expected:** Returns trips created on that specific day
- **Validation:** All records have CREATED_DATE='20240115'

**AT-011: Multiple Status Filtering**
- **Input:** Standard input with trips having both status '04' and '06'
- **Expected:** Returns trips with both statuses
- **Validation:** Status values are '04' or '06' only

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
ZSCM_CTD_GETTRIPHDR (FM)
    ↓
ZSCE_CTD_HDR (Database Table)
    ↓
ET_TRIP_HDR (Output Table)
    ↓
Calling Program
```

---

## 5. Future Enhancements (Out of Scope)

- Pagination support for large result sets
- Additional filtering options (e.g., by Business, Sub-Business directly)
- Sorting options in output
- Delta retrieval (only changed records since last call)
- Caching mechanism for frequently accessed data

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

