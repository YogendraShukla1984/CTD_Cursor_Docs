# Functional Specification
## CTD Trip Details Enrichment Program

---

**Document Information**

| Field | Value |
|-------|-------|
| **Document Type** | Functional Specification |
| **Program Name** | ZSCM_CTD_ENRICHTRIPDETAILS |
| **Transaction Code** | ZCTD_ENRICH_TRIPDETAILS |
| **Function Module** | ZSCM_CTD_ENRICHTRIPDETAILS |
| **Version** | 1.0 |
| **Date** | [Current Date] |
| **Author** | [Author Name] |

---

## 1. Executive Summary

### 1.1 Purpose
This document defines the functional requirements for the CTD (Converted to Dedicated) Trip Details Enrichment Program. The program enriches completed trip records with additional attributes, validates trip completeness, inserts empty legs where required, and updates trip status to reflect the next stage in the CTD lifecycle.

### 1.2 Business Objective
- Automate the enrichment of CTD trip details with derived attributes
- Ensure trip completeness by validating source and destination dates
- Automatically insert empty legs between loaded legs when required
- Classify legs as loaded or empty based on shipment assignment
- Update trip status to reflect enrichment completion
- Support simulation mode for testing without database updates

### 1.3 Scope
- **In Scope:**
  - Enrichment of completed trips (Status '03')
  - Validation of trip leg data
  - Empty leg insertion logic
  - Trip status update (from '03' to '04')
  - Simulation/test run mode
  - ALV output display

- **Out of Scope:**
  - Creation of new trips
  - Modification of trip header data beyond status update
  - Integration with external systems
  - Batch job scheduling configuration

---

## 2. Business Process Overview

### 2.1 Process Flow
1. User executes transaction ZCTD_ENRICH_TRIPDETAILS
2. User enters selection criteria (Date Range, Optional Trip Numbers)
3. User optionally selects Test Run mode
4. System fetches completed trips based on criteria
5. System validates trip leg data
6. System enriches loaded legs with shipment details
7. System determines and inserts empty legs
8. System classifies legs (Loaded/Empty)
9. System fetches material and region details
10. System displays results in ALV (or updates database if not test run)
11. System updates trip status for successfully processed trips

### 2.2 User Roles
- **Primary Users:** Supply Chain Management Team, Logistics Coordinators
- **Authorization:** Transaction code authorization required (S_TCODE)

---

## 3. Functional Requirements

### 3.1 Selection Screen Requirements

#### 3.1.1 Mandatory Inputs

**From Date (P_FROM_DATE)**
- **Type:** DATS (Date)
- **Length:** 8 characters
- **Format:** DD.MM.YYYY or YYYYMMDD
- **Validation:** 
  - Must not be blank
  - Must be valid date
  - Must be less than or equal to To Date
- **Description:** Start date for trip selection based on CREATED_DATE

**To Date (P_TO_DATE)**
- **Type:** DATS (Date)
- **Length:** 8 characters
- **Format:** DD.MM.YYYY or YYYYMMDD
- **Validation:**
  - Must not be blank
  - Must be valid date
  - Must be greater than or equal to From Date
- **Description:** End date for trip selection based on CREATED_DATE

#### 3.1.2 Optional Inputs

**Trip Number (S_TRIP_NO)**
- **Type:** CHAR14 (Select-Options)
- **Length:** 14 characters
- **Multiple Selection:** Yes
- **Validation:**
  - If provided, selected trips must have CREATED_DATE within date range
  - If trip CREATED_DATE is outside range, display error: "Selected Trip does not fall within the given date range"
- **Description:** Specific trip numbers to process (if left blank, all trips in date range are processed)

#### 3.1.3 Additional Options

**Test Run / Simulation Mode (P_TESTRUN)**
- **Type:** CHAR1 (Checkbox)
- **Values:** 'X' = Test Run, Blank = Production Run
- **Default:** Blank (Production Run)
- **Description:** 
  - When selected: Executes all logic without committing database updates
  - When not selected: Executes logic and commits database updates

#### 3.1.4 Execution Mode
- **Foreground Execution:** Supported
- **Background Execution:** Supported (via SM36/SM37)

---

### 3.2 Data Selection and Validation

#### 3.2.1 Trip Header Selection (Step 1)

**Source Table:** ZSCE_CTD_HDR

**Selection Criteria:**
- TRIP_STATUS = '03' (Completed)
- CREATED_DATE BETWEEN P_FROM_DATE AND P_TO_DATE
- If S_TRIP_NO is provided: TRIP_NO IN S_TRIP_NO

**Validation:**
- If specific trip numbers are provided, validate that CREATED_DATE falls within date range
- If validation fails: Exclude trip from processing and log error

**Output:** List of valid trip headers for processing

#### 3.2.2 Trip Leg Selection (Step 2)

**Source Table:** ZSCE_CTD_ITM

**Selection Criteria:**
- TRIP_NO IN (selected trip headers)
- Order by: TRIP_NO, COUNTER (ascending)

**Key Fields Extracted:**
- LIFNR (Vendor)
- TRUCK_NO (Truck Number)
- TRIP_NO (Trip Number)
- COUNTER (Leg Counter)
- SHNUMBER (Shipment Number)
- SOURCE_DATE (Source Date)
- DEST_DATE (Destination Date)
- MVT_TYPE (Movement Type)
- AREA (Area Code)

**Validation Rule:**
- If any leg has SOURCE_DATE or DEST_DATE blank:
  - Exclude entire trip from further processing
  - Log validation error: "Trip excluded: Missing Source or Destination Date"
  - Do not perform enrichment for that trip

**Output:** Valid trip legs grouped by trip

---

### 3.3 Enrichment Logic

#### 3.3.1 Loaded Leg Shipment Enrichment (Step 3)

**Condition:** Process legs where SHNUMBER is populated

**Data Sources:**
1. **OIGSS Table** (TSTYP = '1')
   - Route information
   - Distance
   - Source Zone
   - Destination Zone
   - Source Address (ADRNR)
   - Destination Address (ADRNZ)

2. **YTTSTX0001 Table** (TRK_PURPOS = 'R')
   - MG_EXIT_DT (for PO/STO movements)

3. **YTTSTX0002 Table**
   - REPORT_NO (for SO/STO movements)

4. **Function Module:** Z_SCM_GET_BUSINESS
   - BUSINESS_ID
   - SUBBUSINESS_ID

**Date Determination Logic:**

**DEST_EXIT_DATE:**
- For SO (Sales Order): Use DEST_DATE from leg
- For PO (Purchase Order) / STO (Stock Transfer Order): Use MG_EXIT_DT from YTTSTX0001 where TRK_PURPOS = 'R'

**SOURCE_ENT_DATE:**
- For SO / STO: Use PP_ENTR_DT from YTTSTX0001 (accessed via REPORT_NO from YTTSTX0002)
- For PO: Use SOURCE_DATE from leg

**Enriched Fields:**
- Route
- Distance
- Source Zone
- Destination Zone
- Source Address
- Destination Address
- DEST_EXIT_DATE
- SOURCE_ENT_DATE
- BUSINESS_ID
- SUBBUSINESS_ID

#### 3.3.2 Empty Leg Determination and Insertion (Step 4)

**Processing Logic:**
1. Sort legs by COUNTER (ascending) within each trip
2. Compare consecutive legs
3. Determine if empty leg is required

**Empty Leg Insertion Conditions:**
- Insert empty leg when:
  - Destination Area of preceding leg is blank, OR
  - Destination Area of preceding leg ≠ Source Area of subsequent leg

**Empty Leg Derivation:**
- COUNTER: Previous COUNTER + 1 (adjust subsequent leg counters)
- SOURCE_ZONE: Destination Zone of preceding leg
- DEST_ZONE: Source Zone of subsequent leg
- SOURCE_DATE: DEST_EXIT_DATE of preceding leg
- DEST_DATE: SOURCE_ENT_DATE of subsequent leg
- Route: From TROLZ table (VSBED = '03', TRAGR = '0006')
- Distance: From TVRO table
- SHNUMBER: Blank (indicates empty leg)
- LEG_TYPE: 'E' (Empty)

**Note:** Empty legs are added to internal table only and persisted during final database commit.

#### 3.3.3 Leg Classification (Step 5)

**Classification Rules:**
- **Loaded Leg (LEG_TYPE = 'L'):** SHNUMBER is populated
- **Empty Leg (LEG_TYPE = 'E'):** SHNUMBER is blank

**Validation:**
- For Empty Legs, validate that Route and Distance are populated
- If validation fails:
  - Exclude trip from further processing
  - Set CTD_RULEENG_REMARKS = 'Empty Leg Route / Distance missing'
  - Log validation error

#### 3.3.4 Material and Region Determination (Step 6)

**Material Determination (Loaded Legs Only):**
1. Use SHNUMBER to query OIGSI table
2. Extract DOC_NUMBER (Delivery Number)
3. Use DOC_NUMBER as VBELN to query LIPS table
4. Extract MATNR (Material Number)

**Region Determination (Loaded Legs Only):**
1. Use ADRNR (Source Address) to query ADRC table
2. Extract SOURCE_REGION
3. Use ADRNZ (Destination Address) to query ADRC table
4. Extract DEST_REGION

---

### 3.4 Output and Display Requirements

#### 3.4.1 ALV Output Structure

**Display Fields:**
- LIFNR (Vendor)
- TRUCK_NO (Truck Number)
- TRIP_NO (Trip Number)
- COUNTER (Leg Counter)
- LEG_TYPE (Leg Type: L/E)
- SHNUMBER (Shipment Number)
- SOURCE_DATE (Source Date)
- DEST_DATE (Destination Date)
- SOURCE_ZONE (Source Zone)
- DEST_ZONE (Destination Zone)
- ROUTE (Route)
- DISTANCE (Distance)
- MATNR (Material Number - Loaded Legs)
- SOURCE_REGION (Source Region - Loaded Legs)
- DEST_REGION (Destination Region - Loaded Legs)
- BUSINESS_ID (Business ID - Loaded Legs)
- SUBBUSINESS_ID (Sub Business ID - Loaded Legs)
- TRIP_STATUS (Trip Status)
- CTD_RULEENG_REMARKS (Validation Remarks)

**Sorting:**
- Primary: LIFNR (ascending)
- Secondary: TRUCK_NO (ascending)
- Tertiary: TRIP_NO (ascending)
- Quaternary: COUNTER (ascending)

**Color Coding:**
- Records with validation issues: Display in RED
- Records without issues: Default color

**ALV Features:**
- Filter functionality
- Sort functionality
- Download to Excel/CSV
- Print functionality
- Column selection
- Layout save/load

#### 3.4.2 Log Output

**Log Structure (ET_LOG):**
- Trip Number
- Leg Counter (if applicable)
- Message Type (E/W/I/S)
- Message Text
- Timestamp

**Log Categories:**
- **Information:** Processing started, trip processed successfully
- **Warning:** Validation warnings, missing optional data
- **Error:** Validation failures, data inconsistencies, processing errors

---

### 3.5 Database Update Requirements

#### 3.5.1 Update Conditions

**Update ZSCE_CTD_ITM:**
- Update enriched leg-level data for all processed legs
- Include newly inserted empty legs
- Update only if trip passed all validations

**Update ZSCE_CTD_HDR:**
- Update trip-level derived details
- Update TRIP_STATUS from '03' to '04' (if trip passed all validations)

#### 3.5.2 Trip Status Management

**Status Transition:**
- **Initial Status:** '03' (Completed)
- **Updated Status:** '04' (Trip Details Updated / Pending for Transporter Trip Confirmation)

**Status Update Conditions:**
- Trip must pass all validations
- Enrichment logic must complete successfully
- All legs must be processed without errors

**Status Retention:**
- Trips excluded due to validation errors retain original status '03'

#### 3.5.3 Test Run vs Production Run

**Test Run Mode (I_TEST_RUN = 'X'):**
- Execute all enrichment logic
- Perform all validations
- Display results in ALV
- Display derived trip status '04' in output (for reference)
- **NO database updates**
- **NO commit**

**Production Run Mode (I_TEST_RUN = Blank):**
- Execute all enrichment logic
- Perform all validations
- Display results in ALV
- **Perform database updates**
- **Commit once after all eligible trips are processed** (single LUW)

---

### 3.6 Error Handling and Validation

#### 3.6.1 Validation Rules

1. **Date Range Validation:**
   - From Date must be ≤ To Date
   - Trip CREATED_DATE must be within date range

2. **Trip Leg Validation:**
   - SOURCE_DATE must not be blank
   - DEST_DATE must not be blank
   - Empty legs must have Route and Distance populated

3. **Data Consistency Validation:**
   - Shipment number must exist in OIGSS
   - Address numbers must exist in ADRC
   - Material must exist in LIPS

#### 3.6.2 Error Messages

| Error Code | Message Text | Type |
|------------|--------------|------|
| E001 | From Date must be less than or equal to To Date | Error |
| E002 | Selected Trip does not fall within the given date range | Error |
| E003 | Trip excluded: Missing Source or Destination Date | Error |
| E004 | Trip excluded: Empty Leg Route / Distance missing | Error |
| E005 | Shipment not found in OIGSS | Error |
| E006 | Address not found in ADRC | Error |
| E007 | Material not found in LIPS | Error |
| W001 | Optional data missing for leg | Warning |
| I001 | Processing started | Information |
| I002 | Trip processed successfully | Information |
| S001 | Enrichment completed | Success |

---

### 3.7 Performance Requirements

- **Online Response Time:** < 3 seconds for up to 100 trips
- **Batch Processing:** < 5 minutes for up to 10,000 trips
- **Database Time:** < 50% of total runtime
- **Memory Usage:** Optimized to handle large datasets

---

### 3.8 Authorization Requirements

- **Transaction Authorization:** S_TCODE object, ID 'TCD' = 'ZCTD_ENRICH_TRIPDETAILS'
- **Authorization Check:** Must be performed at program initialization
- **Error Handling:** If authorization fails, display error message and exit program

---

## 4. Business Rules

### 4.1 Trip Selection Rules
- Only trips with status '03' (Completed) are eligible for enrichment
- Trips must have CREATED_DATE within specified date range
- If specific trip numbers are provided, they must also meet date range criteria

### 4.2 Leg Validation Rules
- All legs must have valid SOURCE_DATE and DEST_DATE
- If any leg fails validation, entire trip is excluded

### 4.3 Empty Leg Rules
- Empty legs are inserted automatically when area mismatch is detected
- Empty legs must have valid Route and Distance
- Empty legs are assigned LEG_TYPE = 'E'

### 4.4 Status Update Rules
- Status is updated only if trip passes all validations
- Status update is atomic (all or nothing per trip)
- Failed trips retain original status

---

## 5. User Interface Requirements

### 5.1 Selection Screen Layout

```
┌─────────────────────────────────────────────────────────┐
│ CTD Trip Details Enrichment                             │
├─────────────────────────────────────────────────────────┤
│                                                           │
│ Selection Criteria                                        │
│ ┌─────────────────────────────────────────────────────┐ │
│ │ From Date        : [DD.MM.YYYY]  *                  │ │
│ │ To Date          : [DD.MM.YYYY]  *                  │ │
│ │ Trip Number      : [____] [____] [Multiple]         │ │
│ │                                                       │ │
│ │ ☐ Test Run / Simulation Mode                        │ │
│ └─────────────────────────────────────────────────────┘ │
│                                                           │
│ [Execute]  [Cancel]                                       │
└─────────────────────────────────────────────────────────┘
```

### 5.2 ALV Display Layout
- Standard ALV Grid display
- Toolbar with standard ALV functions
- Color-coded rows for validation issues
- Column headers in user-friendly language

---

## 6. Integration Points

### 6.1 Function Module: Z_SCM_GET_BUSINESS
- **Purpose:** Retrieve business information for shipment
- **Input:** SHNUMBER
- **Output:** BUSINESS_ID, SUBBUSINESS_ID

### 6.2 Standard Tables
- **OIGSS:** Shipment header data
- **OIGSI:** Shipment item data
- **YTTSTX0001:** Trip status data
- **YTTSTX0002:** Trip report data
- **LIPS:** Delivery item data
- **ADRC:** Address data
- **TROLZ:** Route determination
- **TVRO:** Route distance data

---

## 7. Test Scenarios

### 7.1 Test Case 1: Basic Enrichment
- **Input:** Date range with completed trips
- **Expected:** All trips enriched, status updated to '04'

### 7.2 Test Case 2: Test Run Mode
- **Input:** Date range with Test Run selected
- **Expected:** All logic executed, no database updates, status '04' shown in output

### 7.3 Test Case 3: Trip Number Filter
- **Input:** Specific trip numbers within date range
- **Expected:** Only selected trips processed

### 7.4 Test Case 4: Validation Failure
- **Input:** Trip with missing SOURCE_DATE
- **Expected:** Trip excluded, error logged, status remains '03'

### 7.5 Test Case 5: Empty Leg Insertion
- **Input:** Trip with area mismatch between legs
- **Expected:** Empty leg inserted between legs

### 7.6 Test Case 6: Date Range Validation
- **Input:** Trip number with CREATED_DATE outside date range
- **Expected:** Error message displayed, trip excluded

---

## 8. Appendix

### 8.1 Glossary
- **CTD:** Converted to Dedicated
- **Leg:** A segment of a trip (loaded or empty)
- **Loaded Leg:** Trip leg with assigned shipment
- **Empty Leg:** Trip leg without shipment (deadhead)
- **Enrichment:** Addition of derived attributes to trip data
- **LUW:** Logical Unit of Work

### 8.2 Related Documents
- BRD: CTD_BRD1.MD
- Technical Specification: CTD_Technical_Specification.md
- ABAP Coding Guidelines: ABAP Code Rules/

---

**Document Approval**

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Business Analyst | | | |
| Functional Lead | | | |
| Project Manager | | | |

---

**Revision History**

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | [Date] | [Author] | Initial version |

