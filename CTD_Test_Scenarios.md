# Test Scenarios and Sample Data
## CTD Trip Details Enrichment Program

---

**Document Information**

| Field | Value |
|-------|-------|
| **Document Type** | Test Scenarios |
| **Program Name** | ZSCM_CTD_ENRICHTRIPDETAILS |
| **Version** | 1.0 |
| **Date** | [Current Date] |
| **Author** | [Author Name] |

---

## 1. Test Data Setup

### 1.1 Sample Data for ZSCE_CTD_HDR (Trip Header Table)

| TRIP_NO | LIFNR | TRUCK_NO | TRIP_STATUS | CREATED_DATE |
|---------|-------|----------|-------------|--------------|
| TRIP0000001 | VENDOR01 | TRUCK001 | 03 | 20240115 |
| TRIP0000002 | VENDOR01 | TRUCK002 | 03 | 20240116 |
| TRIP0000003 | VENDOR02 | TRUCK003 | 03 | 20240117 |
| TRIP0000004 | VENDOR01 | TRUCK001 | 03 | 20240118 |
| TRIP0000005 | VENDOR03 | TRUCK004 | 03 | 20240119 |
| TRIP0000006 | VENDOR02 | TRUCK005 | 03 | 20240120 |
| TRIP0000007 | VENDOR01 | TRUCK001 | 02 | 20240121 |
| TRIP0000008 | VENDOR04 | TRUCK006 | 03 | 20240201 |

**Notes:**
- TRIP0000001 to TRIP0000006: Valid completed trips (Status '03') within test date range
- TRIP0000007: Invalid trip (Status '02' - not completed)
- TRIP0000008: Valid trip but outside default test date range

---

### 1.2 Sample Data for ZSCE_CTD_ITM (Trip Item/Leg Table)

#### Trip TRIP0000001 - Valid Trip with Loaded Legs
| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|
| TRIP0000001 | 1 | VENDOR01 | TRUCK001 | SHIP001 | 20240115 | 20240116 | SO | AREA01 | ADR001 | ADR002 |
| TRIP0000001 | 2 | VENDOR01 | TRUCK001 | SHIP002 | 20240116 | 20240117 | SO | AREA02 | ADR003 | ADR004 |
| TRIP0000001 | 3 | VENDOR01 | TRUCK001 | SHIP003 | 20240117 | 20240118 | PO | AREA03 | ADR005 | ADR006 |

#### Trip TRIP0000002 - Valid Trip with Area Mismatch (Requires Empty Leg)
| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|
| TRIP0000002 | 1 | VENDOR01 | TRUCK002 | SHIP004 | 20240116 | 20240117 | SO | AREA01 | ADR007 | ADR008 |
| TRIP0000002 | 2 | VENDOR01 | TRUCK002 | SHIP005 | 20240118 | 20240119 | STO | AREA05 | ADR009 | ADR010 |

**Note:** AREA01 ≠ AREA05, so empty leg should be inserted between leg 1 and leg 2

#### Trip TRIP0000003 - Valid Trip with Empty Destination Area
| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|
| TRIP0000003 | 1 | VENDOR02 | TRUCK003 | SHIP006 | 20240117 | 20240118 | SO | AREA02 | ADR011 | ADR012 |
| TRIP0000003 | 2 | VENDOR02 | TRUCK003 | SHIP007 | 20240119 | 20240120 | PO | AREA04 | ADR013 | ADR014 |

**Note:** Leg 1 has blank destination area, so empty leg should be inserted

#### Trip TRIP0000004 - Invalid Trip (Missing Source Date)
| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|
| TRIP0000004 | 1 | VENDOR01 | TRUCK001 | SHIP008 | 00000000 | 20240118 | SO | AREA01 | ADR015 | ADR016 |

**Note:** SOURCE_DATE is blank - trip should be excluded

#### Trip TRIP0000005 - Invalid Trip (Missing Destination Date)
| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|
| TRIP0000005 | 1 | VENDOR03 | TRUCK004 | SHIP009 | 20240119 | 00000000 | SO | AREA02 | ADR017 | ADR018 |

**Note:** DEST_DATE is blank - trip should be excluded

#### Trip TRIP0000006 - Valid Trip with Mixed Loaded and Empty Legs
| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|
| TRIP0000006 | 1 | VENDOR02 | TRUCK005 | SHIP010 | 20240120 | 20240121 | SO | AREA03 | ADR019 | ADR020 |
| TRIP0000006 | 2 | VENDOR02 | TRUCK005 | | 20240122 | 20240123 | | AREA04 | | |

**Note:** Leg 2 has no shipment (empty leg already exists)

---

### 1.3 Sample Data for OIGSS (Shipment Header)

| SHNUMBER | TSTYP | ROUTE | DISTANCE | SOURCE_ZONE | DEST_ZONE | ADRNR | ADRNZ |
|----------|-------|-------|----------|-------------|-----------|-------|-------|
| SHIP001 | 1 | ROUTE01 | 150 | ZONE01 | ZONE02 | ADR001 | ADR002 |
| SHIP002 | 1 | ROUTE02 | 200 | ZONE02 | ZONE03 | ADR003 | ADR004 |
| SHIP003 | 1 | ROUTE03 | 180 | ZONE03 | ZONE04 | ADR005 | ADR006 |
| SHIP004 | 1 | ROUTE04 | 120 | ZONE01 | ZONE02 | ADR007 | ADR008 |
| SHIP005 | 1 | ROUTE05 | 250 | ZONE05 | ZONE06 | ADR009 | ADR010 |
| SHIP006 | 1 | ROUTE06 | 175 | ZONE02 | ZONE03 | ADR011 | ADR012 |
| SHIP007 | 1 | ROUTE07 | 190 | ZONE04 | ZONE05 | ADR013 | ADR014 |
| SHIP008 | 1 | ROUTE08 | 160 | ZONE01 | ZONE02 | ADR015 | ADR016 |
| SHIP009 | 1 | ROUTE09 | 140 | ZONE02 | ZONE03 | ADR017 | ADR018 |
| SHIP010 | 1 | ROUTE10 | 165 | ZONE03 | ZONE04 | ADR019 | ADR020 |

---

### 1.4 Sample Data for OIGSI (Shipment Item)

| SHNUMBER | DOC_NUMBER |
|----------|------------|
| SHIP001 | DELV001 |
| SHIP002 | DELV002 |
| SHIP003 | DELV003 |
| SHIP004 | DELV004 |
| SHIP005 | DELV005 |
| SHIP006 | DELV006 |
| SHIP007 | DELV007 |
| SHIP008 | DELV008 |
| SHIP009 | DELV009 |
| SHIP010 | DELV010 |

---

### 1.5 Sample Data for LIPS (Delivery Item)

| VBELN | MATNR |
|-------|-------|
| DELV001 | MAT001 |
| DELV002 | MAT002 |
| DELV003 | MAT003 |
| DELV004 | MAT004 |
| DELV005 | MAT005 |
| DELV006 | MAT006 |
| DELV007 | MAT007 |
| DELV008 | MAT008 |
| DELV009 | MAT009 |
| DELV010 | MAT010 |

---

### 1.6 Sample Data for ADRC (Address)

| ADDRNUMBER | REGION |
|------------|--------|
| ADR001 | REGION01 |
| ADR002 | REGION02 |
| ADR003 | REGION03 |
| ADR004 | REGION04 |
| ADR005 | REGION05 |
| ADR006 | REGION06 |
| ADR007 | REGION01 |
| ADR008 | REGION02 |
| ADR009 | REGION05 |
| ADR010 | REGION06 |
| ADR011 | REGION03 |
| ADR012 | REGION04 |
| ADR013 | REGION04 |
| ADR014 | REGION05 |
| ADR015 | REGION01 |
| ADR016 | REGION02 |
| ADR017 | REGION03 |
| ADR018 | REGION04 |
| ADR019 | REGION05 |
| ADR020 | REGION06 |

---

### 1.7 Sample Data for YTTSTX0001 (Trip Status - PO/STO)

| SHNUMBER | TRK_PURPOS | MG_EXIT_DT | PP_ENTR_DT |
|----------|------------|------------|-------------|
| SHIP003 | R | 20240118 | 20240115 |
| SHIP005 | R | 20240119 | 20240116 |
| SHIP007 | R | 20240120 | 20240117 |

---

### 1.8 Sample Data for YTTSTX0002 (Trip Report)

| SHNUMBER | REPORT_NO |
|----------|-----------|
| SHIP001 | REP001 |
| SHIP002 | REP002 |
| SHIP004 | REP004 |
| SHIP005 | REP005 |
| SHIP006 | REP006 |
| SHIP010 | REP010 |

---

### 1.9 Sample Data for TROLZ (Route Determination)

| VSBED | TRAGR | ROUTE |
|-------|-------|-------|
| 03 | 0006 | ROUTE_EMPTY_01 |
| 03 | 0006 | ROUTE_EMPTY_02 |
| 03 | 0006 | ROUTE_EMPTY_03 |

---

### 1.10 Sample Data for TVRO (Route Distance)

| ROUTE | DISTANCE |
|-------|----------|
| ROUTE_EMPTY_01 | 50 |
| ROUTE_EMPTY_02 | 75 |
| ROUTE_EMPTY_03 | 60 |
| ROUTE01 | 150 |
| ROUTE02 | 200 |
| ROUTE03 | 180 |
| ROUTE04 | 120 |
| ROUTE05 | 250 |
| ROUTE06 | 175 |
| ROUTE07 | 190 |
| ROUTE08 | 160 |
| ROUTE09 | 140 |
| ROUTE10 | 165 |

---

### 1.11 Function Module Z_SCM_GET_BUSINESS - Expected Output

| SHNUMBER | BUSINESS_ID | SUBBUSINESS_ID |
|----------|-------------|----------------|
| SHIP001 | BUS001 | SUB001 |
| SHIP002 | BUS002 | SUB002 |
| SHIP003 | BUS001 | SUB003 |
| SHIP004 | BUS002 | SUB001 |
| SHIP005 | BUS003 | SUB002 |
| SHIP006 | BUS001 | SUB001 |
| SHIP007 | BUS002 | SUB003 |
| SHIP008 | BUS001 | SUB002 |
| SHIP009 | BUS003 | SUB001 |
| SHIP010 | BUS002 | SUB002 |

---

## 2. Test Scenarios

### Test Case 1: Basic Enrichment - Valid Trip with Loaded Legs

**Objective:** Verify basic enrichment functionality for a valid trip with all loaded legs

**Test Data:**
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: TRIP0000001 (or leave blank)
- Test Run: Unchecked

**Expected Results:**
- Trip TRIP0000001 should be processed
- All 3 legs should be enriched with:
  - Route, Distance from OIGSS
  - Source Zone, Destination Zone from OIGSS
  - Material from LIPS (via OIGSI)
  - Source Region, Destination Region from ADRC
  - Business ID, Sub Business ID from Z_SCM_GET_BUSINESS
  - DEST_EXIT_DATE and SOURCE_ENT_DATE based on movement type
- Leg Type should be 'L' for all legs
- Trip Status should be updated from '03' to '04'
- No empty legs should be inserted (areas are consecutive)

**Validation Points:**
- Check ALV output shows all enriched fields
- Verify database updates in ZSCE_CTD_ITM
- Verify trip status update in ZSCE_CTD_HDR

---

### Test Case 2: Empty Leg Insertion - Area Mismatch

**Objective:** Verify empty leg insertion when destination area ≠ source area of next leg

**Test Data:**
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: TRIP0000002
- Test Run: Unchecked

**Expected Results:**
- Trip TRIP0000002 should be processed
- Leg 1: AREA01 → (should have destination zone from SHIP004)
- **Empty Leg Inserted:** Between leg 1 and leg 2
  - COUNTER: 2 (leg 2 becomes 3)
  - SOURCE_ZONE: Destination Zone of leg 1
  - DEST_ZONE: Source Zone of leg 2
  - SOURCE_DATE: DEST_EXIT_DATE of leg 1
  - DEST_DATE: SOURCE_ENT_DATE of leg 2
  - Route and Distance from TROLZ/TVRO
  - Leg Type: 'E'
- Leg 2 (now leg 3): AREA05 → (should have source zone from SHIP005)
- Trip Status should be updated to '04'

**Validation Points:**
- Verify empty leg is inserted with correct counter
- Verify subsequent leg counters are adjusted
- Verify empty leg has route and distance populated
- Check database for new leg record

---

### Test Case 3: Empty Leg Insertion - Blank Destination Area

**Objective:** Verify empty leg insertion when preceding leg has blank destination area

**Test Data:**
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: TRIP0000003
- Test Run: Unchecked

**Expected Results:**
- Trip TRIP0000003 should be processed
- Leg 1: Has blank destination area
- **Empty Leg Inserted:** Between leg 1 and leg 2
  - SOURCE_ZONE: Should be determined (may be blank if leg 1 has no destination zone)
  - DEST_ZONE: Source Zone of leg 2
- Leg 2: Should be enriched normally
- Trip Status should be updated to '04'

**Validation Points:**
- Verify empty leg insertion logic handles blank areas
- Verify trip is not excluded due to blank area

---

### Test Case 4: Validation Failure - Missing Source Date

**Objective:** Verify trip exclusion when source date is missing

**Test Data:**
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: TRIP0000004
- Test Run: Unchecked

**Expected Results:**
- Trip TRIP0000004 should be **excluded** from processing
- Error log entry: "Trip excluded: Missing Source or Destination Date"
- Trip Status should remain '03' (not updated)
- No database updates for this trip

**Validation Points:**
- Check log for validation error
- Verify trip status unchanged
- Verify no enrichment performed

---

### Test Case 5: Validation Failure - Missing Destination Date

**Objective:** Verify trip exclusion when destination date is missing

**Test Data:**
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: TRIP0000005
- Test Run: Unchecked

**Expected Results:**
- Trip TRIP0000005 should be **excluded** from processing
- Error log entry: "Trip excluded: Missing Source or Destination Date"
- Trip Status should remain '03'
- No database updates

**Validation Points:**
- Check log for validation error
- Verify trip status unchanged

---

### Test Case 6: Test Run Mode - No Database Updates

**Objective:** Verify test run mode executes logic without database updates

**Test Data:**
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: TRIP0000001
- Test Run: **Checked**

**Expected Results:**
- All enrichment logic should execute
- ALV output should display enriched data
- Trip Status in output should show '04' (for reference)
- **NO database updates** should occur
- Trip Status in ZSCE_CTD_HDR should remain '03'
- No updates in ZSCE_CTD_ITM

**Validation Points:**
- Verify database unchanged after test run
- Verify ALV shows expected enriched data
- Check log confirms test run mode

---

### Test Case 7: Date Range Validation

**Objective:** Verify date range validation for specific trip numbers

**Test Data:**
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: TRIP0000008 (CREATED_DATE = 20240201 - outside range)
- Test Run: Unchecked

**Expected Results:**
- Error message: "Selected Trip does not fall within the given date range"
- Trip TRIP0000008 should be excluded
- No processing should occur

**Validation Points:**
- Check error message displayed
- Verify no trips processed

---

### Test Case 8: Multiple Trips Processing

**Objective:** Verify processing of multiple trips in single run

**Test Data:**
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: Leave blank (process all)
- Test Run: Unchecked

**Expected Results:**
- TRIP0000001: Processed successfully, status → '04'
- TRIP0000002: Processed successfully, status → '04'
- TRIP0000003: Processed successfully, status → '04'
- TRIP0000004: Excluded (missing source date), status → '03'
- TRIP0000005: Excluded (missing destination date), status → '03'
- TRIP0000006: Processed successfully, status → '04'
- TRIP0000007: Not selected (status ≠ '03')
- TRIP0000008: Not selected (outside date range)

**Validation Points:**
- Verify all valid trips processed
- Verify invalid trips excluded
- Check log for all processing results
- Verify single commit after all updates

---

### Test Case 9: Empty Leg Route/Distance Validation

**Objective:** Verify validation of empty leg route and distance

**Test Data:**
- Create a trip with area mismatch but route/distance not available in TROLZ/TVRO
- From Date: 15.01.2024
- To Date: 20.01.2024
- Test Run: Unchecked

**Expected Results:**
- Empty leg should be created
- If route/distance missing: Trip should be excluded
- Error log: "Trip excluded: Empty Leg Route / Distance missing"
- CTD_RULEENG_REMARKS should be populated

**Validation Points:**
- Verify trip exclusion when route/distance missing
- Check remarks field populated

---

### Test Case 10: Material and Region Determination

**Objective:** Verify material and region determination for loaded legs

**Test Data:**
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: TRIP0000001
- Test Run: Unchecked

**Expected Results:**
- Leg 1 (SHIP001):
  - Material: MAT001 (via DELV001 from OIGSI → LIPS)
  - Source Region: REGION01 (via ADR001 from ADRC)
  - Destination Region: REGION02 (via ADR002 from ADRC)
- Leg 2 (SHIP002):
  - Material: MAT002 (via DELV002)
  - Source Region: REGION03 (via ADR003)
  - Destination Region: REGION04 (via ADR004)
- Leg 3 (SHIP003):
  - Material: MAT003 (via DELV003)
  - Source Region: REGION05 (via ADR005)
  - Destination Region: REGION06 (via ADR006)

**Validation Points:**
- Verify material populated correctly
- Verify source and destination regions populated
- Check data flow: SHNUMBER → OIGSI → LIPS → MATNR
- Check data flow: ADRNR/ADRNZ → ADRC → REGION

---

### Test Case 11: Date Determination Logic - SO Movement

**Objective:** Verify date determination for Sales Order movements

**Test Data:**
- Trip with SO movement type
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: TRIP0000001 (Leg 1 and 2 are SO)
- Test Run: Unchecked

**Expected Results:**
- DEST_EXIT_DATE: Should equal DEST_DATE (for SO)
- SOURCE_ENT_DATE: Should come from YTTSTX0001 via YTTSTX0002 (for SO)

**Validation Points:**
- Verify DEST_EXIT_DATE = DEST_DATE for SO
- Verify SOURCE_ENT_DATE from YTTSTX0001

---

### Test Case 12: Date Determination Logic - PO Movement

**Objective:** Verify date determination for Purchase Order movements

**Test Data:**
- Trip with PO movement type
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: TRIP0000001 (Leg 3 is PO)
- Test Run: Unchecked

**Expected Results:**
- DEST_EXIT_DATE: Should come from YTTSTX0001 (MG_EXIT_DT where TRK_PURPOS = 'R')
- SOURCE_ENT_DATE: Should equal SOURCE_DATE (for PO)

**Validation Points:**
- Verify DEST_EXIT_DATE from YTTSTX0001 for PO
- Verify SOURCE_ENT_DATE = SOURCE_DATE for PO

---

### Test Case 13: Date Determination Logic - STO Movement

**Objective:** Verify date determination for Stock Transfer Order movements

**Test Data:**
- Trip with STO movement type
- From Date: 15.01.2024
- To Date: 20.01.2024
- Trip Number: TRIP0000002 (Leg 2 is STO)
- Test Run: Unchecked

**Expected Results:**
- DEST_EXIT_DATE: Should come from YTTSTX0001 (MG_EXIT_DT where TRK_PURPOS = 'R')
- SOURCE_ENT_DATE: Should come from YTTSTX0001 (PP_ENTR_DT via REPORT_NO from YTTSTX0002)

**Validation Points:**
- Verify DEST_EXIT_DATE from YTTSTX0001 for STO
- Verify SOURCE_ENT_DATE from YTTSTX0001 via YTTSTX0002 for STO

---

### Test Case 14: Background Execution

**Objective:** Verify program execution in background mode

**Test Data:**
- Schedule program in SM36
- From Date: 15.01.2024
- To Date: 20.01.2024
- Test Run: Unchecked

**Expected Results:**
- Program should execute successfully in background
- Results should be available in spool
- Database updates should occur
- No screen interactions required

**Validation Points:**
- Check background job log
- Verify spool output
- Verify database updates

---

### Test Case 15: Authorization Check

**Objective:** Verify authorization check functionality

**Test Data:**
- User without authorization for transaction ZCTD_ENRICH_TRIPDETAILS
- Attempt to execute program

**Expected Results:**
- Error message: Authorization check failed
- Program should exit
- No processing should occur

**Validation Points:**
- Verify error message displayed
- Verify program exits
- Check no data processed

---

## 3. SQL Scripts for Test Data Creation

### 3.1 Insert Trip Headers

```sql
INSERT INTO zsce_ctd_hdr VALUES ('TRIP0000001', 'VENDOR01', 'TRUCK001', '03', '20240115');
INSERT INTO zsce_ctd_hdr VALUES ('TRIP0000002', 'VENDOR01', 'TRUCK002', '03', '20240116');
INSERT INTO zsce_ctd_hdr VALUES ('TRIP0000003', 'VENDOR02', 'TRUCK003', '03', '20240117');
INSERT INTO zsce_ctd_hdr VALUES ('TRIP0000004', 'VENDOR01', 'TRUCK001', '03', '20240118');
INSERT INTO zsce_ctd_hdr VALUES ('TRIP0000005', 'VENDOR03', 'TRUCK004', '03', '20240119');
INSERT INTO zsce_ctd_hdr VALUES ('TRIP0000006', 'VENDOR02', 'TRUCK005', '03', '20240120');
INSERT INTO zsce_ctd_hdr VALUES ('TRIP0000007', 'VENDOR01', 'TRUCK001', '02', '20240121');
INSERT INTO zsce_ctd_hdr VALUES ('TRIP0000008', 'VENDOR04', 'TRUCK006', '03', '20240201');
```

### 3.2 Insert Trip Legs

```sql
-- TRIP0000001
INSERT INTO zsce_ctd_itm VALUES ('TRIP0000001', 1, 'VENDOR01', 'TRUCK001', 'SHIP001', '20240115', '20240116', 'SO', 'AREA01', 'ADR001', 'ADR002');
INSERT INTO zsce_ctd_itm VALUES ('TRIP0000001', 2, 'VENDOR01', 'TRUCK001', 'SHIP002', '20240116', '20240117', 'SO', 'AREA02', 'ADR003', 'ADR004');
INSERT INTO zsce_ctd_itm VALUES ('TRIP0000001', 3, 'VENDOR01', 'TRUCK001', 'SHIP003', '20240117', '20240118', 'PO', 'AREA03', 'ADR005', 'ADR006');

-- TRIP0000002
INSERT INTO zsce_ctd_itm VALUES ('TRIP0000002', 1, 'VENDOR01', 'TRUCK002', 'SHIP004', '20240116', '20240117', 'SO', 'AREA01', 'ADR007', 'ADR008');
INSERT INTO zsce_ctd_itm VALUES ('TRIP0000002', 2, 'VENDOR01', 'TRUCK002', 'SHIP005', '20240118', '20240119', 'STO', 'AREA05', 'ADR009', 'ADR010');

-- TRIP0000003
INSERT INTO zsce_ctd_itm VALUES ('TRIP0000003', 1, 'VENDOR02', 'TRUCK003', 'SHIP006', '20240117', '20240118', 'SO', 'AREA02', 'ADR011', 'ADR012');
INSERT INTO zsce_ctd_itm VALUES ('TRIP0000003', 2, 'VENDOR02', 'TRUCK003', 'SHIP007', '20240119', '20240120', 'PO', 'AREA04', 'ADR013', 'ADR014');

-- TRIP0000004 (Invalid - missing source date)
INSERT INTO zsce_ctd_itm VALUES ('TRIP0000004', 1, 'VENDOR01', 'TRUCK001', 'SHIP008', '00000000', '20240118', 'SO', 'AREA01', 'ADR015', 'ADR016');

-- TRIP0000005 (Invalid - missing destination date)
INSERT INTO zsce_ctd_itm VALUES ('TRIP0000005', 1, 'VENDOR03', 'TRUCK004', 'SHIP009', '20240119', '00000000', 'SO', 'AREA02', 'ADR017', 'ADR018');

-- TRIP0000006
INSERT INTO zsce_ctd_itm VALUES ('TRIP0000006', 1, 'VENDOR02', 'TRUCK005', 'SHIP010', '20240120', '20240121', 'SO', 'AREA03', 'ADR019', 'ADR020');
INSERT INTO zsce_ctd_itm VALUES ('TRIP0000006', 2, 'VENDOR02', 'TRUCK005', '', '20240122', '20240123', '', 'AREA04', '', '');
```

### 3.3 Insert Shipment Data (OIGSS)

```sql
INSERT INTO oigss VALUES ('SHIP001', '1', 'ROUTE01', 150, 'ZONE01', 'ZONE02', 'ADR001', 'ADR002');
INSERT INTO oigss VALUES ('SHIP002', '1', 'ROUTE02', 200, 'ZONE02', 'ZONE03', 'ADR003', 'ADR004');
INSERT INTO oigss VALUES ('SHIP003', '1', 'ROUTE03', 180, 'ZONE03', 'ZONE04', 'ADR005', 'ADR006');
INSERT INTO oigss VALUES ('SHIP004', '1', 'ROUTE04', 120, 'ZONE01', 'ZONE02', 'ADR007', 'ADR008');
INSERT INTO oigss VALUES ('SHIP005', '1', 'ROUTE05', 250, 'ZONE05', 'ZONE06', 'ADR009', 'ADR010');
INSERT INTO oigss VALUES ('SHIP006', '1', 'ROUTE06', 175, 'ZONE02', 'ZONE03', 'ADR011', 'ADR012');
INSERT INTO oigss VALUES ('SHIP007', '1', 'ROUTE07', 190, 'ZONE04', 'ZONE05', 'ADR013', 'ADR014');
INSERT INTO oigss VALUES ('SHIP008', '1', 'ROUTE08', 160, 'ZONE01', 'ZONE02', 'ADR015', 'ADR016');
INSERT INTO oigss VALUES ('SHIP009', '1', 'ROUTE09', 140, 'ZONE02', 'ZONE03', 'ADR017', 'ADR018');
INSERT INTO oigss VALUES ('SHIP010', '1', 'ROUTE10', 165, 'ZONE03', 'ZONE04', 'ADR019', 'ADR020');
```

### 3.4 Insert Shipment Item Data (OIGSI)

```sql
INSERT INTO oigsi VALUES ('SHIP001', 'DELV001');
INSERT INTO oigsi VALUES ('SHIP002', 'DELV002');
INSERT INTO oigsi VALUES ('SHIP003', 'DELV003');
INSERT INTO oigsi VALUES ('SHIP004', 'DELV004');
INSERT INTO oigsi VALUES ('SHIP005', 'DELV005');
INSERT INTO oigsi VALUES ('SHIP006', 'DELV006');
INSERT INTO oigsi VALUES ('SHIP007', 'DELV007');
INSERT INTO oigsi VALUES ('SHIP008', 'DELV008');
INSERT INTO oigsi VALUES ('SHIP009', 'DELV009');
INSERT INTO oigsi VALUES ('SHIP010', 'DELV010');
```

### 3.5 Insert Delivery Item Data (LIPS)

```sql
INSERT INTO lips VALUES ('DELV001', 'MAT001');
INSERT INTO lips VALUES ('DELV002', 'MAT002');
INSERT INTO lips VALUES ('DELV003', 'MAT003');
INSERT INTO lips VALUES ('DELV004', 'MAT004');
INSERT INTO lips VALUES ('DELV005', 'MAT005');
INSERT INTO lips VALUES ('DELV006', 'MAT006');
INSERT INTO lips VALUES ('DELV007', 'MAT007');
INSERT INTO lips VALUES ('DELV008', 'MAT008');
INSERT INTO lips VALUES ('DELV009', 'MAT009');
INSERT INTO lips VALUES ('DELV010', 'MAT010');
```

### 3.6 Insert Address Data (ADRC)

```sql
INSERT INTO adrc VALUES ('ADR001', 'REGION01');
INSERT INTO adrc VALUES ('ADR002', 'REGION02');
INSERT INTO adrc VALUES ('ADR003', 'REGION03');
INSERT INTO adrc VALUES ('ADR004', 'REGION04');
INSERT INTO adrc VALUES ('ADR005', 'REGION05');
INSERT INTO adrc VALUES ('ADR006', 'REGION06');
INSERT INTO adrc VALUES ('ADR007', 'REGION01');
INSERT INTO adrc VALUES ('ADR008', 'REGION02');
INSERT INTO adrc VALUES ('ADR009', 'REGION05');
INSERT INTO adrc VALUES ('ADR010', 'REGION06');
INSERT INTO adrc VALUES ('ADR011', 'REGION03');
INSERT INTO adrc VALUES ('ADR012', 'REGION04');
INSERT INTO adrc VALUES ('ADR013', 'REGION04');
INSERT INTO adrc VALUES ('ADR014', 'REGION05');
INSERT INTO adrc VALUES ('ADR015', 'REGION01');
INSERT INTO adrc VALUES ('ADR016', 'REGION02');
INSERT INTO adrc VALUES ('ADR017', 'REGION03');
INSERT INTO adrc VALUES ('ADR018', 'REGION04');
INSERT INTO adrc VALUES ('ADR019', 'REGION05');
INSERT INTO adrc VALUES ('ADR020', 'REGION06');
```

### 3.7 Insert Trip Status Data (YTTSTX0001)

```sql
INSERT INTO yttstx0001 VALUES ('SHIP003', 'R', '20240118', '20240115');
INSERT INTO yttstx0001 VALUES ('SHIP005', 'R', '20240119', '20240116');
INSERT INTO yttstx0001 VALUES ('SHIP007', 'R', '20240120', '20240117');
```

### 3.8 Insert Trip Report Data (YTTSTX0002)

```sql
INSERT INTO yttstx0002 VALUES ('SHIP001', 'REP001');
INSERT INTO yttstx0002 VALUES ('SHIP002', 'REP002');
INSERT INTO yttstx0002 VALUES ('SHIP004', 'REP004');
INSERT INTO yttstx0002 VALUES ('SHIP005', 'REP005');
INSERT INTO yttstx0002 VALUES ('SHIP006', 'REP006');
INSERT INTO yttstx0002 VALUES ('SHIP010', 'REP010');
```

### 3.9 Insert Route Data (TROLZ)

```sql
INSERT INTO trolz VALUES ('03', '0006', 'ROUTE_EMPTY_01');
INSERT INTO trolz VALUES ('03', '0006', 'ROUTE_EMPTY_02');
INSERT INTO trolz VALUES ('03', '0006', 'ROUTE_EMPTY_03');
```

### 3.10 Insert Route Distance Data (TVRO)

```sql
INSERT INTO tvro VALUES ('ROUTE_EMPTY_01', 50);
INSERT INTO tvro VALUES ('ROUTE_EMPTY_02', 75);
INSERT INTO tvro VALUES ('ROUTE_EMPTY_03', 60);
INSERT INTO tvro VALUES ('ROUTE01', 150);
INSERT INTO tvro VALUES ('ROUTE02', 200);
INSERT INTO tvro VALUES ('ROUTE03', 180);
INSERT INTO tvro VALUES ('ROUTE04', 120);
INSERT INTO tvro VALUES ('ROUTE05', 250);
INSERT INTO tvro VALUES ('ROUTE06', 175);
INSERT INTO tvro VALUES ('ROUTE07', 190);
INSERT INTO tvro VALUES ('ROUTE08', 160);
INSERT INTO tvro VALUES ('ROUTE09', 140);
INSERT INTO tvro VALUES ('ROUTE10', 165);
```

---

## 4. Test Execution Checklist

### Pre-Test Setup
- [ ] Create test data in all required tables
- [ ] Verify function module Z_SCM_GET_BUSINESS is available
- [ ] Verify message class ZCTD_MSG exists with all messages
- [ ] Verify text elements are created
- [ ] Verify authorization is set up

### Test Execution
- [ ] Execute Test Case 1: Basic Enrichment
- [ ] Execute Test Case 2: Empty Leg Insertion
- [ ] Execute Test Case 3: Blank Destination Area
- [ ] Execute Test Case 4: Missing Source Date
- [ ] Execute Test Case 5: Missing Destination Date
- [ ] Execute Test Case 6: Test Run Mode
- [ ] Execute Test Case 7: Date Range Validation
- [ ] Execute Test Case 8: Multiple Trips
- [ ] Execute Test Case 9: Empty Leg Validation
- [ ] Execute Test Case 10: Material/Region Determination
- [ ] Execute Test Case 11: SO Date Logic
- [ ] Execute Test Case 12: PO Date Logic
- [ ] Execute Test Case 13: STO Date Logic
- [ ] Execute Test Case 14: Background Execution
- [ ] Execute Test Case 15: Authorization Check

### Post-Test Verification
- [ ] Verify database updates are correct
- [ ] Verify trip status updates
- [ ] Verify log entries
- [ ] Verify ALV output
- [ ] Clean up test data (if needed)

---

## 5. Expected Output Examples

### 5.1 ALV Output for TRIP0000001 (After Enrichment)

| LIFNR | TRUCK_NO | TRIP_NO | COUNTER | LEG_TYPE | SHNUMBER | SOURCE_DATE | DEST_DATE | SOURCE_ZONE | DEST_ZONE | ROUTE | DISTANCE | MATNR | SOURCE_REGION | DEST_REGION | BUSINESS_ID | SUBBUSINESS_ID | TRIP_STATUS |
|-------|----------|---------|---------|----------|----------|-------------|-----------|-------------|-----------|-------|----------|-------|---------------|-------------|-------------|----------------|-------------|
| VENDOR01 | TRUCK001 | TRIP0000001 | 1 | L | SHIP001 | 20240115 | 20240116 | ZONE01 | ZONE02 | ROUTE01 | 150 | MAT001 | REGION01 | REGION02 | BUS001 | SUB001 | 04 |
| VENDOR01 | TRUCK001 | TRIP0000001 | 2 | L | SHIP002 | 20240116 | 20240117 | ZONE02 | ZONE03 | ROUTE02 | 200 | MAT002 | REGION03 | REGION04 | BUS002 | SUB002 | 04 |
| VENDOR01 | TRUCK001 | TRIP0000001 | 3 | L | SHIP003 | 20240117 | 20240118 | ZONE03 | ZONE04 | ROUTE03 | 180 | MAT003 | REGION05 | REGION06 | BUS001 | SUB003 | 04 |

### 5.2 ALV Output for TRIP0000002 (After Empty Leg Insertion)

| LIFNR | TRUCK_NO | TRIP_NO | COUNTER | LEG_TYPE | SHNUMBER | SOURCE_DATE | DEST_DATE | SOURCE_ZONE | DEST_ZONE | ROUTE | DISTANCE | MATNR | TRIP_STATUS |
|-------|----------|---------|---------|----------|----------|-------------|-----------|-------------|-----------|-------|----------|-------|-------------|
| VENDOR01 | TRUCK002 | TRIP0000002 | 1 | L | SHIP004 | 20240116 | 20240117 | ZONE01 | ZONE02 | ROUTE04 | 120 | MAT004 | 04 |
| VENDOR01 | TRUCK002 | TRIP0000002 | 2 | E | | 20240117 | 20240118 | ZONE02 | ZONE05 | ROUTE_EMPTY_01 | 50 | | 04 |
| VENDOR01 | TRUCK002 | TRIP0000002 | 3 | L | SHIP005 | 20240118 | 20240119 | ZONE05 | ZONE06 | ROUTE05 | 250 | MAT005 | 04 |

---

## 6. Test Data Cleanup Scripts

```sql
-- Clean up test data (use with caution in production)
DELETE FROM zsce_ctd_itm WHERE trip_no LIKE 'TRIP%';
DELETE FROM zsce_ctd_hdr WHERE trip_no LIKE 'TRIP%';
DELETE FROM oigss WHERE shnumber LIKE 'SHIP%';
DELETE FROM oigsi WHERE shnumber LIKE 'SHIP%';
DELETE FROM lips WHERE vbeln LIKE 'DELV%';
DELETE FROM adrc WHERE addrnumber LIKE 'ADR%';
DELETE FROM yttstx0001 WHERE shnumber LIKE 'SHIP%';
DELETE FROM yttstx0002 WHERE shnumber LIKE 'SHIP%';
DELETE FROM trolz WHERE route LIKE 'ROUTE_EMPTY%';
DELETE FROM tvro WHERE route LIKE 'ROUTE%';
```

---

**Document Approval**

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Test Lead | | | |
| Quality Assurance | | | |
| Project Manager | | | |

---

**Revision History**

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | [Date] | [Author] | Initial version |

