# Test Data and Test Scenarios
## Function Module: ZSCM_CTD_GETTRIPITM

---

**Document Information**

| Field | Value |
|-------|-------|
| **Document Type** | Test Data and Test Scenarios |
| **Function Module Name** | ZSCM_CTD_GETTRIPITM |
| **Version** | 1.0 |
| **Date** | [Current Date] |
| **Author** | [Author Name] |
| **Related TS** | CTD_BRD03_TS_Fetch_CTD_Trip_Item_Details |

---

## 1. Test Data Preparation

### 1.1 Prerequisites

Before executing tests, ensure:
- Function Module ZSCM_CTD_GETTRIPITM is activated
- Message class ZCTD exists with messages 001-006
- Test users are created with appropriate authorizations
- Test data tables are accessible
- Function Module ZSCM_CTD_GETTRIPHDR is available (for integration testing)

### 1.2 Test Data Setup

#### 1.2.1 Vendor Master Data (LFA1)

Create test vendors in LFA1 table:

| Vendor Number (LIFNR) | Vendor Name | Status |
|------------------------|-------------|--------|
| 0000012345 | Test Vendor 1 | Active |
| 0000012346 | Test Vendor 2 | Active |
| 0000012347 | Test Vendor 3 | Active |

**Note:** Use SE16 or SE11 to create test vendor records if required.

#### 1.2.2 Test Data in ZSCE_CTD_HDR Table

Insert the following test records into ZSCE_CTD_HDR (for trip validation):

**Test Data Set 1: Vendor 0000012345 - Trips**

| TRIP_NO | LIFNR | TRUCK_NO | TRIP_STATUS | CREATED_DATE | CREATED_TIME | CREATED_BY | BUSINESS | SUBBUSINESS |
|---------|-------|----------|-------------|--------------|--------------|------------|----------|-------------|
| TRIP001 | 0000012345 | TRUCK001 | 04 | 20240115 | 100000 | TESTUSER | BUS001 | SUB001 |
| TRIP002 | 0000012345 | TRUCK002 | 04 | 20240120 | 110000 | TESTUSER | BUS001 | SUB002 |
| TRIP003 | 0000012345 | TRUCK001 | 06 | 20240125 | 120000 | TESTUSER | BUS001 | SUB001 |
| TRIP004 | 0000012346 | TRUCK003 | 04 | 20240116 | 100000 | TESTUSER | BUS001 | SUB001 |
| TRIP005 | 0000012347 | TRUCK004 | 04 | 20240118 | 110000 | TESTUSER | BUS003 | SUB004 |
| TRIP006 | 0000012345 | TRUCK001 | 04 | 20240130 | 130000 | TESTUSER | BUS002 | SUB003 |

**Test Data Set 2: Trip with No Items (for testing empty result)**

| TRIP_NO | LIFNR | TRUCK_NO | TRIP_STATUS | CREATED_DATE | CREATED_TIME | CREATED_BY | BUSINESS | SUBBUSINESS |
|---------|-------|----------|-------------|--------------|--------------|------------|----------|-------------|
| TRIP007 | 0000012345 | TRUCK002 | 04 | 20240122 | 140000 | TESTUSER | BUS001 | SUB001 |

#### 1.2.3 Test Data in ZSCE_CTD_ITM Table

Insert the following test records into ZSCE_CTD_ITM (trip item/leg data):

**Test Data Set 1: TRIP001 - Multiple Legs (Vendor 0000012345)**

| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ | SOURCE_ZONE | DEST_ZONE | ROUTE | DISTANCE | MATNR | SOURCE_REGION | DEST_REGION | BUSINESS_ID | SUBBUSINESS_ID | DEST_EXIT_DATE | SOURCE_ENT_DATE | CTD_RULEENG_REMARKS |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|-------------|-----------|-------|----------|-------|---------------|-------------|-------------|----------------|-----------------|------------------|---------------------|
| TRIP001 | 1 | 0000012345 | TRUCK001 | SH001 | 20240115 | 20240116 | SO | AREA1 | ADR001 | ADR002 | ZONE01 | ZONE02 | RT001 | 100 | MAT001 | REG001 | REG002 | BUS001 | SUB001 | 20240116 | 20240115 | Remark1 |
| TRIP001 | 2 | 0000012345 | TRUCK001 | SH002 | 20240116 | 20240117 | PO | AREA2 | ADR002 | ADR003 | ZONE02 | ZONE03 | RT002 | 150 | MAT002 | REG002 | REG003 | BUS001 | SUB001 | 20240117 | 20240116 | Remark2 |
| TRIP001 | 3 | 0000012345 | TRUCK001 | SH003 | 20240117 | 20240118 | STO | AREA3 | ADR003 | ADR004 | ZONE03 | ZONE04 | RT003 | 200 | MAT003 | REG003 | REG004 | BUS001 | SUB001 | 20240118 | 20240117 | Remark3 |

**Test Data Set 2: TRIP002 - Single Leg (Vendor 0000012345)**

| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ | SOURCE_ZONE | DEST_ZONE | ROUTE | DISTANCE | MATNR | SOURCE_REGION | DEST_REGION | BUSINESS_ID | SUBBUSINESS_ID | DEST_EXIT_DATE | SOURCE_ENT_DATE | CTD_RULEENG_REMARKS |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|-------------|-----------|-------|----------|-------|---------------|-------------|-------------|----------------|-----------------|------------------|---------------------|
| TRIP002 | 1 | 0000012345 | TRUCK002 | SH004 | 20240120 | 20240121 | SO | AREA1 | ADR005 | ADR006 | ZONE05 | ZONE06 | RT004 | 120 | MAT004 | REG005 | REG006 | BUS001 | SUB002 | 20240121 | 20240120 | Remark4 |

**Test Data Set 3: TRIP003 - Multiple Legs with Different Sequence (Vendor 0000012345)**

| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ | SOURCE_ZONE | DEST_ZONE | ROUTE | DISTANCE | MATNR | SOURCE_REGION | DEST_REGION | BUSINESS_ID | SUBBUSINESS_ID | DEST_EXIT_DATE | SOURCE_ENT_DATE | CTD_RULEENG_REMARKS |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|-------------|-----------|-------|----------|-------|---------------|-------------|-------------|----------------|-----------------|------------------|---------------------|
| TRIP003 | 1 | 0000012345 | TRUCK001 | SH005 | 20240125 | 20240126 | SO | AREA4 | ADR007 | ADR008 | ZONE07 | ZONE08 | RT005 | 180 | MAT005 | REG007 | REG008 | BUS001 | SUB001 | 20240126 | 20240125 | Remark5 |
| TRIP003 | 2 | 0000012345 | TRUCK001 | SH006 | 20240126 | 20240127 | PO | AREA5 | ADR008 | ADR009 | ZONE08 | ZONE09 | RT006 | 220 | MAT006 | REG008 | REG009 | BUS001 | SUB001 | 20240127 | 20240126 | Remark6 |

**Test Data Set 4: TRIP004 - Different Vendor (Vendor 0000012346) - For Unauthorized Access Test**

| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ | SOURCE_ZONE | DEST_ZONE | ROUTE | DISTANCE | MATNR | SOURCE_REGION | DEST_REGION | BUSINESS_ID | SUBBUSINESS_ID | DEST_EXIT_DATE | SOURCE_ENT_DATE | CTD_RULEENG_REMARKS |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|-------------|-----------|-------|----------|-------|---------------|-------------|-------------|----------------|-----------------|------------------|---------------------|
| TRIP004 | 1 | 0000012346 | TRUCK003 | SH007 | 20240116 | 20240117 | SO | AREA6 | ADR010 | ADR011 | ZONE10 | ZONE11 | RT007 | 90 | MAT007 | REG010 | REG011 | BUS001 | SUB001 | 20240117 | 20240116 | Remark7 |
| TRIP004 | 2 | 0000012346 | TRUCK003 | SH008 | 20240117 | 20240118 | PO | AREA7 | ADR011 | ADR012 | ZONE11 | ZONE12 | RT008 | 110 | MAT008 | REG011 | REG012 | BUS001 | SUB001 | 20240118 | 20240117 | Remark8 |

**Test Data Set 5: TRIP005 - RIL Operations Authorization Test (Vendor 0000012347)**

| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ | SOURCE_ZONE | DEST_ZONE | ROUTE | DISTANCE | MATNR | SOURCE_REGION | DEST_REGION | BUSINESS_ID | SUBBUSINESS_ID | DEST_EXIT_DATE | SOURCE_ENT_DATE | CTD_RULEENG_REMARKS |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|-------------|-----------|-------|----------|-------|---------------|-------------|-------------|----------------|-----------------|------------------|---------------------|
| TRIP005 | 1 | 0000012347 | TRUCK004 | SH009 | 20240118 | 20240119 | SO | AREA8 | ADR013 | ADR014 | ZONE13 | ZONE14 | RT009 | 130 | MAT009 | REG013 | REG014 | BUS003 | SUB004 | 20240119 | 20240118 | Remark9 |
| TRIP005 | 2 | 0000012347 | TRUCK004 | SH010 | 20240119 | 20240120 | PO | AREA9 | ADR014 | ADR015 | ZONE14 | ZONE15 | RT010 | 160 | MAT010 | REG014 | REG015 | BUS003 | SUB004 | 20240120 | 20240119 | Remark10 |

**Test Data Set 6: TRIP006 - Large Trip (Many Legs) - For Performance Test**

| TRIP_NO | COUNTER | LIFNR | TRUCK_NO | SHNUMBER | SOURCE_DATE | DEST_DATE | MVT_TYPE | AREA | ADRNR | ADRNZ | SOURCE_ZONE | DEST_ZONE | ROUTE | DISTANCE | MATNR | SOURCE_REGION | DEST_REGION | BUSINESS_ID | SUBBUSINESS_ID | DEST_EXIT_DATE | SOURCE_ENT_DATE | CTD_RULEENG_REMARKS |
|---------|---------|-------|----------|----------|-------------|-----------|----------|------|-------|-------|-------------|-----------|-------|----------|-------|---------------|-------------|-------------|----------------|-----------------|------------------|---------------------|
| TRIP006 | 1 | 0000012345 | TRUCK001 | SH011 | 20240130 | 20240131 | SO | AREA1 | ADR016 | ADR017 | ZONE16 | ZONE17 | RT011 | 100 | MAT011 | REG016 | REG017 | BUS002 | SUB003 | 20240131 | 20240130 | Remark11 |
| TRIP006 | 2 | 0000012345 | TRUCK001 | SH012 | 20240131 | 20240201 | PO | AREA2 | ADR017 | ADR018 | ZONE17 | ZONE18 | RT012 | 120 | MAT012 | REG017 | REG018 | BUS002 | SUB003 | 20240201 | 20240131 | Remark12 |
| ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... |

**Note:** For performance testing, create TRIP006 with 50+ legs (COUNTER 1 to 50+).

**Test Data Set 7: TRIP007 - No Items (Trip exists but no legs)**

No records in ZSCE_CTD_ITM for TRIP007 (trip exists in ZSCE_CTD_HDR but has no item/leg records).

#### 1.2.4 SQL Scripts for Test Data Insertion

**Option 1: Using SE16 (Table Maintenance)**

1. Go to SE16 → Enter table name: ZSCE_CTD_HDR
2. Click "Create Entries"
3. Enter test data manually
4. Repeat for ZSCE_CTD_ITM

**Option 2: Using ABAP Program (Recommended for Bulk Data)**

```abap
*&---------------------------------------------------------------------*
*& Report  ZSCM_CTD_TEST_DATA_SETUP_ITM
*&---------------------------------------------------------------------*
*& Purpose: Create test data for ZSCM_CTD_GETTRIPITM testing
*&---------------------------------------------------------------------*
REPORT zscm_ctd_test_data_setup_itm.

DATA: lw_hdr TYPE zsce_ctd_hdr,
      lw_itm TYPE zsce_ctd_itm.

" Clear existing test data (optional)
DELETE FROM zsce_ctd_hdr WHERE trip_no LIKE 'TRIP%'.
DELETE FROM zsce_ctd_itm WHERE trip_no LIKE 'TRIP%'.

" Create Header Data: TRIP001
lw_hdr-trip_no = 'TRIP001'.
lw_hdr-lifnr = '0000012345'.
lw_hdr-truck_no = 'TRUCK001'.
lw_hdr-trip_status = '04'.
lw_hdr-created_date = '20240115'.
lw_hdr-created_time = '100000'.
lw_hdr-created_by = sy-uname.
lw_hdr-business = 'BUS001'.
lw_hdr-subbusiness = 'SUB001'.
INSERT zsce_ctd_hdr FROM lw_hdr.

" Create Item Data: TRIP001 - Leg 1
lw_itm-trip_no = 'TRIP001'.
lw_itm-counter = 1.
lw_itm-lifnr = '0000012345'.
lw_itm-truck_no = 'TRUCK001'.
lw_itm-shnumber = 'SH001'.
lw_itm-source_date = '20240115'.
lw_itm-dest_date = '20240116'.
lw_itm-mvt_type = 'SO'.
lw_itm-area = 'AREA1'.
lw_itm-adrnr = 'ADR001'.
lw_itm-adrnz = 'ADR002'.
lw_itm-source_zone = 'ZONE01'.
lw_itm-dest_zone = 'ZONE02'.
lw_itm-route = 'RT001'.
lw_itm-distance = 100.
lw_itm-matnr = 'MAT001'.
lw_itm-source_region = 'REG001'.
lw_itm-dest_region = 'REG002'.
lw_itm-business_id = 'BUS001'.
lw_itm-subbusiness_id = 'SUB001'.
lw_itm-dest_exit_date = '20240116'.
lw_itm-source_ent_date = '20240115'.
lw_itm-ctd_ruleeng_remarks = 'Remark1'.
INSERT zsce_ctd_itm FROM lw_itm.

" Create Item Data: TRIP001 - Leg 2
lw_itm-trip_no = 'TRIP001'.
lw_itm-counter = 2.
lw_itm-shnumber = 'SH002'.
lw_itm-source_date = '20240116'.
lw_itm-dest_date = '20240117'.
lw_itm-mvt_type = 'PO'.
lw_itm-area = 'AREA2'.
lw_itm-adrnr = 'ADR002'.
lw_itm-adrnz = 'ADR003'.
lw_itm-source_zone = 'ZONE02'.
lw_itm-dest_zone = 'ZONE03'.
lw_itm-route = 'RT002'.
lw_itm-distance = 150.
lw_itm-matnr = 'MAT002'.
lw_itm-source_region = 'REG002'.
lw_itm-dest_region = 'REG003'.
lw_itm-dest_exit_date = '20240117'.
lw_itm-source_ent_date = '20240116'.
lw_itm-ctd_ruleeng_remarks = 'Remark2'.
INSERT zsce_ctd_itm FROM lw_itm.

" Create Item Data: TRIP001 - Leg 3
lw_itm-trip_no = 'TRIP001'.
lw_itm-counter = 3.
lw_itm-shnumber = 'SH003'.
lw_itm-source_date = '20240117'.
lw_itm-dest_date = '20240118'.
lw_itm-mvt_type = 'STO'.
lw_itm-area = 'AREA3'.
lw_itm-adrnr = 'ADR003'.
lw_itm-adrnz = 'ADR004'.
lw_itm-source_zone = 'ZONE03'.
lw_itm-dest_zone = 'ZONE04'.
lw_itm-route = 'RT003'.
lw_itm-distance = 200.
lw_itm-matnr = 'MAT003'.
lw_itm-source_region = 'REG003'.
lw_itm-dest_region = 'REG004'.
lw_itm-dest_exit_date = '20240118'.
lw_itm-source_ent_date = '20240117'.
lw_itm-ctd_ruleeng_remarks = 'Remark3'.
INSERT zsce_ctd_itm FROM lw_itm.

" Repeat for all other trips and legs...
" (Add all test data records)

COMMIT WORK.

WRITE: / 'Test data created successfully'.
```

#### 1.2.5 User Setup for Testing

**Test Users:**

| User ID | User Type | Vendor Number | Business Authorization | Sub-Business Authorization |
|---------|-----------|---------------|----------------------|---------------------------|
| TESTUSER1 | Vendor | 0000012345 | N/A | N/A |
| TESTUSER2 | Vendor | 0000012346 | N/A | N/A |
| RILUSER1 | RIL Operations | N/A | BUS001, BUS002 | SUB001, SUB002, SUB003 |
| RILUSER2 | RIL Operations | N/A | BUS003, BUS004 | SUB004, SUB005, SUB006 |
| UNAUTHUSER | RIL Operations | N/A | None | None |

**Authorization Setup:**
- Ensure RFC authorization (S_RFC) is configured for test users
- Configure Business/Sub-Business authorization for RIL Operations users
- Use custom authorization object Z_CTD_BUSINESS or custom table

---

## 2. Test Scenarios

### 2.1 Happy Path Test Scenarios

#### Test Case TC-001: Vendor User - Valid Trip with Multiple Legs

**Objective:** Verify vendor user can retrieve all item/leg records for their trip, sorted by COUNTER

**Preconditions:**
- Test data is loaded
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP001'
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '0000012345'
4. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_ITM contains 3 records (COUNTER 1, 2, 3)
- All records have TRIP_NO = 'TRIP001'
- All records have LIFNR = '0000012345'
- Records are sorted by COUNTER in ascending order (1, 2, 3)
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] ET_TRIP_ITM contains 3 records
- [ ] All TRIP_NO = 'TRIP001'
- [ ] All LIFNR = '0000012345'
- [ ] COUNTER values are 1, 2, 3 in order
- [ ] Records sorted by COUNTER (ascending)
- [ ] No exception raised

---

#### Test Case TC-002: Vendor User - Valid Trip with Single Leg

**Objective:** Verify single leg trip is handled correctly

**Preconditions:**
- Test data is loaded
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP002'
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '0000012345'
4. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_ITM contains 1 record (COUNTER = 1)
- Record has TRIP_NO = 'TRIP002'
- Record has LIFNR = '0000012345'
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] ET_TRIP_ITM contains 1 record
- [ ] COUNTER = 1
- [ ] TRIP_NO = 'TRIP002'
- [ ] LIFNR = '0000012345'
- [ ] No exception raised

---

#### Test Case TC-003: RIL Operations User - Valid Trip with Authorization

**Objective:** Verify RIL Operations user can retrieve items for authorized trip

**Preconditions:**
- Test data is loaded
- User RILUSER2 is logged in
- User has authorization for BUS003, SUB004

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP005'
   - IV_USER_TYPE: 'R'
   - IV_USER_ID: 'RILUSER2'
4. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_ITM contains 2 records (COUNTER 1, 2)
- All records have TRIP_NO = 'TRIP005'
- Records are sorted by COUNTER in ascending order
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] ET_TRIP_ITM contains 2 records
- [ ] All TRIP_NO = 'TRIP005'
- [ ] Records sorted by COUNTER
- [ ] No exception raised

**Note:** Actual result depends on authorization implementation. If authorization is not implemented, this test may need adjustment.

---

#### Test Case TC-004: Trip with No Items - Valid Scenario

**Objective:** Verify empty result set is handled correctly (not an error)

**Preconditions:**
- Test data is loaded
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP007'
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '0000012345'
4. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_ITM is empty (no records)
- No exception is raised
- This is a valid scenario, not an error

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] ET_TRIP_ITM is empty
- [ ] No exception raised
- [ ] No error message displayed

---

#### Test Case TC-005: Verify Sorting by COUNTER

**Objective:** Verify records are sorted by COUNTER in ascending order

**Preconditions:**
- Test data is loaded
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP001'
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '0000012345'
4. Click "Execute" (F8)
5. Verify COUNTER values are in ascending order

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_ITM contains records with COUNTER values: 1, 2, 3
- COUNTER values are in ascending order
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] COUNTER values are 1, 2, 3
- [ ] Records are in ascending order by COUNTER
- [ ] No exception raised

---

### 2.2 Error Path Test Scenarios

#### Test Case TC-006: Trip Not Found

**Objective:** Verify exception is raised when trip does not exist

**Preconditions:**
- Function Module is activated
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'INVALID' (non-existent trip)
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '0000012345'
4. Click "Execute" (F8)

**Expected Results:**
- Exception TRIP_NOT_FOUND is raised
- SY-SUBRC = 1 (exception code)
- Error message displayed: "Trip number not found"
- ET_TRIP_ITM is empty

**Validation Checklist:**
- [ ] Exception TRIP_NOT_FOUND raised
- [ ] SY-SUBRC = 1
- [ ] Error message is correct
- [ ] ET_TRIP_ITM is empty

---

#### Test Case TC-007: Blank Trip Number

**Objective:** Verify exception is raised for blank trip number

**Preconditions:**
- Function Module is activated
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: '' (blank)
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '0000012345'
4. Click "Execute" (F8)

**Expected Results:**
- Exception TRIP_NOT_FOUND is raised
- SY-SUBRC = 1
- Error message displayed: "Trip number is mandatory" or "Trip number not found"
- ET_TRIP_ITM is empty

**Validation Checklist:**
- [ ] Exception TRIP_NOT_FOUND raised
- [ ] SY-SUBRC = 1
- [ ] Error message is correct
- [ ] ET_TRIP_ITM is empty

---

#### Test Case TC-008: Invalid User Type

**Objective:** Verify exception is raised for invalid user type

**Preconditions:**
- Function Module is activated
- User is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP001'
   - IV_USER_TYPE: 'X' (invalid)
   - IV_USER_ID: '0000012345'
4. Click "Execute" (F8)

**Expected Results:**
- Exception INVALID_USER_TYPE is raised
- SY-SUBRC = 2
- Error message displayed: "Invalid user type. Must be V or R"
- ET_TRIP_ITM is empty

**Validation Checklist:**
- [ ] Exception INVALID_USER_TYPE raised
- [ ] SY-SUBRC = 2
- [ ] Error message is correct
- [ ] ET_TRIP_ITM is empty

---

#### Test Case TC-009: Blank User Type

**Objective:** Verify exception is raised for blank user type

**Preconditions:**
- Function Module is activated
- User is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP001'
   - IV_USER_TYPE: '' (blank)
   - IV_USER_ID: '0000012345'
4. Click "Execute" (F8)

**Expected Results:**
- Exception INVALID_USER_TYPE is raised
- SY-SUBRC = 2
- Error message displayed: "User type is mandatory"
- ET_TRIP_ITM is empty

**Validation Checklist:**
- [ ] Exception INVALID_USER_TYPE raised
- [ ] SY-SUBRC = 2
- [ ] Error message is correct
- [ ] ET_TRIP_ITM is empty

---

#### Test Case TC-010: Blank User ID

**Objective:** Verify exception is raised for blank user ID

**Preconditions:**
- Function Module is activated
- User is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP001'
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '' (blank)
4. Click "Execute" (F8)

**Expected Results:**
- Exception INVALID_USER_ID is raised
- SY-SUBRC = 3
- Error message displayed: "User ID is mandatory"
- ET_TRIP_ITM is empty

**Validation Checklist:**
- [ ] Exception INVALID_USER_ID raised
- [ ] SY-SUBRC = 3
- [ ] Error message is correct
- [ ] ET_TRIP_ITM is empty

---

#### Test Case TC-011: Unauthorized Access - Vendor Accessing Other Vendor's Trip

**Objective:** Verify exception is raised when vendor tries to access another vendor's trip

**Preconditions:**
- Test data is loaded
- User TESTUSER1 (Vendor 0000012345) is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP004' (belongs to Vendor 0000012346)
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '0000012345' (different vendor)
4. Click "Execute" (F8)

**Expected Results:**
- Exception UNAUTHORIZED_ACCESS is raised
- SY-SUBRC = 4
- Error message displayed: "User does not have access to this trip"
- ET_TRIP_ITM is empty

**Validation Checklist:**
- [ ] Exception UNAUTHORIZED_ACCESS raised
- [ ] SY-SUBRC = 4
- [ ] Error message is correct
- [ ] ET_TRIP_ITM is empty

---

#### Test Case TC-012: No RFC Authorization

**Objective:** Verify exception is raised when user lacks RFC authorization

**Preconditions:**
- Function Module is activated
- User UNAUTHUSER is logged in (no RFC authorization)

**Test Steps:**
1. Log in as UNAUTHUSER (user without RFC authorization)
2. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
3. Click "Test" button
4. Enter test data:
   - IV_TRIP_NO: 'TRIP001'
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '0000012345'
5. Click "Execute" (F8)

**Expected Results:**
- Exception NO_AUTHORITY is raised
- SY-SUBRC = 5
- Error message displayed: "No RFC authorization"
- ET_TRIP_ITM is empty

**Validation Checklist:**
- [ ] Exception NO_AUTHORITY raised
- [ ] SY-SUBRC = 5
- [ ] Error message is correct
- [ ] ET_TRIP_ITM is empty

---

#### Test Case TC-013: RIL Operations - No Authorization for Trip

**Objective:** Verify exception is raised when RIL Operations user lacks authorization for trip

**Preconditions:**
- Test data is loaded
- User UNAUTHUSER (RIL Operations with no Business authorization) is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP005' (BUS003, SUB004)
   - IV_USER_TYPE: 'R'
   - IV_USER_ID: 'UNAUTHUSER' (no authorization)
4. Click "Execute" (F8)

**Expected Results:**
- Exception NO_AUTHORITY is raised (or empty result if authorization not implemented)
- SY-SUBRC = 5 (or 0 if empty result)
- Error message displayed: "No authorization for this trip" (if exception)
- ET_TRIP_ITM is empty

**Validation Checklist:**
- [ ] Exception NO_AUTHORITY raised OR empty result
- [ ] SY-SUBRC = 5 OR 0
- [ ] Error message is correct (if exception)
- [ ] ET_TRIP_ITM is empty

**Note:** Behavior depends on authorization implementation.

---

### 2.3 Edge Case Test Scenarios

#### Test Case TC-014: Trip with Duplicate Counters (Data Quality Issue)

**Objective:** Verify handling of duplicate COUNTER values (if data quality issue exists)

**Preconditions:**
- Test data is loaded (may need to create test data with duplicate counters)
- User TESTUSER1 is logged in

**Test Steps:**
1. Create test data with duplicate COUNTER values (if possible)
2. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
3. Click "Test" button
4. Enter test data:
   - IV_TRIP_NO: 'TRIPXXX' (trip with duplicate counters)
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '0000012345'
5. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_ITM contains all records (including duplicates)
- Records are sorted by COUNTER
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] All records returned (including duplicates)
- [ ] Records sorted by COUNTER
- [ ] No exception raised

---

#### Test Case TC-015: Large Trip (Many Legs) - Performance Test

**Objective:** Verify performance with trip having many legs

**Preconditions:**
- Test data is loaded (TRIP006 with 50+ legs)
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP006'
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '0000012345'
4. Click "Execute" (F8)
5. Note the execution time

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_ITM contains all legs (50+ records)
- Records are sorted by COUNTER
- Response time < 5 seconds (performance requirement)
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] All legs returned
- [ ] Records sorted by COUNTER
- [ ] Response time < 5 seconds
- [ ] No exception raised

---

#### Test Case TC-016: Integration with Header FM

**Objective:** Verify integration with ZSCM_CTD_GETTRIPHDR

**Preconditions:**
- Test data is loaded
- Both Function Modules are activated
- User TESTUSER1 is logged in

**Test Steps:**
1. First, call ZSCM_CTD_GETTRIPHDR to get trip header
2. Select a trip from the result (e.g., TRIP001)
3. Call ZSCM_CTD_GETTRIPITM with the selected trip number
4. Verify data consistency

**Expected Results:**
- Both Function Modules execute successfully
- Trip header data matches item data
- LIFNR, TRUCK_NO, BUSINESS, SUBBUSINESS are consistent
- No exception is raised

**Validation Checklist:**
- [ ] Header FM executes successfully
- [ ] Item FM executes successfully
- [ ] Data is consistent between header and items
- [ ] No exception raised

---

### 2.4 Performance Test Scenarios

#### Test Case TC-017: Performance Test - Typical Query

**Objective:** Verify performance meets requirement (< 2 seconds for < 50 legs)

**Preconditions:**
- Test data is loaded
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPITM
2. Click "Test" button
3. Enter test data:
   - IV_TRIP_NO: 'TRIP001' (3 legs)
   - IV_USER_TYPE: 'V'
   - IV_USER_ID: '0000012345'
4. Click "Execute" (F8)
5. Note execution time from runtime analysis

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- Response time < 2 seconds (performance requirement)
- Database time < 50% of total runtime

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] Response time < 2 seconds
- [ ] Database time < 50% of total runtime

---

#### Test Case TC-018: Concurrent Calls Test

**Objective:** Verify function module handles concurrent calls

**Preconditions:**
- Test data is loaded
- Multiple test users are available

**Test Steps:**
1. Open multiple SE37 sessions (or use program to call FM concurrently)
2. Execute FM simultaneously for different trips:
   - User 1: TRIP001
   - User 2: TRIP002
   - User 3: TRIP003
3. Execute all calls simultaneously
4. Verify all complete successfully

**Expected Results:**
- All function module calls execute successfully
- No deadlocks or locking issues
- All return correct results
- Performance acceptable

**Validation Checklist:**
- [ ] All calls complete successfully
- [ ] No locking issues
- [ ] Results are correct
- [ ] Performance acceptable

---

## 3. Test Execution Checklist

### 3.1 Pre-Test Checklist

- [ ] Function Module is activated
- [ ] Message class ZCTD exists with all messages
- [ ] Test data is loaded in ZSCE_CTD_HDR
- [ ] Test data is loaded in ZSCE_CTD_ITM
- [ ] Test users are created with appropriate authorizations
- [ ] RFC authorization is configured
- [ ] Business/Sub-Business authorization is configured (for RIL Operations)

### 3.2 Test Execution

1. Execute Happy Path Tests (TC-001 to TC-005)
2. Execute Error Path Tests (TC-006 to TC-013)
3. Execute Edge Case Tests (TC-014 to TC-016)
4. Execute Performance Tests (TC-017 to TC-018)

### 3.3 Post-Test Checklist

- [ ] All test cases executed
- [ ] Test results documented
- [ ] Defects logged (if any)
- [ ] Test data cleaned up (optional)

---

## 4. Test Data Cleanup

### 4.1 Cleanup Script

After testing, optionally clean up test data:

```abap
*&---------------------------------------------------------------------*
*& Report  ZSCM_CTD_TEST_DATA_CLEANUP_ITM
*&---------------------------------------------------------------------*
*& Purpose: Clean up test data for ZSCM_CTD_GETTRIPITM testing
*&---------------------------------------------------------------------*
REPORT zscm_ctd_test_data_cleanup_itm.

DATA: lv_count_hdr TYPE i,
      lv_count_itm TYPE i.

" Delete test data
DELETE FROM zsce_ctd_itm WHERE trip_no LIKE 'TRIP%'.
DELETE FROM zsce_ctd_hdr WHERE trip_no LIKE 'TRIP%'.

" Get count
SELECT COUNT(*) FROM zsce_ctd_hdr INTO lv_count_hdr
  WHERE trip_no LIKE 'TRIP%'.

SELECT COUNT(*) FROM zsce_ctd_itm INTO lv_count_itm
  WHERE trip_no LIKE 'TRIP%'.

IF lv_count_hdr = 0 AND lv_count_itm = 0.
  COMMIT WORK.
  WRITE: / 'Test data cleaned up successfully'.
ELSE.
  WRITE: / 'Error: Test data still exists'.
  WRITE: / 'Header records:', lv_count_hdr.
  WRITE: / 'Item records:', lv_count_itm.
ENDIF.
```

---

## 5. Test Results Template

### 5.1 Test Execution Log

| Test Case ID | Test Case Name | Status | Execution Date | Executed By | Notes |
|--------------|----------------|--------|----------------|-------------|-------|
| TC-001 | Vendor User - Multiple Legs | | | | |
| TC-002 | Vendor User - Single Leg | | | | |
| TC-003 | RIL Operations User - Valid Trip | | | | |
| TC-004 | Trip with No Items | | | | |
| TC-005 | Verify Sorting by COUNTER | | | | |
| TC-006 | Trip Not Found | | | | |
| TC-007 | Blank Trip Number | | | | |
| TC-008 | Invalid User Type | | | | |
| TC-009 | Blank User Type | | | | |
| TC-010 | Blank User ID | | | | |
| TC-011 | Unauthorized Access - Vendor | | | | |
| TC-012 | No RFC Authorization | | | | |
| TC-013 | RIL Operations - No Authorization | | | | |
| TC-014 | Duplicate Counters | | | | |
| TC-015 | Large Trip - Performance | | | | |
| TC-016 | Integration with Header FM | | | | |
| TC-017 | Performance Test - Typical | | | | |
| TC-018 | Concurrent Calls Test | | | | |

**Status Values:**
- PASS: Test passed
- FAIL: Test failed
- BLOCKED: Test blocked (prerequisites not met)
- NOT RUN: Test not executed

---

## 6. Defect Log Template

### 6.1 Defect Log

| Defect ID | Test Case ID | Severity | Description | Status | Assigned To |
|-----------|--------------|----------|-------------|--------|-------------|
| | | | | | |

**Severity Values:**
- Critical: Functionality completely broken
- High: Major functionality issue
- Medium: Minor functionality issue
- Low: Cosmetic or minor issue

---

## 7. Approval

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Test Lead | | | |
| Functional Lead | | | |
| Technical Lead | | | |
| Project Manager | | | |

---

**Document End**

