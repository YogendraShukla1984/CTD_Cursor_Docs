# Test Data and Test Scenarios
## Function Module: ZSCM_CTD_GETTRIPHDR

---

**Document Information**

| Field | Value |
|-------|-------|
| **Document Type** | Test Data and Test Scenarios |
| **Function Module Name** | ZSCM_CTD_GETTRIPHDR |
| **Version** | 1.0 |
| **Date** | [Current Date] |
| **Author** | [Author Name] |
| **Related TS** | CTD_BRD02_TS_Fetch_CTD_Trip_Header |

---

## 1. Test Data Preparation

### 1.1 Prerequisites

Before executing tests, ensure:
- Function Module ZSCM_CTD_GETTRIPHDR is activated
- Message class ZCTD exists with messages 001-006
- Test users are created with appropriate authorizations
- Test data tables are accessible

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

Insert the following test records into ZSCE_CTD_HDR:

**Test Data Set 1: Vendor 0000012345 - Status '04' (Pending)**

| TRIP_NO | LIFNR | TRUCK_NO | TRIP_STATUS | CREATED_DATE | CREATED_TIME | CREATED_BY | BUSINESS | SUBBUSINESS |
|---------|-------|----------|-------------|--------------|--------------|------------|----------|-------------|
| TRIP001 | 0000012345 | TRUCK001 | 04 | 20240115 | 100000 | TESTUSER | BUS001 | SUB001 |
| TRIP002 | 0000012345 | TRUCK001 | 04 | 20240120 | 110000 | TESTUSER | BUS001 | SUB001 |
| TRIP003 | 0000012345 | TRUCK002 | 04 | 20240125 | 120000 | TESTUSER | BUS001 | SUB002 |
| TRIP004 | 0000012345 | TRUCK003 | 04 | 20240130 | 130000 | TESTUSER | BUS002 | SUB003 |

**Test Data Set 2: Vendor 0000012345 - Status '06' (Rejected)**

| TRIP_NO | LIFNR | TRUCK_NO | TRIP_STATUS | CREATED_DATE | CREATED_TIME | CREATED_BY | BUSINESS | SUBBUSINESS |
|---------|-------|----------|-------------|--------------|--------------|------------|----------|-------------|
| TRIP005 | 0000012345 | TRUCK001 | 06 | 20240110 | 090000 | TESTUSER | BUS001 | SUB001 |
| TRIP006 | 0000012345 | TRUCK002 | 06 | 20240118 | 140000 | TESTUSER | BUS001 | SUB002 |

**Test Data Set 3: Vendor 0000012346 - Status '04' (Pending)**

| TRIP_NO | LIFNR | TRUCK_NO | TRIP_STATUS | CREATED_DATE | CREATED_TIME | CREATED_BY | BUSINESS | SUBBUSINESS |
|---------|-------|----------|-------------|--------------|--------------|------------|----------|-------------|
| TRIP007 | 0000012346 | TRUCK004 | 04 | 20240116 | 100000 | TESTUSER | BUS001 | SUB001 |
| TRIP008 | 0000012346 | TRUCK005 | 04 | 20240122 | 110000 | TESTUSER | BUS002 | SUB003 |

**Test Data Set 4: Other Statuses (Should NOT be returned)**

| TRIP_NO | LIFNR | TRUCK_NO | TRIP_STATUS | CREATED_DATE | CREATED_TIME | CREATED_BY | BUSINESS | SUBBUSINESS |
|---------|-------|----------|-------------|--------------|--------------|------------|----------|-------------|
| TRIP009 | 0000012345 | TRUCK001 | 01 | 20240112 | 080000 | TESTUSER | BUS001 | SUB001 |
| TRIP010 | 0000012345 | TRUCK001 | 03 | 20240114 | 090000 | TESTUSER | BUS001 | SUB001 |
| TRIP011 | 0000012345 | TRUCK001 | 05 | 20240117 | 100000 | TESTUSER | BUS001 | SUB001 |

**Test Data Set 5: Out of Date Range (Should NOT be returned)**

| TRIP_NO | LIFNR | TRUCK_NO | TRIP_STATUS | CREATED_DATE | CREATED_TIME | CREATED_BY | BUSINESS | SUBBUSINESS |
|---------|-------|----------|-------------|--------------|--------------|------------|----------|-------------|
| TRIP012 | 0000012345 | TRUCK001 | 04 | 20231231 | 100000 | TESTUSER | BUS001 | SUB001 |
| TRIP013 | 0000012345 | TRUCK001 | 04 | 20250201 | 100000 | TESTUSER | BUS001 | SUB001 |

**Test Data Set 6: RIL Operations - Business/Sub-Business Authorization**

| TRIP_NO | LIFNR | TRUCK_NO | TRIP_STATUS | CREATED_DATE | CREATED_TIME | CREATED_BY | BUSINESS | SUBBUSINESS |
|---------|-------|----------|-------------|--------------|--------------|------------|----------|-------------|
| TRIP014 | 0000012347 | TRUCK006 | 04 | 20240115 | 100000 | TESTUSER | BUS003 | SUB004 |
| TRIP015 | 0000012347 | TRUCK007 | 06 | 20240120 | 110000 | TESTUSER | BUS003 | SUB005 |
| TRIP016 | 0000012347 | TRUCK008 | 04 | 20240125 | 120000 | TESTUSER | BUS004 | SUB006 |

#### 1.2.3 SQL Scripts for Test Data Insertion

**Option 1: Using SE16 (Table Maintenance)**

1. Go to SE16 → Enter table name: ZSCE_CTD_HDR
2. Click "Create Entries"
3. Enter test data manually

**Option 2: Using ABAP Program (Recommended for Bulk Data)**

```abap
*&---------------------------------------------------------------------*
*& Report  ZSCM_CTD_TEST_DATA_SETUP
*&---------------------------------------------------------------------*
*& Purpose: Create test data for ZSCM_CTD_GETTRIPHDR testing
*&---------------------------------------------------------------------*
REPORT zscm_ctd_test_data_setup.

DATA: lw_hdr TYPE zsce_ctd_hdr.

" Clear existing test data (optional)
DELETE FROM zsce_ctd_hdr WHERE trip_no LIKE 'TRIP%'.

" Test Data Set 1: Vendor 0000012345 - Status '04'
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

" Repeat for all test records...
" (Add all test data records)

COMMIT WORK.

WRITE: / 'Test data created successfully'.
```

#### 1.2.4 User Setup for Testing

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

#### Test Case TC-001: Vendor User - Valid Data - Multiple Trips

**Objective:** Verify vendor user can retrieve all trips for their vendor

**Preconditions:**
- Test data is loaded
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20240131'
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_HDR contains 6 records:
  - TRIP001, TRIP002, TRIP003, TRIP004 (Status '04')
  - TRIP005, TRIP006 (Status '06')
- All records have LIFNR = '0000012345'
- All records have TRIP_STATUS in ('04', '06')
- All records have CREATED_DATE between '20240101' and '20240131'
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] ET_TRIP_HDR contains 6 records
- [ ] All LIFNR = '0000012345'
- [ ] All TRIP_STATUS in ('04', '06')
- [ ] All CREATED_DATE in range
- [ ] No exception raised

---

#### Test Case TC-002: Vendor User - With Truck Number Filter

**Objective:** Verify truck number filter works correctly

**Preconditions:**
- Test data is loaded
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20240131'
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: 'TRUCK001'
4. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_HDR contains 3 records:
  - TRIP001, TRIP002 (Status '04')
  - TRIP005 (Status '06')
- All records have TRUCK_NO = 'TRUCK001'
- All records have LIFNR = '0000012345'
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] ET_TRIP_HDR contains 3 records
- [ ] All TRUCK_NO = 'TRUCK001'
- [ ] All LIFNR = '0000012345'
- [ ] No exception raised

---

#### Test Case TC-003: RIL Operations User - Valid Data

**Objective:** Verify RIL Operations user can retrieve trips for authorized Business/Sub-Business

**Preconditions:**
- Test data is loaded
- User RILUSER1 is logged in
- User has authorization for BUS001, BUS002, SUB001, SUB002, SUB003

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'R'
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20240131'
   - IV_USER_ID: 'RILUSER1'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_HDR contains trips with:
  - BUSINESS in ('BUS001', 'BUS002')
  - SUBBUSINESS in ('SUB001', 'SUB002', 'SUB003')
  - TRIP_STATUS in ('04', '06')
  - CREATED_DATE between '20240101' and '20240131'
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] All BUSINESS in authorized list
- [ ] All SUBBUSINESS in authorized list
- [ ] All TRIP_STATUS in ('04', '06')
- [ ] No exception raised

**Note:** Actual count depends on authorization implementation. If authorization is not implemented, this test may need adjustment.

---

#### Test Case TC-004: No Data Found - Valid Scenario

**Objective:** Verify empty result set is handled correctly (not an error)

**Preconditions:**
- Test data is loaded
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '20250101'
   - IV_TO_DATE: '20250131'
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_HDR is empty (no records)
- No exception is raised
- This is a valid scenario, not an error

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] ET_TRIP_HDR is empty
- [ ] No exception raised
- [ ] No error message displayed

---

#### Test Case TC-005: Single Day Date Range

**Objective:** Verify single day date range works correctly

**Preconditions:**
- Test data is loaded
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '20240115'
   - IV_TO_DATE: '20240115'
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_HDR contains 1 record: TRIP001
- Record has CREATED_DATE = '20240115'
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] ET_TRIP_HDR contains 1 record
- [ ] CREATED_DATE = '20240115'
- [ ] No exception raised

---

### 2.2 Error Path Test Scenarios

#### Test Case TC-006: Invalid User Type

**Objective:** Verify exception is raised for invalid user type

**Preconditions:**
- Function Module is activated
- User is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'X' (invalid)
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20240131'
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)

**Expected Results:**
- Exception INVALID_USER_TYPE is raised
- SY-SUBRC = 1 (exception code)
- Error message displayed: "Invalid user type. Must be V or R"
- ET_TRIP_HDR is empty

**Validation Checklist:**
- [ ] Exception INVALID_USER_TYPE raised
- [ ] SY-SUBRC = 1
- [ ] Error message is correct
- [ ] ET_TRIP_HDR is empty

---

#### Test Case TC-007: Blank User Type

**Objective:** Verify exception is raised for blank user type

**Preconditions:**
- Function Module is activated
- User is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: '' (blank)
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20240131'
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)

**Expected Results:**
- Exception INVALID_USER_TYPE is raised
- SY-SUBRC = 1
- Error message displayed: "User type is mandatory"
- ET_TRIP_HDR is empty

**Validation Checklist:**
- [ ] Exception INVALID_USER_TYPE raised
- [ ] SY-SUBRC = 1
- [ ] Error message is correct
- [ ] ET_TRIP_HDR is empty

---

#### Test Case TC-008: Invalid Date Range (FROM > TO)

**Objective:** Verify exception is raised when FROM_DATE > TO_DATE

**Preconditions:**
- Function Module is activated
- User is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '20240131'
   - IV_TO_DATE: '20240101' (FROM > TO)
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)

**Expected Results:**
- Exception INVALID_DATE_RANGE is raised
- SY-SUBRC = 2
- Error message displayed: "From date must be <= To date"
- ET_TRIP_HDR is empty

**Validation Checklist:**
- [ ] Exception INVALID_DATE_RANGE raised
- [ ] SY-SUBRC = 2
- [ ] Error message is correct
- [ ] ET_TRIP_HDR is empty

---

#### Test Case TC-009: Blank Date Range

**Objective:** Verify exception is raised for blank dates

**Preconditions:**
- Function Module is activated
- User is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '' (blank)
   - IV_TO_DATE: '20240131'
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)

**Expected Results:**
- Exception INVALID_DATE_RANGE is raised
- SY-SUBRC = 2
- Error message displayed: "Date range is mandatory"
- ET_TRIP_HDR is empty

**Validation Checklist:**
- [ ] Exception INVALID_DATE_RANGE raised
- [ ] SY-SUBRC = 2
- [ ] Error message is correct
- [ ] ET_TRIP_HDR is empty

---

#### Test Case TC-010: Blank User ID

**Objective:** Verify exception is raised for blank user ID

**Preconditions:**
- Function Module is activated
- User is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20240131'
   - IV_USER_ID: '' (blank)
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)

**Expected Results:**
- Exception INVALID_USER_ID is raised
- SY-SUBRC = 3
- Error message displayed: "User ID is mandatory"
- ET_TRIP_HDR is empty

**Validation Checklist:**
- [ ] Exception INVALID_USER_ID raised
- [ ] SY-SUBRC = 3
- [ ] Error message is correct
- [ ] ET_TRIP_HDR is empty

---

#### Test Case TC-011: No RFC Authorization

**Objective:** Verify exception is raised when user lacks RFC authorization

**Preconditions:**
- Function Module is activated
- User UNAUTHUSER is logged in (no RFC authorization)

**Test Steps:**
1. Log in as UNAUTHUSER (user without RFC authorization)
2. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
3. Click "Test" button
4. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20240131'
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: (leave blank)
5. Click "Execute" (F8)

**Expected Results:**
- Exception NO_AUTHORITY is raised
- SY-SUBRC = 5
- Error message displayed: "No RFC authorization"
- ET_TRIP_HDR is empty

**Validation Checklist:**
- [ ] Exception NO_AUTHORITY raised
- [ ] SY-SUBRC = 5
- [ ] Error message is correct
- [ ] ET_TRIP_HDR is empty

---

### 2.3 Edge Case Test Scenarios

#### Test Case TC-012: Large Date Range (1 Year)

**Objective:** Verify performance with large date range

**Preconditions:**
- Test data is loaded (may need additional data for 1 year)
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20241231' (1 year range)
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)
5. Note the execution time

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_HDR contains all matching records
- Response time < 10 seconds (performance requirement)
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] All matching records returned
- [ ] Response time < 10 seconds
- [ ] No exception raised

---

#### Test Case TC-013: Vendor Accessing Other Vendor's Data

**Objective:** Verify vendor cannot access other vendor's trips

**Preconditions:**
- Test data is loaded
- User TESTUSER1 (Vendor 0000012345) is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20240131'
   - IV_USER_ID: '0000012345' (but try to access Vendor 0000012346's data)
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_HDR contains only trips for Vendor 0000012345
- No trips for Vendor 0000012346 are returned
- No exception is raised (filtering is implicit in WHERE clause)

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] Only Vendor 0000012345's trips returned
- [ ] No Vendor 0000012346's trips returned
- [ ] No exception raised

**Note:** This test verifies that the WHERE clause correctly filters by LIFNR.

---

#### Test Case TC-014: RIL Operations - No Authorization

**Objective:** Verify RIL Operations user with no authorization gets empty result

**Preconditions:**
- Test data is loaded
- User UNAUTHUSER (RIL Operations with no Business authorization) is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'R'
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20240131'
   - IV_USER_ID: 'UNAUTHUSER'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_HDR is empty (no authorization = no data)
- No exception is raised (empty result is valid)

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] ET_TRIP_HDR is empty
- [ ] No exception raised

**Note:** Behavior depends on authorization implementation. If authorization check raises exception, adjust expected result.

---

#### Test Case TC-015: Multiple Status Filtering

**Objective:** Verify both status '04' and '06' are returned

**Preconditions:**
- Test data is loaded
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20240131'
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)
5. Verify both statuses are present

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- ET_TRIP_HDR contains trips with both status '04' and '06'
- Status values are only '04' or '06' (no other statuses)
- No exception is raised

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] Both status '04' and '06' present
- [ ] No other statuses present
- [ ] No exception raised

---

### 2.4 Performance Test Scenarios

#### Test Case TC-016: Performance Test - Typical Query

**Objective:** Verify performance meets requirement (< 3 seconds)

**Preconditions:**
- Test data is loaded
- User TESTUSER1 is logged in

**Test Steps:**
1. Open SE37 → Function Module: ZSCM_CTD_GETTRIPHDR
2. Click "Test" button
3. Enter test data:
   - IV_USER_TYPE: 'V'
   - IV_FROM_DATE: '20240101'
   - IV_TO_DATE: '20240131' (1 month range)
   - IV_USER_ID: '0000012345'
   - IV_TRUCK_NO: (leave blank)
4. Click "Execute" (F8)
5. Note execution time from runtime analysis

**Expected Results:**
- Function Module executes successfully (SY-SUBRC = 0)
- Response time < 3 seconds (performance requirement)
- Database time < 50% of total runtime

**Validation Checklist:**
- [ ] SY-SUBRC = 0
- [ ] Response time < 3 seconds
- [ ] Database time < 50% of total runtime

---

#### Test Case TC-017: Concurrent Calls Test

**Objective:** Verify function module handles concurrent calls

**Preconditions:**
- Test data is loaded
- Multiple test users are available

**Test Steps:**
1. Open multiple SE37 sessions (or use program to call FM concurrently)
2. Execute FM simultaneously from different users:
   - User 1: Vendor 0000012345
   - User 2: Vendor 0000012346
   - User 3: RIL Operations user
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
- [ ] Test users are created with appropriate authorizations
- [ ] RFC authorization is configured
- [ ] Business/Sub-Business authorization is configured (for RIL Operations)

### 3.2 Test Execution

1. Execute Happy Path Tests (TC-001 to TC-005)
2. Execute Error Path Tests (TC-006 to TC-011)
3. Execute Edge Case Tests (TC-012 to TC-015)
4. Execute Performance Tests (TC-016 to TC-017)

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
*& Report  ZSCM_CTD_TEST_DATA_CLEANUP
*&---------------------------------------------------------------------*
*& Purpose: Clean up test data for ZSCM_CTD_GETTRIPHDR testing
*&---------------------------------------------------------------------*
REPORT zscm_ctd_test_data_cleanup.

DATA: lv_count TYPE i.

" Delete test data
DELETE FROM zsce_ctd_hdr WHERE trip_no LIKE 'TRIP%'.

" Get count
SELECT COUNT(*) FROM zsce_ctd_hdr INTO lv_count
  WHERE trip_no LIKE 'TRIP%'.

IF lv_count = 0.
  COMMIT WORK.
  WRITE: / 'Test data cleaned up successfully'.
ELSE.
  WRITE: / 'Error: Test data still exists'.
ENDIF.
```

---

## 5. Test Results Template

### 5.1 Test Execution Log

| Test Case ID | Test Case Name | Status | Execution Date | Executed By | Notes |
|--------------|----------------|--------|----------------|-------------|-------|
| TC-001 | Vendor User - Valid Data | | | | |
| TC-002 | Vendor User - With Truck Filter | | | | |
| TC-003 | RIL Operations User - Valid Data | | | | |
| TC-004 | No Data Found | | | | |
| TC-005 | Single Day Date Range | | | | |
| TC-006 | Invalid User Type | | | | |
| TC-007 | Blank User Type | | | | |
| TC-008 | Invalid Date Range | | | | |
| TC-009 | Blank Date Range | | | | |
| TC-010 | Blank User ID | | | | |
| TC-011 | No RFC Authorization | | | | |
| TC-012 | Large Date Range | | | | |
| TC-013 | Vendor Access Control | | | | |
| TC-014 | RIL Operations - No Authorization | | | | |
| TC-015 | Multiple Status Filtering | | | | |
| TC-016 | Performance Test - Typical | | | | |
| TC-017 | Concurrent Calls Test | | | | |

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

