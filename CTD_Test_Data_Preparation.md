# Test Data Preparation Guide
## CTD Trip Details Enrichment Program

---

## Quick Reference: Test Data Summary

### Test Trips Overview

| Trip Number | Status | Created Date | Scenario | Expected Result |
|-------------|--------|--------------|----------|-----------------|
| TRIP0000001 | 03 | 20240115 | Valid trip, 3 loaded legs | ✅ Processed, Status → 04 |
| TRIP0000002 | 03 | 20240116 | Area mismatch (AREA01 → AREA05) | ✅ Processed with empty leg, Status → 04 |
| TRIP0000003 | 03 | 20240117 | Blank destination area | ✅ Processed with empty leg, Status → 04 |
| TRIP0000004 | 03 | 20240118 | Missing source date | ❌ Excluded, Status → 03 |
| TRIP0000005 | 03 | 20240119 | Missing destination date | ❌ Excluded, Status → 03 |
| TRIP0000006 | 03 | 20240120 | Mixed loaded/empty legs | ✅ Processed, Status → 04 |
| TRIP0000007 | 02 | 20240121 | Not completed | ❌ Not selected |
| TRIP0000008 | 03 | 20240201 | Outside date range | ❌ Not selected |

---

## Step-by-Step Test Data Setup

### Step 1: Prepare Test Date Range
- **From Date:** 15.01.2024
- **To Date:** 20.01.2024
- This range covers TRIP0000001 through TRIP0000006

### Step 2: Create Trip Headers (ZSCE_CTD_HDR)

Use SE16 or SQL to insert:

```
TRIP0000001 | VENDOR01 | TRUCK001 | 03 | 20240115
TRIP0000002 | VENDOR01 | TRUCK002 | 03 | 20240116
TRIP0000003 | VENDOR02 | TRUCK003 | 03 | 20240117
TRIP0000004 | VENDOR01 | TRUCK001 | 03 | 20240118
TRIP0000005 | VENDOR03 | TRUCK004 | 03 | 20240119
TRIP0000006 | VENDOR02 | TRUCK005 | 03 | 20240120
```

### Step 3: Create Trip Legs (ZSCE_CTD_ITM)

**TRIP0000001 - 3 Legs:**
```
Leg 1: SHIP001 | 20240115 | 20240116 | SO | AREA01
Leg 2: SHIP002 | 20240116 | 20240117 | SO | AREA02
Leg 3: SHIP003 | 20240117 | 20240118 | PO | AREA03
```

**TRIP0000002 - 2 Legs (Area Mismatch):**
```
Leg 1: SHIP004 | 20240116 | 20240117 | SO | AREA01
Leg 2: SHIP005 | 20240118 | 20240119 | STO | AREA05
Note: AREA01 ≠ AREA05 → Empty leg will be inserted
```

**TRIP0000003 - 2 Legs (Blank Destination Area):**
```
Leg 1: SHIP006 | 20240117 | 20240118 | SO | AREA02 (blank dest area)
Leg 2: SHIP007 | 20240119 | 20240120 | PO | AREA04
Note: Blank dest area → Empty leg will be inserted
```

**TRIP0000004 - Invalid (Missing Source Date):**
```
Leg 1: SHIP008 | 00000000 | 20240118 | SO | AREA01
Note: Will be excluded
```

**TRIP0000005 - Invalid (Missing Destination Date):**
```
Leg 1: SHIP009 | 20240119 | 00000000 | SO | AREA02
Note: Will be excluded
```

**TRIP0000006 - 2 Legs (Mixed):**
```
Leg 1: SHIP010 | 20240120 | 20240121 | SO | AREA03
Leg 2: (empty) | 20240122 | 20240123 | | AREA04
Note: Leg 2 already empty
```

### Step 4: Create Shipment Data

**OIGSS (Shipment Header):**
- Create 10 shipments: SHIP001 through SHIP010
- Set TSTYP = '1' for all
- Assign routes, distances, zones, and addresses

**OIGSI (Shipment Item):**
- Map shipments to deliveries: SHIP001 → DELV001, etc.

**LIPS (Delivery Item):**
- Map deliveries to materials: DELV001 → MAT001, etc.

### Step 5: Create Address Data (ADRC)

Create 20 addresses (ADR001 through ADR020) with corresponding regions.

### Step 6: Create Supporting Data

**YTTSTX0001:** For PO/STO movements (SHIP003, SHIP005, SHIP007)
**YTTSTX0002:** For SO/STO movements (SHIP001, SHIP002, SHIP004, SHIP005, SHIP006, SHIP010)
**TROLZ:** Route determination for empty legs (VSBED='03', TRAGR='0006')
**TVRO:** Route distances

---

## Test Execution Quick Guide

### Test Case 1: Basic Functionality
1. Execute program with:
   - From Date: 15.01.2024
   - To Date: 20.01.2024
   - Trip: TRIP0000001
   - Test Run: Unchecked
2. Verify:
   - All 3 legs enriched
   - Material, regions populated
   - Status updated to 04

### Test Case 2: Empty Leg Insertion
1. Execute program with:
   - Trip: TRIP0000002
2. Verify:
   - Empty leg inserted between leg 1 and leg 2
   - Counter adjusted (leg 2 becomes leg 3)
   - Route and distance populated

### Test Case 3: Validation Failure
1. Execute program with:
   - Trip: TRIP0000004
2. Verify:
   - Trip excluded
   - Error in log
   - Status remains 03

### Test Case 4: Test Run Mode
1. Execute program with:
   - Trip: TRIP0000001
   - Test Run: Checked
2. Verify:
   - ALV shows enriched data
   - No database updates
   - Status remains 03

---

## Data Validation Queries

### Check Trip Headers
```sql
SELECT * FROM zsce_ctd_hdr WHERE trip_no LIKE 'TRIP%' ORDER BY trip_no;
```

### Check Trip Legs
```sql
SELECT * FROM zsce_ctd_itm WHERE trip_no LIKE 'TRIP%' ORDER BY trip_no, counter;
```

### Check Enrichment Results
```sql
SELECT trip_no, counter, leg_type, route, distance, matnr, 
       source_region, dest_region, business_id, subbusiness_id
FROM zsce_ctd_itm 
WHERE trip_no LIKE 'TRIP%' 
ORDER BY trip_no, counter;
```

### Check Trip Status Updates
```sql
SELECT trip_no, trip_status, created_date 
FROM zsce_ctd_hdr 
WHERE trip_no LIKE 'TRIP%' 
ORDER BY trip_no;
```

---

## Common Issues and Solutions

### Issue 1: No Trips Selected
**Solution:** Check:
- Trip status = '03'
- Created date within range
- Authorization check passed

### Issue 2: Empty Leg Not Inserted
**Solution:** Check:
- Area mismatch exists
- Route and distance available in TROLZ/TVRO
- Zones are populated

### Issue 3: Material Not Found
**Solution:** Check:
- OIGSI has mapping (SHNUMBER → DOC_NUMBER)
- LIPS has mapping (VBELN → MATNR)
- Data exists for shipment

### Issue 4: Region Not Found
**Solution:** Check:
- ADRC has address records
- ADRNR and ADRNZ are populated in OIGSS
- Address numbers match ADRC

---

## Test Data Cleanup

After testing, clean up test data:

```sql
DELETE FROM zsce_ctd_itm WHERE trip_no LIKE 'TRIP%';
DELETE FROM zsce_ctd_hdr WHERE trip_no LIKE 'TRIP%';
```

**Note:** Only delete test data. Do not delete production data.

---

## Function Module Mock Data

If Z_SCM_GET_BUSINESS is not available, create a test version that returns:

```
SHIP001 → BUS001, SUB001
SHIP002 → BUS002, SUB002
SHIP003 → BUS001, SUB003
... (see test scenarios document)
```

---

## Performance Testing Data

For performance testing, create:
- 100+ trips
- 500+ legs
- Multiple vendors
- Various date ranges

Use batch job (SM36) to execute and monitor performance.

---

**Last Updated:** [Date]
**Version:** 1.0

