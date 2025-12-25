# CTD Trip Details Enrichment Program

## 1. Purpose
The purpose of this development is to create a new custom SAP program and function module to enrich CTD (Converted to Dedicated) Trip Details for Completed Trips.

The enrichment logic:
- Derives additional attributes for each Trip Leg (Loaded and Empty)
- Validates trip completeness
- Inserts Empty Legs where required
- Updates CTD tables accordingly

The solution supports:
- Simulation (Test Run) mode
- Foreground and background execution
- Trip Status update to reflect the next stage in the CTD lifecycle

---

## 2. Objects in Scope

| Object Type      | Name                       |
|------------------|----------------------------|
| Program          | ZSCM_CTD_ENRICHTRIPDETAILS |
| Transaction Code | ZCTD_ENRICH_TRIPDETAILS    |
| Function Module  | ZSCM_CTD_ENRICHTRIPDETAILS |
| Header Table     | ZSCE_CTD_HDR               |
| Item Table       | ZSCE_CTD_ITM               |

---

## 3. Selection Screen

### Mandatory Inputs
- **From Date** (DATS, 8)
- **To Date** (DATS, 8)

### Optional Inputs
- **Trip Number** (CHAR14 – Multiple selection)

### Additional Option
- **Test Run / Simulation Mode** (Checkbox)
  - When selected, logic is executed without committing any database updates

The program must support both foreground and background execution.

---

## 4. Function Module Design

### FM Name
**ZSCM_CTD_ENRICHTRIPDETAILS**

### Import Parameters

| Parameter        | Type            | Mandatory |
|------------------|-----------------|-----------|
| I_FROM_DATE      | DATS            | Yes       |
| I_TO_DATE        | DATS            | Yes       |
| IT_TRIP_NUMBERS  | CHAR14 (Table)  | No        |
| I_TEST_RUN       | CHAR1 (X/Blank) | No        |

### Export Parameters

| Parameter  | Description                   |
|------------|-------------------------------|
| ET_OUTPUT  | Enriched Trip Leg Data        |
| ET_LOG     | Processing and Error Log      |

> All enrichment logic must be executed inside the Function Module.  
> Database updates must be triggered only from the calling program.

---

## 5. High-Level Processing Flow

1. Fetch Completed Trips (Header)
2. Fetch Trip Legs (Items)
3. Validate Source and Destination Dates
4. Enrich Loaded Leg Shipment Details
5. Determine and Insert Empty Legs
6. Classify Legs (Loaded / Empty)
7. Fetch Material and Region Details
8. Prepare ALV Output
9. Update CTD Tables and Trip Status (Non-Test Run)

---

## 6. Detailed Processing Logic

### Step 1: Fetch Trip Header Records
- Fetch records from **ZSCE_CTD_HDR** where:
  - `TRIP_STATUS = '03'` (Completed)
  - `CREATED_DATE` falls within the selected date range
- If Trip Number(s) are provided:
  - Fetch only those Trips
  - Validate `CREATED_DATE` is within the input date range
  - Else raise error:  
    **"Selected Trip does not fall within the given date range"**

Only valid Trips are considered for further processing.

---

### Step 2: Fetch Trip Item (Leg) Records
- Fetch corresponding records from **ZSCE_CTD_ITM**
- Extract key leg-level fields:
  - LIFNR, TRUCK_NO, TRIP_NO, COUNTER, SHNUMBER
  - SOURCE_DATE, DEST_DATE, MVT_TYPE, AREA, etc.

#### Validation Rule
- If any leg has SOURCE_DATE or DEST_DATE blank:
  - Exclude the entire Trip from further processing
  - Do not perform CTD enrichment for that Trip

---

### Step 3: Enrich Shipment Details for Loaded Legs
- For all legs where SHNUMBER is populated:
  - Fetch shipment details from **OIGSS** (`TSTYP = '1'`)
  - Enrich internal table with:
    - Route, Distance
    - Source Zone, Destination Zone
    - Source/Destination Addresses

#### Date Determination Logic

**DEST_EXIT_DATE**
- SO → DEST_DATE
- PO / STO → MG_EXIT_DT from **YTTSTX0001** (`TRK_PURPOS = 'R'`)

**SOURCE_ENT_DATE**
- SO / STO → PP_ENTR_DT from **YTTSTX0001** (via REPORT_NO from **YTTSTX0002**)
- PO → SOURCE_DATE

#### Business Information
- Call FM **Z_SCM_GET_BUSINESS** using SHNUMBER
- Derive:
  - BUSINESS_ID
  - SUBBUSINESS_ID

---

### Step 4: Empty Leg Determination and Insertion
- Sort legs by **COUNTER** (ascending)
- Compare consecutive legs

#### Insert Empty Leg When
- Destination Area of preceding leg is blank, or
- Destination Area ≠ Source Area of subsequent leg

#### Empty Leg Derivation
- COUNTER = previous COUNTER + 1
- SOURCE_ZONE = Destination Zone of preceding leg
- DEST_ZONE = Source Zone of subsequent leg
- SOURCE_DATE = DEST_EXIT_DATE of preceding leg
- DEST_DATE = SOURCE_ENT_DATE of subsequent leg
- Route → **TROLZ** (`VSBED = '03'`, `TRAGR = '0006'`)
- Distance → **TVRO**

Empty Legs are added only to the internal table and persisted during final commit.

---

### Step 5: Leg Classification and Validation
- SHNUMBER populated → **Loaded Leg** (`LEG_TYPE = 'L'`)
- SHNUMBER blank → **Empty Leg** (`LEG_TYPE = 'E'`)

#### Validation
- If any Empty Leg has missing Route or Distance:
  - Exclude the Trip from further processing
  - Populate:
    - `CTD_RULEENG_REMARKS = 'Empty Leg Route / Distance missing'`

---

### Step 6: Material and Region Determination (Loaded Legs)

#### Material Determination
1. Pass SHNUMBER to **OIGSI** → fetch Delivery (DOC_NUMBER)
2. Pass DOC_NUMBER to **LIPS** (VBELN) → fetch MATNR

#### Region Determination
- Pass ADRNR to **ADRC** → fetch SOURCE_REGION
- Pass ADRNZ to **ADRC** → fetch DEST_REGION

---

## 7. ALV Output Requirements
- Display all enriched Trip Leg records
- Sort by **LIFNR, TRUCK_NO, TRIP_NO**
- Highlight records with validation issues in **RED**
- Enable filter, sort, and download options

---

## 8. Database Update and Trip Status Management

### CTD Table Updates
- Update **ZSCE_CTD_ITM** with enriched leg-level data
- Update **ZSCE_CTD_HDR** with derived Trip-level details

### Trip Status Update Logic
- Initial Status: `'03'` (Completed)
- Updated Status: `'04'`  
  *(Trip Details Updated / Pending for Transporter Trip Confirmation)*

#### Conditions
- Trip must pass all validations
- Enrichment logic must complete successfully

### Simulation / Test Run Handling

**When `I_TEST_RUN = 'X'`:**
- No updates to database tables
- ALV output displays derived Trip Status `'04'` for reference only

**When `I_TEST_RUN` is blank:**
- Perform database updates in a single LUW
- Commit once after all eligible Trips are processed

Trips excluded due to validation errors retain original Trip Status `'03'`.
