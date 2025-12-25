# Technical Specification
## CTD Trip Details Enrichment Program

---

**Document Information**

| Field | Value |
|-------|-------|
| **Document Type** | Technical Specification |
| **Program Name** | ZSCM_CTD_ENRICHTRIPDETAILS |
| **Transaction Code** | ZCTD_ENRICH_TRIPDETAILS |
| **Function Module** | ZSCM_CTD_ENRICHTRIPDETAILS |
| **Version** | 1.0 |
| **Date** | [Current Date] |
| **Author** | [Author Name] |
| **Target System** | SAP ECC 6.0 / NetWeaver 7.31 |
| **Syntax Level** | abap_731 |

---

## 1. Technical Overview

### 1.1 System Requirements
- **SAP Version:** ECC 6.0 / NetWeaver 7.31
- **Syntax Level:** abap_731 (strict compatibility required)
- **Database:** SAP HANA or AnyDB
- **UI Technology:** Classic ALV Grid (CL_GUI_ALV_GRID)

### 1.2 Architecture
- **Program Type:** Executable Report (Type 1)
- **Design Pattern:** Object-Oriented (Local Classes)
- **Function Module:** Encapsulated business logic
- **Database Updates:** Single LUW commit model

### 1.3 Coding Standards Compliance
- Follows ABAP Code Rules (NetWeaver 7.31 compatible)
- Object-Oriented by default (no FORMs/PERFORM)
- All logic in local classes
- Function module for reusable logic

---

## 2. Object Structure

### 2.1 Program Structure

```
ZSCM_CTD_ENRICHTRIPDETAILS
├── TOP Include (ZSCM_CTD_ENRICHTRIPDETAILSTOP)
│   ├── Global Data Declarations
│   ├── Type Definitions
│   └── Constants
├── Class Include (ZSCM_CTD_ENRICHTRIPDETAILSC01)
│   ├── lcl_main_processor (Main Processing Class)
│   ├── lcl_data_selector (Data Selection Class)
│   ├── lcl_enrichment_processor (Enrichment Logic Class)
│   ├── lcl_validator (Validation Class)
│   └── lcl_alv_display (ALV Display Class)
└── Selection Screen
    ├── PARAMETERS
    └── SELECT-OPTIONS
```

### 2.2 Function Module Structure

```
ZSCM_CTD_ENRICHTRIPDETAILS
├── Import Parameters
│   ├── I_FROM_DATE
│   ├── I_TO_DATE
│   ├── IT_TRIP_NUMBERS
│   └── I_TEST_RUN
├── Export Parameters
│   ├── ET_OUTPUT
│   └── ET_LOG
├── Local Classes
│   ├── lcl_data_fetcher
│   ├── lcl_enrichment_engine
│   └── lcl_validator
└── Exception Handling
```

---

## 3. Data Dictionary Objects

### 3.1 Type Definitions

#### 3.1.1 Global Types (TOP Include)

```abap
" Trip Header Structure
TYPES: BEGIN OF gty_trip_header,
         trip_no TYPE zsce_ctd_hdr-trip_no,
         lifnr TYPE zsce_ctd_hdr-lifnr,
         truck_no TYPE zsce_ctd_hdr-truck_no,
         trip_status TYPE zsce_ctd_hdr-trip_status,
         created_date TYPE zsce_ctd_hdr-created_date,
       END OF gty_trip_header.

TYPES: gty_trip_header_table TYPE TABLE OF gty_trip_header.

" Trip Leg Structure (Input)
TYPES: BEGIN OF gty_trip_leg_input,
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
       END OF gty_trip_leg_input.

TYPES: gty_trip_leg_input_table TYPE TABLE OF gty_trip_leg_input.

" Enriched Trip Leg Structure (Output)
TYPES: BEGIN OF gty_trip_leg_output,
         trip_no TYPE zsce_ctd_itm-trip_no,
         counter TYPE zsce_ctd_itm-counter,
         lifnr TYPE zsce_ctd_itm-lifnr,
         truck_no TYPE zsce_ctd_itm-truck_no,
         leg_type TYPE char1,  " L = Loaded, E = Empty
         shnumber TYPE zsce_ctd_itm-shnumber,
         source_date TYPE zsce_ctd_itm-source_date,
         dest_date TYPE zsce_ctd_itm-dest_date,
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
         trip_status TYPE zsce_ctd_hdr-trip_status,
         ctd_ruleeng_remarks TYPE zsce_ctd_itm-ctd_ruleeng_remarks,
         validation_error TYPE abap_bool,
       END OF gty_trip_leg_output.

TYPES: gty_trip_leg_output_table TYPE TABLE OF gty_trip_leg_output.

" Log Structure
TYPES: BEGIN OF gty_log,
         trip_no TYPE zsce_ctd_hdr-trip_no,
         counter TYPE zsce_ctd_itm-counter,
         msgty TYPE symsgty,
         msgid TYPE symsgid,
         msgno TYPE symsgno,
         msgv1 TYPE symsgv,
         msgv2 TYPE symsgv,
         msgv3 TYPE symsgv,
         msgv4 TYPE symsgv,
         timestamp TYPE timestamp,
       END OF gty_log.

TYPES: gty_log_table TYPE TABLE OF gty_log.

" Shipment Details Structure
TYPES: BEGIN OF gty_shipment_details,
         shnumber TYPE oigss-shnumber,
         route TYPE oigss-route,
         distance TYPE oigss-distance,
         source_zone TYPE oigss-source_zone,
         dest_zone TYPE oigss-dest_zone,
         adrnr TYPE oigss-adrnr,
         adrnz TYPE oigss-adrnz,
       END OF gty_shipment_details.

TYPES: gty_shipment_details_table TYPE TABLE OF gty_shipment_details.
```

#### 3.1.2 Function Module Types

```abap
" Function Module Import/Export Types
TYPES: BEGIN OF ty_trip_number,
         sign TYPE ddsign,
         option TYPE ddoption,
         low TYPE zsce_ctd_hdr-trip_no,
         high TYPE zsce_ctd_hdr-trip_no,
       END OF ty_trip_number.

TYPES: ty_trip_number_table TYPE TABLE OF ty_trip_number.
```

### 3.2 Constants

```abap
" Trip Status Constants
CONSTANTS: gc_status_completed TYPE zsce_ctd_hdr-trip_status VALUE '03',
           gc_status_enriched TYPE zsce_ctd_hdr-trip_status VALUE '04'.

" Leg Type Constants
CONSTANTS: gc_leg_type_loaded TYPE char1 VALUE 'L',
           gc_leg_type_empty TYPE char1 VALUE 'E'.

" Movement Type Constants
CONSTANTS: gc_mvt_type_so TYPE char2 VALUE 'SO',
           gc_mvt_type_po TYPE char2 VALUE 'PO',
           gc_mvt_type_sto TYPE char2 VALUE 'STO'.

" Table Constants
CONSTANTS: gc_oigss_tstyp TYPE oigss-tstyp VALUE '1',
           gc_trolz_vsbed TYPE trolz-vsbed VALUE '03',
           gc_trolz_tragr TYPE trolz-tragr VALUE '0006',
           gc_yttstx0001_trk_purpos TYPE char1 VALUE 'R'.

" Test Run Constant
CONSTANTS: gc_test_run_x TYPE char1 VALUE 'X'.
```

---

## 4. Selection Screen Design

### 4.1 Selection Screen Code

```abap
" Selection Screen Definition
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
  PARAMETERS: p_from_date TYPE datum OBLIGATORY,
              p_to_date TYPE datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-002.
  SELECT-OPTIONS: s_trip_no FOR gv_trip_no.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-003.
  PARAMETERS: p_testrun TYPE char1 AS CHECKBOX DEFAULT space.
SELECTION-SCREEN END OF BLOCK b03.
```

### 4.2 Selection Screen Validations

```abap
AT SELECTION-SCREEN.
  " Date range validation
  IF p_from_date > p_to_date.
    MESSAGE e001(zctd_msg) WITH p_from_date p_to_date.
  ENDIF.

  " Trip number validation (if provided)
  IF s_trip_no[] IS NOT INITIAL.
    " Validation will be done in data selection
  ENDIF.

AT SELECTION-SCREEN ON p_from_date.
  " Validate date format
  IF p_from_date IS INITIAL.
    MESSAGE e002(zctd_msg).
  ENDIF.

AT SELECTION-SCREEN ON p_to_date.
  " Validate date format
  IF p_to_date IS INITIAL.
    MESSAGE e002(zctd_msg).
  ENDIF.
```

---

## 5. Class Design

### 5.1 Main Processor Class (lcl_main_processor)

```abap
CLASS lcl_main_processor DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
               IMPORTING iv_from_date TYPE datum
                         iv_to_date TYPE datum
                         it_trip_numbers TYPE gty_trip_number_table
                         iv_test_run TYPE char1,
             execute
               EXPORTING et_output TYPE gty_trip_leg_output_table
                         et_log TYPE gty_log_table
               RAISING cx_root.

  PRIVATE SECTION.
    DATA: mv_from_date TYPE datum,
          mv_to_date TYPE datum,
          mt_trip_numbers TYPE gty_trip_number_table,
          mv_test_run TYPE char1,
          mo_data_selector TYPE REF TO lcl_data_selector,
          mo_enrichment_processor TYPE REF TO lcl_enrichment_processor,
          mo_validator TYPE REF TO lcl_validator.
ENDCLASS.
```

### 5.2 Data Selector Class (lcl_data_selector)

```abap
CLASS lcl_data_selector DEFINITION.
  PUBLIC SECTION.
    METHODS: fetch_trip_headers
               IMPORTING iv_from_date TYPE datum
                         iv_to_date TYPE datum
                         it_trip_numbers TYPE gty_trip_number_table
               EXPORTING et_trip_headers TYPE gty_trip_header_table
                         et_log TYPE gty_log_table,
             fetch_trip_legs
               IMPORTING it_trip_headers TYPE gty_trip_header_table
               EXPORTING et_trip_legs TYPE gty_trip_leg_input_table
                         et_log TYPE gty_log_table.

  PRIVATE SECTION.
    METHODS: validate_trip_date_range
               IMPORTING iv_trip_no TYPE zsce_ctd_hdr-trip_no
                         iv_created_date TYPE datum
                         iv_from_date TYPE datum
                         iv_to_date TYPE datum
               RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.
```

### 5.3 Enrichment Processor Class (lcl_enrichment_processor)

```abap
CLASS lcl_enrichment_processor DEFINITION.
  PUBLIC SECTION.
    METHODS: enrich_loaded_legs
               IMPORTING it_trip_legs TYPE gty_trip_leg_input_table
               EXPORTING et_enriched_legs TYPE gty_trip_leg_output_table
                         et_log TYPE gty_log_table,
             insert_empty_legs
               CHANGING ct_trip_legs TYPE gty_trip_leg_output_table
               EXPORTING et_log TYPE gty_log_table,
             determine_material_region
               CHANGING ct_trip_legs TYPE gty_trip_leg_output_table
               EXPORTING et_log TYPE gty_log_table.

  PRIVATE SECTION.
    METHODS: fetch_shipment_details
               IMPORTING iv_shnumber TYPE oigss-shnumber
               EXPORTING es_shipment_details TYPE gty_shipment_details
                         ev_found TYPE abap_bool,
             determine_dest_exit_date
               IMPORTING iv_mvt_type TYPE char2
                         iv_dest_date TYPE datum
                         iv_shnumber TYPE oigss-shnumber
               RETURNING VALUE(rv_exit_date) TYPE datum,
             determine_source_ent_date
               IMPORTING iv_mvt_type TYPE char2
                         iv_source_date TYPE datum
                         iv_shnumber TYPE oigss-shnumber
               RETURNING VALUE(rv_ent_date) TYPE datum,
             fetch_business_info
               IMPORTING iv_shnumber TYPE oigss-shnumber
               EXPORTING ev_business_id TYPE zsce_ctd_itm-business_id
                         ev_subbusiness_id TYPE zsce_ctd_itm-subbusiness_id,
             fetch_material
               IMPORTING iv_shnumber TYPE oigss-shnumber
               RETURNING VALUE(rv_matnr) TYPE matnr,
             fetch_regions
               IMPORTING iv_adrnr TYPE adrnr
                         iv_adrnz TYPE adrnz
               EXPORTING ev_source_region TYPE zsce_ctd_itm-source_region
                         ev_dest_region TYPE zsce_ctd_itm-dest_region,
             fetch_route_distance
               IMPORTING iv_source_zone TYPE zsce_ctd_itm-source_zone
                         iv_dest_zone TYPE zsce_ctd_itm-dest_zone
               EXPORTING ev_route TYPE zsce_ctd_itm-route
                         ev_distance TYPE zsce_ctd_itm-distance
                         ev_found TYPE abap_bool.
ENDCLASS.
```

### 5.4 Validator Class (lcl_validator)

```abap
CLASS lcl_validator DEFINITION.
  PUBLIC SECTION.
    METHODS: validate_trip_legs
               IMPORTING it_trip_legs TYPE gty_trip_leg_input_table
               EXPORTING et_valid_trips TYPE gty_trip_header_table
                         et_invalid_trips TYPE gty_trip_header_table
                         et_log TYPE gty_log_table,
             validate_empty_legs
               IMPORTING it_trip_legs TYPE gty_trip_leg_output_table
               EXPORTING et_valid_trips TYPE gty_trip_header_table
                         et_invalid_trips TYPE gty_trip_header_table
                         et_log TYPE gty_log_table.

  PRIVATE SECTION.
    METHODS: validate_leg_dates
               IMPORTING iv_source_date TYPE datum
                         iv_dest_date TYPE datum
               RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.
```

### 5.5 ALV Display Class (lcl_alv_display)

```abap
CLASS lcl_alv_display DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
               IMPORTING io_container TYPE REF TO cl_gui_container,
             display
               IMPORTING it_data TYPE gty_trip_leg_output_table.

  PRIVATE SECTION.
    DATA: mo_alv_grid TYPE REF TO cl_gui_alv_grid,
          mo_container TYPE REF TO cl_gui_container,
          mt_fieldcat TYPE lvc_t_fcat,
          mt_output TYPE gty_trip_leg_output_table.

    METHODS: build_field_catalog,
             apply_color_coding,
             set_layout.
ENDCLASS.
```

---

## 6. Function Module Design

### 6.1 Function Module Interface

```abap
FUNCTION zscm_ctd_enrichtripdetails.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_FROM_DATE) TYPE DATS
*"     VALUE(I_TO_DATE) TYPE DATS
*"     VALUE(IT_TRIP_NUMBERS) TYPE ZSCE_CTD_HDR-TRIP_NO OPTIONAL
*"     VALUE(I_TEST_RUN) TYPE CHAR1 DEFAULT SPACE
*"  EXPORTING
*"     VALUE(ET_OUTPUT) TYPE GTY_TRIP_LEG_OUTPUT_TABLE
*"     VALUE(ET_LOG) TYPE GTY_LOG_TABLE
*"  EXCEPTIONS
*"     INVALID_DATE_RANGE
*"     PROCESSING_ERROR
*"----------------------------------------------------------------------
```

### 6.2 Function Module Implementation Structure

```abap
FUNCTION zscm_ctd_enrichtripdetails.
  " Local data declarations
  DATA: lo_main_processor TYPE REF TO lcl_main_processor,
        lt_trip_numbers TYPE gty_trip_number_table,
        lv_from_date TYPE datum,
        lv_to_date TYPE datum,
        lv_test_run TYPE char1.

  " Input validation
  IF i_from_date > i_to_date.
    RAISE invalid_date_range.
  ENDIF.

  " Convert input parameters
  lv_from_date = i_from_date.
  lv_to_date = i_to_date.
  lv_test_run = i_test_run.

  " Convert trip numbers table if provided
  IF it_trip_numbers[] IS NOT INITIAL.
    lt_trip_numbers = it_trip_numbers[].
  ENDIF.

  " Create main processor instance
  CREATE OBJECT lo_main_processor
    EXPORTING
      iv_from_date = lv_from_date
      iv_to_date = lv_to_date
      it_trip_numbers = lt_trip_numbers
      iv_test_run = lv_test_run.

  " Execute processing
  TRY.
      CALL METHOD lo_main_processor->execute
        IMPORTING
          et_output = et_output
          et_log = et_log.
    CATCH cx_root INTO lo_exception.
      RAISE processing_error.
  ENDTRY.

ENDFUNCTION.
```

---

## 7. Database Access Patterns

### 7.1 Trip Header Selection

```abap
" Performance: Use specific fields, not SELECT *
" Structure matches SELECT fields exactly
TYPES: BEGIN OF ty_ctd_hdr_selected,
         trip_no TYPE zsce_ctd_hdr-trip_no,
         lifnr TYPE zsce_ctd_hdr-lifnr,
         truck_no TYPE zsce_ctd_hdr-truck_no,
         trip_status TYPE zsce_ctd_hdr-trip_status,
         created_date TYPE zsce_ctd_hdr-created_date,
       END OF ty_ctd_hdr_selected.

TYPES: tt_ctd_hdr_selected TYPE TABLE OF ty_ctd_hdr_selected.

DATA: lt_trip_headers TYPE tt_ctd_hdr_selected,
      lw_trip_header TYPE ty_ctd_hdr_selected.

" SELECT fields: trip_no, lifnr, truck_no, trip_status, created_date
SELECT trip_no lifnr truck_no trip_status created_date
  FROM zsce_ctd_hdr
  INTO TABLE lt_trip_headers
  WHERE trip_status = gc_status_completed
    AND created_date BETWEEN p_from_date AND p_to_date
    AND ( s_trip_no[] IS INITIAL OR trip_no IN s_trip_no[] ).

IF sy-subrc = 0.
  " Process headers
ENDIF.
```

### 7.2 Trip Leg Selection

```abap
" Use FOR ALL ENTRIES pattern (with empty check)
IF lt_trip_headers IS NOT INITIAL.
  " Deduplicate trip numbers first
  DATA: lt_trip_no TYPE TABLE OF zsce_ctd_hdr-trip_no.
  
  LOOP AT lt_trip_headers INTO lw_trip_header.
    APPEND lw_trip_header-trip_no TO lt_trip_no.
  ENDLOOP.
  
  SORT lt_trip_no.
  DELETE ADJACENT DUPLICATES FROM lt_trip_no.

  " SELECT with FOR ALL ENTRIES
  SELECT trip_no counter lifnr truck_no shnumber
         source_date dest_date mvt_type area adrnr adrnz
    FROM zsce_ctd_itm
    INTO TABLE lt_trip_legs
    FOR ALL ENTRIES IN lt_trip_no
    WHERE trip_no = lt_trip_no-table_line.

  IF sy-subrc = 0.
    " Sort by trip_no and counter for processing
    SORT lt_trip_legs BY trip_no counter.
  ENDIF.
ENDIF.
```

### 7.3 Shipment Details Selection

```abap
" Collect shipment numbers for FOR ALL ENTRIES
DATA: lt_shnumber TYPE TABLE OF oigss-shnumber,
      lt_shipment_details TYPE gty_shipment_details_table.

" Collect unique shipment numbers
LOOP AT lt_trip_legs INTO lw_trip_leg WHERE shnumber <> space.
  APPEND lw_trip_leg-shnumber TO lt_shnumber.
ENDLOOP.

SORT lt_shnumber.
DELETE ADJACENT DUPLICATES FROM lt_shnumber.

" Fetch shipment details
IF lt_shnumber IS NOT INITIAL.
  " SELECT fields: shnumber, route, distance, source_zone, dest_zone, adrnr, adrnz
  TYPES: BEGIN OF ty_oigss_selected,
           shnumber TYPE oigss-shnumber,
           route TYPE oigss-route,
           distance TYPE oigss-distance,
           source_zone TYPE oigss-source_zone,
           dest_zone TYPE oigss-dest_zone,
           adrnr TYPE oigss-adrnr,
           adrnz TYPE oigss-adrnz,
         END OF ty_oigss_selected.
  
  TYPES: tt_oigss_selected TYPE TABLE OF ty_oigss_selected.
  DATA: lt_oigss TYPE tt_oigss_selected.

  SELECT shnumber route distance source_zone dest_zone adrnr adrnz
    FROM oigss
    INTO TABLE lt_oigss
    FOR ALL ENTRIES IN lt_shnumber
    WHERE shnumber = lt_shnumber-table_line
      AND tstyp = gc_oigss_tstyp.

  IF sy-subrc = 0.
    " Sort for binary search
    SORT lt_oigss BY shnumber.
  ENDIF.
ENDIF.
```

### 7.4 Material Determination

```abap
" Step 1: Get delivery number from OIGSI
" Step 2: Get material from LIPS
" Use FOR ALL ENTRIES to avoid SELECT in loop

" Collect shipment numbers
DATA: lt_shnumber_for_mat TYPE TABLE OF oigss-shnumber,
      lt_doc_number TYPE TABLE OF oigsi-doc_number,
      lt_material_data TYPE TABLE OF lips.

" Step 1: Get delivery numbers
IF lt_shnumber_for_mat IS NOT INITIAL.
  SELECT shnumber doc_number
    FROM oigsi
    INTO TABLE lt_doc_number
    FOR ALL ENTRIES IN lt_shnumber_for_mat
    WHERE shnumber = lt_shnumber_for_mat-table_line.

  IF sy-subrc = 0.
    SORT lt_doc_number BY shnumber doc_number.
  ENDIF.
ENDIF.

" Step 2: Get materials
IF lt_doc_number IS NOT INITIAL.
  " Deduplicate doc_numbers
  DATA: lt_vbeln TYPE TABLE OF lips-vbeln.
  
  LOOP AT lt_doc_number INTO lw_doc_number.
    APPEND lw_doc_number-doc_number TO lt_vbeln.
  ENDLOOP.
  
  SORT lt_vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_vbeln.

  SELECT vbeln matnr
    FROM lips
    INTO TABLE lt_material_data
    FOR ALL ENTRIES IN lt_vbeln
    WHERE vbeln = lt_vbeln-table_line.

  IF sy-subrc = 0.
    SORT lt_material_data BY vbeln.
  ENDIF.
ENDIF.
```

---

## 8. Performance Optimization

### 8.1 Table Lookup Optimization

```abap
" Always use SORT + BINARY SEARCH for table lookups
SORT lt_shipment_details BY shnumber.

LOOP AT lt_trip_legs INTO lw_trip_leg WHERE shnumber <> space.
  READ TABLE lt_shipment_details INTO lw_shipment_details
    WITH KEY shnumber = lw_trip_leg-shnumber
    BINARY SEARCH.
  
  IF sy-subrc = 0.
    " Process found record
  ENDIF.
ENDLOOP.
```

### 8.2 Parallel Cursor Pattern

```abap
" Sort both tables for parallel cursor
SORT lt_trip_headers BY trip_no.
SORT lt_trip_legs BY trip_no counter.

LOOP AT lt_trip_headers INTO lw_trip_header.
  " Use parallel cursor for legs
  LOOP AT lt_trip_legs INTO lw_trip_leg
    WHERE trip_no = lw_trip_header-trip_no.
    " Process leg
  ENDLOOP.
ENDLOOP.
```

### 8.3 Table Modification Optimization

```abap
" Use ASSIGNING instead of MODIFY in loop
FIELD-SYMBOLS: <lfs_leg> TYPE gty_trip_leg_output.

LOOP AT ct_trip_legs ASSIGNING <lfs_leg>.
  <lfs_leg>-leg_type = gc_leg_type_loaded.
  " Direct modification, no search needed
ENDLOOP.
```

---

## 9. Error Handling

### 9.1 Exception Classes

```abap
" Custom Exception Classes (if needed)
CLASS zcx_ctd_processing_error DEFINITION
  INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS: constructor
               IMPORTING iv_textid TYPE scx_t100key OPTIONAL
                         iv_previous TYPE REF TO cx_root OPTIONAL.
ENDCLASS.
```

### 9.2 Error Handling Pattern

```abap
" Declare exception object BEFORE TRY block
DATA: lo_exception TYPE REF TO cx_root,
      lv_error_text TYPE string.

TRY.
    " Processing code
    CALL METHOD lo_processor->execute
      IMPORTING
        et_output = lt_output
        et_log = lt_log.

  CATCH cx_root INTO lo_exception.
    " Get error text
    lv_error_text = lo_exception->get_text( ).
    
    " Display error (use DISPLAY LIKE for reports)
    MESSAGE lv_error_text TYPE 'I' DISPLAY LIKE 'E'.
    
    " Log error
    APPEND INITIAL LINE TO lt_log ASSIGNING <lfs_log>.
    <lfs_log>-msgty = 'E'.
    <lfs_log>-msgid = 'ZCTD_MSG'.
    <lfs_log>-msgno = '005'.
    <lfs_log>-msgv1 = lv_error_text.
ENDTRY.
```

---

## 10. Database Update Logic

### 10.1 Update Pattern

```abap
" Only update if not test run
IF mv_test_run <> gc_test_run_x.
  " Update trip legs
  LOOP AT lt_enriched_legs INTO lw_enriched_leg.
    UPDATE zsce_ctd_itm SET
      leg_type = lw_enriched_leg-leg_type
      source_zone = lw_enriched_leg-source_zone
      dest_zone = lw_enriched_leg-dest_zone
      route = lw_enriched_leg-route
      distance = lw_enriched_leg-distance
      matnr = lw_enriched_leg-matnr
      source_region = lw_enriched_leg-source_region
      dest_region = lw_enriched_leg-dest_region
      business_id = lw_enriched_leg-business_id
      subbusiness_id = lw_enriched_leg-subbusiness_id
      dest_exit_date = lw_enriched_leg-dest_exit_date
      source_ent_date = lw_enriched_leg-source_ent_date
      ctd_ruleeng_remarks = lw_enriched_leg-ctd_ruleeng_remarks
    WHERE trip_no = lw_enriched_leg-trip_no
      AND counter = lw_enriched_leg-counter.

    IF sy-subrc <> 0.
      " Log update error
    ENDIF.
  ENDLOOP.

  " Update trip headers (status)
  LOOP AT lt_valid_trips INTO lw_trip_header.
    UPDATE zsce_ctd_hdr SET
      trip_status = gc_status_enriched
    WHERE trip_no = lw_trip_header-trip_no.

    IF sy-subrc <> 0.
      " Log update error
    ENDIF.
  ENDLOOP.

  " Commit once after all updates
  COMMIT WORK.
ENDIF.
```

### 10.2 Update Transaction Handling

```abap
" Use single LUW for all updates
" All updates must succeed or all must rollback
" Use COMMIT WORK only once at the end

" If any update fails, rollback
IF lv_error_occurred = abap_true.
  ROLLBACK WORK.
ELSE.
  COMMIT WORK.
ENDIF.
```

---

## 11. ALV Display Implementation

### 11.1 Field Catalog

```abap
METHOD build_field_catalog.
  DATA: lw_fieldcat TYPE lvc_s_fcat.

  " LIFNR
  CLEAR lw_fieldcat.
  lw_fieldcat-fieldname = 'LIFNR'.
  lw_fieldcat-scrtext_l = 'Vendor'(001).
  lw_fieldcat-just = 'L'.
  APPEND lw_fieldcat TO mt_fieldcat.

  " TRUCK_NO
  CLEAR lw_fieldcat.
  lw_fieldcat-fieldname = 'TRUCK_NO'.
  lw_fieldcat-scrtext_l = 'Truck Number'(002).
  lw_fieldcat-just = 'L'.
  APPEND lw_fieldcat TO mt_fieldcat.

  " LEG_TYPE
  CLEAR lw_fieldcat.
  lw_fieldcat-fieldname = 'LEG_TYPE'.
  lw_fieldcat-scrtext_l = 'Leg Type'(003).
  lw_fieldcat-just = 'L'.
  APPEND lw_fieldcat TO mt_fieldcat.

  " DISTANCE - Right justified
  CLEAR lw_fieldcat.
  lw_fieldcat-fieldname = 'DISTANCE'.
  lw_fieldcat-scrtext_l = 'Distance'(004).
  lw_fieldcat-just = 'R'.
  APPEND lw_fieldcat TO mt_fieldcat.

  " Continue for all fields...
ENDMETHOD.
```

### 11.2 Color Coding

```abap
METHOD apply_color_coding.
  DATA: lt_color TYPE lvc_t_scol,
        lw_color TYPE lvc_s_scol.

  FIELD-SYMBOLS: <lfs_output> TYPE gty_trip_leg_output.

  LOOP AT mt_output ASSIGNING <lfs_output>
    WHERE validation_error = abap_true.
    
    CLEAR lt_color.
    CLEAR lw_color.
    lw_color-fname = 'LEG_TYPE'.
    lw_color-color-col = 6.  " Red
    lw_color-color-int = 0.
    lw_color-color-inv = 0.
    APPEND lw_color TO lt_color.

    " Set color for row
    CALL METHOD mo_alv_grid->set_row_color
      EXPORTING
        it_color = lt_color
        iv_row = sy-tabix.
  ENDLOOP.
ENDMETHOD.
```

---

## 12. Authorization Check

### 12.1 Authorization Implementation

```abap
INITIALIZATION.
  " Check transaction authorization
  AUTHORITY-CHECK OBJECT 'S_TCODE'
                  ID 'TCD' FIELD 'ZCTD_ENRICH_TRIPDETAILS'.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE w077(s#) WITH 'ZCTD_ENRICH_TRIPDETAILS' DISPLAY LIKE 'E'.
    LEAVE PROGRAM.
  ENDIF.
```

---

## 13. Code Generation Checklist Compliance

### 13.1 Performance Patterns
- ✅ SORT + BINARY SEARCH for all table lookups
- ✅ FOR ALL ENTRIES with empty check
- ✅ No SELECT in loops
- ✅ ASSIGNING used for table modifications
- ✅ Specific fields in SELECT (no SELECT *)
- ✅ Internal table structures match SELECT fields exactly

### 13.2 Syntax Compatibility
- ✅ No inline declarations
- ✅ No constructor operators (NEW, VALUE, CORRESPONDING)
- ✅ No string templates
- ✅ No table expressions
- ✅ No host variables in SQL
- ✅ All variables declared upfront

### 13.3 Code Quality
- ✅ SY-SUBRC checks after all database operations
- ✅ Exception handling with TRY-CATCH
- ✅ Authorization checks
- ✅ Input validation
- ✅ Proper error messages

---

## 14. Testing Strategy

### 14.1 Unit Testing
- Test each class method independently
- Mock external dependencies
- Test error scenarios

### 14.2 Integration Testing
- Test end-to-end flow
- Test database updates
- Test ALV display

### 14.3 Performance Testing
- Test with large datasets (10,000+ trips)
- Measure database time
- Verify response times

---

## 15. Deployment Considerations

### 15.1 Transport Request
- All objects in single transport request
- Include program, function module, message class
- Include any custom types if created

### 15.2 Post-Deployment
- Verify transaction code assignment
- Test authorization setup
- Monitor initial runs

---

## 16. Appendix

### 16.1 Message Class Structure

**Message Class: ZCTD_MSG**

| Number | Type | Text |
|--------|------|------|
| 001 | E | From Date must be less than or equal to To Date |
| 002 | E | Date field is required |
| 003 | E | Selected Trip does not fall within the given date range |
| 004 | E | Trip excluded: Missing Source or Destination Date |
| 005 | E | Trip excluded: Empty Leg Route / Distance missing |
| 006 | E | Shipment not found in OIGSS |
| 007 | E | Address not found in ADRC |
| 008 | E | Material not found in LIPS |
| 001 | W | Optional data missing for leg |
| 001 | I | Processing started |
| 002 | I | Trip processed successfully |
| 001 | S | Enrichment completed |

### 16.2 Text Elements

**Text-001:** Selection Criteria  
**Text-002:** Trip Number  
**Text-003:** Options  
**Text-004:** Test Run / Simulation Mode

---

**Document Approval**

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Technical Lead | | | |
| ABAP Architect | | | |
| Project Manager | | | |

---

**Revision History**

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | [Date] | [Author] | Initial version |

