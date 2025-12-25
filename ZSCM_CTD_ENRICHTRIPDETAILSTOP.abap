*&---------------------------------------------------------------------*
*& Include  ZSCM_CTD_ENRICHTRIPDETAILSTOP
*&---------------------------------------------------------------------*
*& Purpose: Global Data Declarations, Type Definitions, Constants
*&---------------------------------------------------------------------*

" Global variable for select-option
DATA: gv_trip_no TYPE zsce_ctd_hdr-trip_no.

*&---------------------------------------------------------------------*
*& Type Definitions
*&---------------------------------------------------------------------*

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
         leg_type TYPE char1,
         shnumber TYPE zsce_ctd_itm-shnumber,
         source_date TYPE zsce_ctd_itm-source_date,
         dest_date TYPE zsce_ctd_itm-dest_date,
         area TYPE zsce_ctd_itm-area,
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

" Trip Number Range Structure
TYPES: BEGIN OF gty_trip_number,
         sign TYPE ddsign,
         option TYPE ddoption,
         low TYPE zsce_ctd_hdr-trip_no,
         high TYPE zsce_ctd_hdr-trip_no,
       END OF gty_trip_number.

TYPES: gty_trip_number_table TYPE TABLE OF gty_trip_number.

" CTD Header Selected Fields (matches SELECT statement)
TYPES: BEGIN OF ty_ctd_hdr_selected,
         trip_no TYPE zsce_ctd_hdr-trip_no,
         lifnr TYPE zsce_ctd_hdr-lifnr,
         truck_no TYPE zsce_ctd_hdr-truck_no,
         trip_status TYPE zsce_ctd_hdr-trip_status,
         created_date TYPE zsce_ctd_hdr-created_date,
       END OF ty_ctd_hdr_selected.

TYPES: tt_ctd_hdr_selected TYPE TABLE OF ty_ctd_hdr_selected.

" OIGSS Selected Fields (matches SELECT statement)
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

" OIGSI Selected Fields (matches SELECT statement)
TYPES: BEGIN OF ty_oigsi_selected,
         shnumber TYPE oigsi-shnumber,
         doc_number TYPE oigsi-doc_number,
       END OF ty_oigsi_selected.

TYPES: tt_oigsi_selected TYPE TABLE OF ty_oigsi_selected.

" LIPS Selected Fields (matches SELECT statement)
TYPES: BEGIN OF ty_lips_selected,
         vbeln TYPE lips-vbeln,
         matnr TYPE lips-matnr,
       END OF ty_lips_selected.

TYPES: tt_lips_selected TYPE TABLE OF ty_lips_selected.

" ADRC Selected Fields (matches SELECT statement)
TYPES: BEGIN OF ty_adrc_selected,
         addrnumber TYPE adrc-addrnumber,
         region TYPE adrc-region,
       END OF ty_adrc_selected.

TYPES: tt_adrc_selected TYPE TABLE OF ty_adrc_selected.

" YTTSTX0001 Selected Fields (matches SELECT statement)
TYPES: BEGIN OF ty_yttstx0001_selected,
         shnumber TYPE yttstx0001-shnumber,
         trk_purpos TYPE yttstx0001-trk_purpos,
         mg_exit_dt TYPE yttstx0001-mg_exit_dt,
         pp_entr_dt TYPE yttstx0001-pp_entr_dt,
       END OF ty_yttstx0001_selected.

TYPES: tt_yttstx0001_selected TYPE TABLE OF ty_yttstx0001_selected.

" YTTSTX0002 Selected Fields (matches SELECT statement)
TYPES: BEGIN OF ty_yttstx0002_selected,
         shnumber TYPE yttstx0002-shnumber,
         report_no TYPE yttstx0002-report_no,
       END OF ty_yttstx0002_selected.

TYPES: tt_yttstx0002_selected TYPE TABLE OF ty_yttstx0002_selected.

" TROLZ Selected Fields (matches SELECT statement)
TYPES: BEGIN OF ty_trolz_selected,
         vsbed TYPE trolz-vsbed,
         tragr TYPE trolz-tragr,
         route TYPE trolz-route,
       END OF ty_trolz_selected.

TYPES: tt_trolz_selected TYPE TABLE OF ty_trolz_selected.

" TVRO Selected Fields (matches SELECT statement)
TYPES: BEGIN OF ty_tvro_selected,
         route TYPE tvro-route,
         distance TYPE tvro-distance,
       END OF ty_tvro_selected.

TYPES: tt_tvro_selected TYPE TABLE OF ty_tvro_selected.

*&---------------------------------------------------------------------*
*& Constants
*&---------------------------------------------------------------------*

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

