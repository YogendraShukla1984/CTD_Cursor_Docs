*&---------------------------------------------------------------------*
*& Include  ZSCM_CTD_GETTRIPITMTOP (Function Group: ZSCM_CTD)
*&---------------------------------------------------------------------*
*& Purpose: Global Type Definitions and Constants for Function Module
*&          ZSCM_CTD_GETTRIPITM
*&
*& This include contains:
*& - Type definitions for trip item/leg structure and table
*& - Type definition for trip header authorization structure
*& - Constants for user types
*&
*& Author: [Author Name]
*& Creation Date: [Date]
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*&---------------------------------------------------------------------*
*& Type Definitions
*&---------------------------------------------------------------------*

" Trip Item/Leg Structure (matches SELECT statement fields exactly)
" SELECT fields: trip_no, counter, lifnr, truck_no, shnumber,
"                source_date, dest_date, mvt_type, area, adrnr, adrnz,
"                source_zone, dest_zone, route, distance, matnr,
"                source_region, dest_region, business_id, subbusiness_id,
"                dest_exit_date, source_ent_date, ctd_ruleeng_remarks
TYPES: BEGIN OF gty_trip_itm,
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
         ctd_ruleeng_remarks TYPE zsce_ctd_itm-ctd_ruleeng_remarks,
       END OF gty_trip_itm.

" Trip Item/Leg Table Type (specific type, not generic)
TYPES: gty_trip_itm_table TYPE TABLE OF gty_trip_itm.

" Trip Header Structure (for authorization check)
TYPES: BEGIN OF gty_trip_hdr_auth,
         trip_no TYPE zsce_ctd_hdr-trip_no,
         lifnr TYPE zsce_ctd_hdr-lifnr,
         business TYPE zsce_ctd_hdr-business,
         subbusiness TYPE zsce_ctd_hdr-subbusiness,
       END OF gty_trip_hdr_auth.

*&---------------------------------------------------------------------*
*& Constants
*&---------------------------------------------------------------------*

" User Type Constants
CONSTANTS: gc_user_type_vendor TYPE char1 VALUE 'V',      " Vendor user
           gc_user_type_ril TYPE char1 VALUE 'R'.          " RIL Operations user

" END: Cursor Generated Code

