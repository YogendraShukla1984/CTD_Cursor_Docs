*&---------------------------------------------------------------------*
*& Include  ZSCM_CTD_GETTRIPHDRTOP (Function Group: ZSCM_CTD)
*&---------------------------------------------------------------------*
*& Purpose: Global Type Definitions and Constants for Function Module
*&          ZSCM_CTD_GETTRIPHDR
*&
*& This include contains:
*& - Type definitions for trip header structure and table
*& - Constants for user types and trip statuses
*&
*& Author: [Author Name]
*& Creation Date: [Date]
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*&---------------------------------------------------------------------*
*& Type Definitions
*&---------------------------------------------------------------------*

" Trip Header Structure (matches SELECT statement fields exactly)
" SELECT fields: trip_no, lifnr, truck_no, trip_status, created_date,
"                created_time, created_by, business, subbusiness
TYPES: BEGIN OF gty_trip_hdr,
         trip_no TYPE zsce_ctd_hdr-trip_no,
         lifnr TYPE zsce_ctd_hdr-lifnr,
         truck_no TYPE zsce_ctd_hdr-truck_no,
         trip_status TYPE zsce_ctd_hdr-trip_status,
         created_date TYPE zsce_ctd_hdr-created_date,
         created_time TYPE zsce_ctd_hdr-created_time,
         created_by TYPE zsce_ctd_hdr-created_by,
         business TYPE zsce_ctd_hdr-business,
         subbusiness TYPE zsce_ctd_hdr-subbusiness,
       END OF gty_trip_hdr.

" Trip Header Table Type (specific type, not generic)
TYPES: gty_trip_hdr_table TYPE TABLE OF gty_trip_hdr.

*&---------------------------------------------------------------------*
*& Constants
*&---------------------------------------------------------------------*

" User Type Constants
CONSTANTS: gc_user_type_vendor TYPE char1 VALUE 'V',      " Vendor user
           gc_user_type_ril TYPE char1 VALUE 'R'.          " RIL Operations user

" Trip Status Constants
CONSTANTS: gc_status_pending TYPE char2 VALUE '04',        " Pending for Transporter Confirmation
           gc_status_rejected TYPE char2 VALUE '06'.      " Rejected by RIL Operations

" END: Cursor Generated Code

