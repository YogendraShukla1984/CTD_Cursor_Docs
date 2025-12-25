*&---------------------------------------------------------------------*
*& Include  Z_SCM_CTD_TRAN_TRIPCONFIRMTOP (Function Group: ZSCM_CTD)
*&---------------------------------------------------------------------*
*& Purpose: Global Type Definitions and Constants for Function Module
*&          Z_SCM_CTD_TRAN_TRIPCONFIRM
*&
*& This include contains:
*& - Type definitions for leg update structure and table
*& - Type definition for trip header confirmation structure
*& - Type definition for leg validation structure
*& - Constants for leg types, trip status, and return status
*&
*& Author: [Author Name]
*& Creation Date: [Date]
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*&---------------------------------------------------------------------*
*& Type Definitions
*&---------------------------------------------------------------------*

" Leg Update Structure (for IT_LEG_UPDATE parameter)
TYPES: BEGIN OF gty_leg_update,
         trip_no TYPE zsce_ctd_itm-trip_no,
         counter TYPE zsce_ctd_itm-counter,
         leg_type TYPE char1,
         source_date TYPE zsce_ctd_itm-source_date,
         destination_date TYPE zsce_ctd_itm-dest_date,
         vendor_remarks TYPE string,
       END OF gty_leg_update.

" Leg Update Table Type (specific type, not generic)
TYPES: gty_leg_update_table TYPE TABLE OF gty_leg_update.

" Trip Header Structure (for validation and confirmation)
TYPES: BEGIN OF gty_trip_hdr_confirm,
         trip_no TYPE zsce_ctd_hdr-trip_no,
         lifnr TYPE zsce_ctd_hdr-lifnr,
         trip_status TYPE zsce_ctd_hdr-trip_status,
       END OF gty_trip_hdr_confirm.

" Leg Structure (for validation)
TYPES: BEGIN OF gty_leg_validate,
         trip_no TYPE zsce_ctd_itm-trip_no,
         counter TYPE zsce_ctd_itm-counter,
         leg_type TYPE char1,
       END OF gty_leg_validate.

" Leg Validation Table Type
TYPES: gty_leg_validate_table TYPE TABLE OF gty_leg_validate.

*&---------------------------------------------------------------------*
*& Constants
*&---------------------------------------------------------------------*

" Leg Type Constants
CONSTANTS: gc_leg_type_empty TYPE char1 VALUE 'E',      " Empty leg
           gc_leg_type_loaded TYPE char1 VALUE 'L'.      " Loaded leg

" Trip Status Constants
CONSTANTS: gc_status_pending TYPE char2 VALUE '04',     " Pending for Transporter Confirmation
           gc_status_confirmed TYPE char2 VALUE '05'.   " Trip Details Confirmed by Vendor

" Status Constants (for EV_STATUS)
CONSTANTS: gc_status_success TYPE char1 VALUE 'S',     " Success
           gc_status_error TYPE char1 VALUE 'E'.        " Error

" END: Cursor Generated Code

