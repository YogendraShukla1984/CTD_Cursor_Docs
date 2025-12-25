
# FS – Transporter Trip Details Confirm (CTD)

## FM
**Z_SCM_CTD_TRAN_TRIPCONFIRM**

## 1. Purpose
This functionality enables the Transporter (Vendor) to:
- View complete Trip Header and Leg-level details
- Update Source Date and Destination Date for **Empty Legs only**
- Enter Vendor Remarks
- Confirm the Trip

Post confirmation:
- Item-level details updated in `ZSCE_CTD_ITM`
- Trip status updated to **05 – Trip Details Confirmed by Vendor** in `ZSCE_CTD_HDR`
- Trip becomes visible to RIL Operations

## 2. Pre-requisites
- Trip exists in `ZSCE_CTD_HDR`
- Trip status = **04 – Trip Details Updated / Pending for Transporter Confirmation**

## 3. Input Parameters
| Parameter | Type | Mandatory | Description |
|---------|------|-----------|-------------|
| P_TRIP_NO | CHAR14 | Yes | Trip Number |
| IT_LEG_UPDATE | ZTT_CTD_LEG_UPD | Yes | Leg-level update table |

## 4. IT_LEG_UPDATE Structure
Contains only **Empty Leg** records.

| Field | Description |
|------|-------------|
| TRIP_NO | Trip Number |
| COUNTER | Leg Sequence Number |
| LEG_TYPE | EMPTY |
| SOURCE_DATE | Updated Source Date |
| DESTINATION_DATE | Updated Destination Date |
| VENDOR_REMARKS | Vendor remarks |

## 5. Validations
### Header Level
- Trip missing → Error
- Unauthorized vendor → Error
- Status ≠ 04 → Error

### Item Level
- Loaded leg update → Error
- Source Date > Destination Date → Error

## 6. Processing Logic
1. Authorization & eligibility check
2. Update Empty Leg details in `ZSCE_CTD_ITM`
3. Update Vendor Remarks
4. Update Trip Header to status **05**
5. Commit Work

## 7. Output
| Parameter | Type | Description |
|---------|------|-------------|
| EV_STATUS | CHAR1 | S / E |
| EV_MESSAGE | STRING | Message |
| ET_RETURN | BAPIRET2_T | Details |

## 8. Success Message
> Trip & Empty Leg details confirmed successfully.

## 9. Tables Impacted
### ZSCE_CTD_HDR
- TRIP_STATUS → 05
- CONFIRM_DATE / TIME / USER

### ZSCE_CTD_ITM
- SOURCE_DATE (Empty Legs)
- DESTINATION_DATE (Empty Legs)

## 10. Notes
- One-time confirmation
- Rollback required for re-edit
