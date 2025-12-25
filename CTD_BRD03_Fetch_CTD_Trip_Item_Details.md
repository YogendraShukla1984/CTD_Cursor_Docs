
# Fetch CTD Trip Item / Leg Details

## FM
**ZSCM_CTD_GETTRIPITM**

## 1. Purpose
Reusable FM to retrieve item/leg-level CTD Trip details for:
- Transporter / Vendor users
- RIL Operations users

## 2. Input Parameters
| Parameter | Type | Mandatory | Description |
|---------|------|-----------|-------------|
| P_TRIP_NO | CHAR14 | Yes | Trip Number |
| P_USER_TYPE | CHAR1 | Yes | V = Vendor, R = RIL |
| P_USER_ID | CHAR12 | Yes | Vendor No / SAP User ID |

## 3. Validations
- Trip missing → Error
- Trip invalid → Error
- Unauthorized user → Error
- No items → Empty table

## 4. Processing Logic
1. Authorization based on User Type
2. Fetch all legs from `ZSCE_CTD_ITM`
3. Sort by `COUNTER`

## 5. Output Structure – ET_TRIP_ITM
Includes:
- Transporter details
- Trip & Leg sequence
- Source & Destination data
- Dates, distance, route
- Vendor & RIL remarks

## 6. Error Handling
- Unauthorized → Error message
- No records → Empty table

## 7. Reusability Notes
- Single FM for Vendor & RIL
- Header data via separate FM
- Optimized for on-demand fetch
