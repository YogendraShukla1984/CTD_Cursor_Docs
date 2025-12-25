# Functional Specification – Fetch CTD Trip Header Details (Reusable FM)

FM Name (Proposed): ZSCM_CTD_GETTRIPHDR



## 1. Purpose

This Function Module retrieves CTD Trip Header details from SAP based on user search criteria.
The FM is designed to be reusable and shall support both:

Transporter / Vendor users (external portal), and

RIL Operations users (internal users),

by applying role-based data filtering and authorization logic within the same FM.

The output of this FM shall be consumed by:

Transporter Portal UI, and

RIL Operations CTD monitoring screens.



## 2. Input Parameters













## 3. Validation Rules



## 4. Processing Logic

Step 1 – Determine User Context

Based on input parameter P_USER_TYPE, the FM shall determine the authorization logic to be applied:

P_USER_TYPE = ‘V’ → Transporter / Vendor

P_USER_TYPE = ‘R’ → RIL Operations



Step 2 – Fetch Trip Header Records

Select records from ZSCE_CTD_HDR where:

CREATED_DATE between P_FROM_DATE and P_TO_DATE

TRIP_STATUS IN:

04 – Trip Details Updated / Pending for Transporter Trip Confirmation

06 – Trip Details Rejected by RIL Operations

If P_TRUCK_NO is provided → apply TRUCK_NO = P_TRUCK_NO



Step 3 – Apply Role-Based Data Restriction

a) Transporter / Vendor User (P_USER_TYPE = ‘V’)

Restrict records to trips belonging to the logged-in vendor:

ZSCE_CTD_HDR-LIFNR = P_USER_ID

b) RIL Operations User (P_USER_TYPE = ‘R’)

Restrict records based on Business and Sub-Business authorization:

ZSCE_CTD_HDR-BUSINESS     IN Authorized Business

AND

ZSCE_CTD_HDR-SUBBUSINESS IN Authorized Sub-Business

(Authorization values to be derived from SAP roles or a custom authorization table.)



Step 4 – Populate Output Table

For each valid record, populate the output structure ET_TRIP_HDR.



## 5. Output Structure

📌 Header Level Table – ET_TRIP_HDR



## 6. Error Handling



## 7. Performance & Reusability Considerations

This FM shall fetch header data only.

Item/Leg data shall be fetched using a separate FM on demand.

Same FM shall be reused across:

Transporter Portal

RIL Operations screens

Authorization checks shall be performed within the FM to ensure data security.

Top of Form



Bottom of Form

