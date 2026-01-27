#ABAP #ABAP-New-Syntax #Filter 
# Purpose
- Filtering
# Reference
- N/A
# Implementation
```ABAP
*&---------------------------------------------------------------------*  
*& Report Z47825001VALUEFOR  
*&---------------------------------------------------------------------*  
*&  
*&---------------------------------------------------------------------*  
REPORT Z47825001VALUEFOR.  
  
TYPES ty_t_sflight TYPE STANDARD TABLE OF sflight WITH DEFAULT KEY.  
  
SELECT *  
  FROM sflight  
  INTO TABLE @DATA(lt_flights).  
  
" Modern alternative to FILTER - works on any table  
DATA(lt_result) = VALUE ty_t_sflight(  
                    FOR ls_flight IN lt_flights  
                    WHERE ( carrid = 'DL' )  
                    ( ls_flight )  
                  ).  
  
" BEFORE  
CL_DEMO_OUTPUT=>display(  
  data    = lt_flights  
  name    = 'BEFORE'  
).  
  
" AFTER  
CL_DEMO_OUTPUT=>display(  
  data    = lt_result  
  name    = 'AFTER'  
).
```
# Result
![[Pasted image 20260127110936.png]]
![[Pasted image 20260127110942.png]]
