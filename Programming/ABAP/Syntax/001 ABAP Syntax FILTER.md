#ABAP #ABAP-New-Syntax #Filter 
# Purpose
- Filtering
# Reference
- N/A
# Implementation
## 1. Basic Syntax
`FILTER #( itab [ USING KEY key ] WHERE condition )`
## 2. Example 1
```ABAP
*&---------------------------------------------------------------------*  
*& Report Z47825001FILTER  
*&---------------------------------------------------------------------*  
*&  
*&---------------------------------------------------------------------*  
REPORT Z47825001FILTER.  
  
DATA: lt_flights TYPE SORTED TABLE OF sflight WITH NON-UNIQUE KEY carrid.  
  
SELECT  
    *  
  FROM  
    SFLIGHT  
  INTO TABLE  
    @lt_flights.  
  
DATA(lt_result) = FILTER #( lt_flights WHERE CARRID = 'AA ' ).  
  
"BEFORE  
CL_DEMO_OUTPUT=>DISPLAY( lt_flights ).  
  
"AFTER  
CL_DEMO_OUTPUT=>DISPLAY( lt_result ).
```
# Result
![[Pasted image 20260127110054.png]]
![[Pasted image 20260127110101.png]]

