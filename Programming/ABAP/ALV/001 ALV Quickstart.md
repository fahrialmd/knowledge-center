# Purpose
Display internal table as ALV using CL_SALV_TABLE
# Reference
1. N/A
# Implementation
## 1. Preparation
Simple report example querying data from `EKKO` table
```ABAP

PARAMETERS: p_bukrs TYPE ekko-bukrs OBLIGATORY.  
  
DATA: gt_ekko TYPE STANDARD TABLE OF ekko,  
      go_alv  TYPE REF TO cl_salv_table.  
  
START-OF-SELECTION.  
  
  PERFORM frm_get_data.
```
Subroutine to get data
```ABAP
FORM frm_get_data .    
  SELECT   
      *  
    FROM   
      ekko  
    WHERE   
      bukrs = @p_bukrs  
    INTO TABLE   
      @gt_ekko.  
  IF sy-subrc <> 0.  
    MESSAGE 'No data found' TYPE 'E'.  
  ENDIF.  
ENDFORM.
```
## 2. Creating ALV
Using `cl_salv_table` class and `cl_salv_table=>factory method`
```ABAP
...
START-OF-SELECTION.  
  
  PERFORM frm_get_data.  
  
  PERFORM frm_alv_init.  
  
  PERFORM frm_alv_show.
...  
```
Subroutine to show simple ALV 
```ABAP
FORM frm_alv_init .  
  " Display ALV  
  TRY.  
      cl_salv_table=>factory(  
        IMPORTING  
          r_salv_table = go_alv  
        CHANGING  
          t_table      = gt_ekko ).  
  
      " Enable all standard functions (toolbar buttons)  
      go_alv->get_functions( )->set_all( ).  
  
    CATCH cx_salv_msg INTO DATA(lx_msg).  
      MESSAGE lx_msg->get_text( ) TYPE 'E'.  
  ENDTRY.  
ENDFORM.
```
```ABAP
FORM frm_alv_show.  
  
  TRY.  
      " Display  
      go_alv->display( ).  
    CATCH cx_salv_msg INTO DATA(lx_msg).  
      MESSAGE lx_msg->get_text( ) TYPE 'E'.  
  ENDTRY.  
  
ENDFORM.
```
# Result
![[Pasted image 20260119102145.png]]![[Pasted image 20260119102202.png]]