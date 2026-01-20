# Purpose
Adding custom button to SALV class. Custom button can only be added if we are using a custom GUI Status toolbar.

# Reference
1. [[001 ALV Quickstart]]
2. [[004 ALV Hotspot Event]]
# Implementation
## 1. Create a custom GUI Status
Create a GUI status and create custom button of your choice and a back button
![[Pasted image 20260120085800.png]]
## 2. Create event handler
Add logic when the button is clicked
```ABAP
CLASS lcl_event_handler DEFINITION.  
  PUBLIC SECTION.  
    METHODS:  
      on_link_click  
        FOR EVENT link_click OF cl_salv_events_table  
        IMPORTING row column,  
  
      on_user_command  
        FOR EVENT added_function OF cl_salv_events_table  
        IMPORTING e_salv_function.  
ENDCLASS.  
  
CLASS lcl_event_handler IMPLEMENTATION.  
  METHOD on_link_click.  
    READ TABLE gt_ekpo INTO DATA(ls_ekpo) INDEX row.  
    IF sy-subrc = 0.  
      CASE column.  
        WHEN 'EBELN'.  
          " Navigate to ME23N (Display PO)  
          SET PARAMETER ID 'BES' FIELD ls_ekpo-ebeln.  
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.  
  
        WHEN 'MATNR'.  
          " Navigate to MM03 (Display Material)  
          SET PARAMETER ID 'MAT' FIELD ls_ekpo-matnr.  
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.  
  
        WHEN OTHERS.  
          MESSAGE i001(00) WITH 'Clicked on' column 'Row:' row.  
      ENDCASE.  
    ENDIF.  
  ENDMETHOD.  
  
  METHOD on_user_command.  
    " Handle custom button clicks  
    CASE e_salv_function.  
  
      WHEN '&ZPOST'.  
        " Refresh data  
        MESSAGE 'Posting data...' TYPE 'I'.  
        go_alv->refresh( ).  
  
      WHEN '&ZBACK'.  
        LEAVE TO SCREEN 0.  
  
      WHEN OTHERS.  
        MESSAGE |Unknown function: { e_salv_function }| TYPE 'I'.  
  
    ENDCASE.  
  ENDMETHOD.  
ENDCLASS.
```
## 3. Initialize GUI status for ALV
use `set_screen_status` method to initialize the GUI status
```ABAP
FORM frm_alv_init .   
  TRY.  
      cl_salv_table=>factory(  
        IMPORTING  
          r_salv_table = go_alv  
        CHANGING  
          t_table      = gt_ekpo ).  
  
      go_alv->set_screen_status(  
       EXPORTING  
         report        =   sy-repid  
         pfstatus      = 'ZPFSTATUS'  
         set_functions = cl_salv_table=>c_functions_all  
      ).  
  
    CATCH cx_salv_msg INTO DATA(lx_msg).  
      MESSAGE lx_msg->get_text( ) TYPE 'E'.  
  ENDTRY.      
ENDFORM.
```
## 4. Using standard GUI status (Optional)
Copy a standard GUI status then add custom button. This method keeps the standard buttons and functionality intact. Copy from function group `SLVC_FULLSCREEN`. This is actually a better method.
![[Pasted image 20260120091637.png]]
Copy to the program and add a custom button
![[Pasted image 20260120091731.png]]
the button will appear in the toolbar
![[Pasted image 20260120091815.png]]
# Result
![[Pasted image 20260119164812.png]]
![[Pasted image 20260119164821.png]]
![[Pasted image 20260120091826.png]]