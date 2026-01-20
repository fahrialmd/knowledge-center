# Purpose
Create a selection feature for the ALV and access the selected data

# Reference
1. [[001 ALV Quickstart]]
2. [[005 ALV Toolbar Button Event]]
# Implementation
## 1. Initialize selection mode
```ABAP
REPORT z47825006alv.  
  
DATA: gt_ekpo TYPE STANDARD TABLE OF ekpo,  
      go_alv  TYPE REF TO cl_salv_table.  
  
INCLUDE z47825006alv_01.  
  
DATA go_event_handler TYPE REF TO lcl_event_handler.  
  
PARAMETERS: p_bukrs TYPE ekko-bukrs OBLIGATORY.  
  
START-OF-SELECTION.  
  
  PERFORM frm_get_data.  
  
  PERFORM frm_alv_init.  
  
  PERFORM frm_alv_selection.  
  
  PERFORM frm_alv_col_custom.  
  
  PERFORM frm_alv_event.  
  
  PERFORM frm_alv_show.
```
```ABAP
FORM frm_alv_selection.  
  TRY.  
      DATA(lo_selections) = go_alv->get_selections( ).  
  
      " Set selection mode  
      " Options:  
      " - if_salv_c_selection_mode=>single      (single row)  
      " - if_salv_c_selection_mode=>multiple    (multiple rows)  
      " - if_salv_c_selection_mode=>row_column  (row and column)  
      " - if_salv_c_selection_mode=>cell        (individual cells)  
  
      lo_selections->set_selection_mode(  
        if_salv_c_selection_mode=>row_column ).  " ← Multiple row selection  
  
    CATCH cx_salv_msg INTO DATA(lx_msg).  
      MESSAGE lx_msg->get_text( ) TYPE 'E'.  
  ENDTRY.  
ENDFORM.
```
## 2. Create custom button for selection trigger
![[Pasted image 20260120103559.png]]
## 3. Create a trigger 
```ABAP
  METHOD on_user_command.  
    " Handle custom button clicks  
    CASE e_salv_function.  
  
      WHEN '&ZPOST'.  
        " Refresh data  
        MESSAGE 'Posting data...' TYPE 'I'.  
        go_alv->refresh( ).  
  
      WHEN '&ZSELECT'.  
        DATA(lo_selections) = go_alv->get_selections( ).  
        DATA(lt_rows) = lo_selections->get_selected_rows( ).  
        PERFORM frm_display_selected_data USING lt_rows.  
        MESSAGE 'selected data...' TYPE 'I'.  
  
      WHEN OTHERS.  
        MESSAGE |Unknown function: { e_salv_function }| TYPE 'I'.  
  
    ENDCASE.  
  ENDMETHOD.
```
```ABAP
FORM frm_display_selected_data USING ut_rows TYPE salv_t_row .  
  
  DATA: lt_selected_data LIKE gt_ekpo.  
  lt_selected_data = VALUE #(  
    FOR lv_index IN ut_rows  
    ( gt_ekpo[ lv_index ] )  
  ).  
  
  CL_DEMO_OUTPUT=>display(  
    data    = lt_selected_data  
*    name    =                  " Name  
*    exclude =                  " Exclude structure components  
*    include =                  " Include structure components  
  ).  
  
ENDFORM.
```
# Result
![[Pasted image 20260120103710.png]]