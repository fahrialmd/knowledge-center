# Purpose
Creating a hotspot event, a clickable cell inside ALV
# Reference
1.[[001 ALV Quickstart]]
# Implementation
## 1. Preparation
Add new subroutine for event, include and object reference to local event handler class
```ABAP
REPORT z47825004alv.  
  
DATA: gt_ekpo TYPE STANDARD TABLE OF ekpo,  
      go_alv  TYPE REF TO cl_salv_table.  
  
INCLUDE z47825004alv_01.  
  
DATA go_event_handler TYPE REF TO lcl_event_handler.  
  
PARAMETERS: p_bukrs TYPE ekko-bukrs OBLIGATORY.  
  
START-OF-SELECTION.  
  
  PERFORM frm_get_data.  
  
  PERFORM frm_alv_init.  
  
  PERFORM frm_alv_col_custom.  
  
  PERFORM frm_alv_event.  
  
  PERFORM frm_alv_show.
```
## 2. Create a local event handler class
Hotspot event use `on_link_click` method as a trigger. When `EBELN` field and `MATNR` field are clicked by user, the screen will transition to TCODE `ME23N` and `MM03`
```ABAP
*&---------------------------------------------------------------------*  
*& Include          Z47825004ALV_01  
*&---------------------------------------------------------------------*  
  
CLASS lcl_event_handler DEFINITION.  
  PUBLIC SECTION.  
    METHODS: on_link_click  
      FOR EVENT link_click OF cl_salv_events_table  
      IMPORTING row column.  
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
ENDCLASS.
```
## 3. Updated column customization logic
```ABAP
FORM frm_alv_col_custom .  
  
  TRY.  
      DATA(lo_columns) = go_alv->get_columns( ).  
  
      lo_columns->set_optimize( abap_true ).  
  
      lo_columns->get_column( 'MANDT' )->set_visible( abap_false ). " Hide client  
  
      DATA(lo_display) = go_alv->get_display_settings( ).  
  
      " Enable zebra pattern  
      lo_display->set_striped_pattern( abap_true ).  
  
    CATCH cx_salv_msg INTO DATA(lx_msg).  
      MESSAGE lx_msg->get_text( ) TYPE 'E'.  
  
    CATCH cx_salv_not_found.  
      MESSAGE 'Column not found' TYPE 'E'.  
  
  ENDTRY.  
  
ENDFORM.
```
## 4. Create hotspot configuration logic
```ABAP
FORM frm_alv_event.  
  
  PERFORM frm_event_hotspot.    
  
ENDFORM.
```
```ABAP
FORM frm_event_hotspot.  
  DATA: lo_column TYPE REF TO cl_salv_column_table.  
  
  TRY.  
      " Set EBELN as hotspot  
      lo_column ?= go_alv->get_columns( )->get_column( 'EBELN' ).  
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).  
      lo_column->set_long_text( 'Purchase Order' ).  
  
      " Set MATNR as hotspot  
      lo_column ?= go_alv->get_columns( )->get_column( 'MATNR' ).  
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).  
      lo_column->set_long_text( 'Material Number' ).  
  
      " Hide MANDT  
      go_alv->get_columns( )->get_column( 'MANDT' )->set_visible( abap_false ).  
  
    CATCH cx_salv_not_found.  
      MESSAGE 'Column not found' TYPE 'E'.  
    CATCH cx_salv_msg INTO DATA(lx_msg).  
      MESSAGE lx_msg->get_text( ) TYPE 'E'.  
  ENDTRY.  
ENDFORM.
```
## 5. Create event assignment logic
```ABAP
FORM frm_alv_event.  
  
  PERFORM frm_event_hotspot.  
  
    TRY.  
      CREATE OBJECT go_event_handler.  
  
      DATA(lo_events) = go_alv->get_event( ).  
  
      " Register hotspot event  
      SET HANDLER go_event_handler->on_link_click FOR lo_events.  
  
    CATCH cx_salv_msg INTO DATA(lx_msg).  
      MESSAGE lx_msg->get_text( ) TYPE 'E'.  
  ENDTRY.  
  
ENDFORM.
```
# Result
![[Pasted image 20260119152130.png]]
![[Pasted image 20260119152140.png
![[Pasted image 20260119152156.png]]