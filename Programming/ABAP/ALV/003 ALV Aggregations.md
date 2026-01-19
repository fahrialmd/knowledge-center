# Purpose

# Reference
1. [[001 ALV Quickstart]]
2. [[002 ALV Column Customization]]
# Implementation
## 1. Modify the query
Modifying the query to get data from item table `EKPO`. because that table have quantity and amount fields
```ABAP
REPORT z47825003alv.  
  
PARAMETERS: p_bukrs TYPE ekko-bukrs OBLIGATORY.  
  
DATA: gt_ekpo TYPE STANDARD TABLE OF ekpo,  
      go_alv  TYPE REF TO cl_salv_table.  
  
START-OF-SELECTION.  
  
  PERFORM frm_get_data.  
  
  PERFORM frm_alv_init.  
  
  PERFORM frm_alv_aggregation.  
  
  PERFORM frm_alv_show.
```

```ABAP
FORM frm_get_data .  
  " Select data from EKKO  
  SELECT  
      *  
    FROM  
      ekpo  
    WHERE  
      bukrs = @p_bukrs  
    INTO TABLE  
      @gt_ekpo  
    UP TO 15 ROWS.  
  IF sy-subrc <> 0.  
    MESSAGE 'No data found' TYPE 'E'.  
  ENDIF.  
ENDFORM.
```
## 2. Column Logic (Optional)
```ABAP
FORM frm_alv_col_custom .  
  
  TRY.  
      DATA(lo_columns) = go_alv->get_columns( ).  
  
      lo_columns->set_optimize( abap_true ).  
  
      lo_columns->get_column( 'MANDT' )->set_visible( abap_false ). " Hide client  
  
    CATCH cx_salv_msg INTO DATA(lx_msg).  
      MESSAGE lx_msg->get_text( ) TYPE 'E'.  
  
    CATCH cx_salv_not_found.  
      MESSAGE 'Column not found' TYPE 'E'.  
  
  ENDTRY.  
  
ENDFORM.
```
## 3. Aggregation logic
```ABAP
FORM frm_alv_aggregation.  
  TRY.  
      DATA(lo_aggregations) = go_alv->get_aggregations( ).  
  
      "Total  
      DATA(lo_agg) = lo_aggregations->add_aggregation(  
        columnname  = 'MENGE'  
        aggregation = if_salv_c_aggregation=>total ).  
  
      "Maximum  
      lo_agg = lo_aggregations->add_aggregation(  
        columnname  = 'NETWR'  
        aggregation = if_salv_c_aggregation=>maximum ).  
  
      "Minimum  
      lo_agg = lo_aggregations->add_aggregation(  
        columnname  = 'BRTWR'  
        aggregation = if_salv_c_aggregation=>minimum ).  
  
      "Average  
      lo_agg = lo_aggregations->add_aggregation(  
        columnname = 'NETPR'  
        aggregation = if_salv_c_aggregation=>average  
      ).  
  
    CATCH cx_salv_msg cx_salv_not_found cx_salv_existing INTO DATA(lx_msg).  
      MESSAGE lx_msg->get_text( ) TYPE 'E'.  
  ENDTRY.  
ENDFORM.
```
# Result
![[Pasted image 20260119135534.png]]
