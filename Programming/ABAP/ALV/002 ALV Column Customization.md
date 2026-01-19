# Purpose
Customizing the column
# Reference
1. [[001 ALV Quickstart]]
# Implementation
## 1. Create subroutine
```ABAP
START-OF-SELECTION.  
  
  PERFORM frm_get_data.  
  
  PERFORM frm_alv_init.  
  
  PERFORM frm_alv_col_custom. "Add this 
  
  PERFORM frm_alv_show.
```
## 2. Create logic to customize column or field catalog
```ABAP
FORM frm_alv_col_custom .  
  
  TRY.  
      DATA(lo_columns) = go_alv->get_columns( ).  
  
      lo_columns->set_optimize( abap_true ).  
  
      " 1. EBELN - Purchasing Document Number  
      DATA(lo_col) = lo_columns->get_column( 'EBELN' ).  
      lo_col->set_short_text( 'PO#' ).           " Short text  
      lo_col->set_medium_text( 'PO Number' ).    " Medium text  
      lo_col->set_long_text( 'Purchase Order' ). " Long text  
      lo_col->set_output_length( 15 ).           " Width  
  
      " 2. BUKRS - Company Code  
      lo_col = lo_columns->get_column( 'BUKRS' ).  
      lo_col->set_short_text( 'Co.' ).  
      lo_col->set_medium_text( 'CoCd' ).  
      lo_col->set_long_text( 'Company Code' ).  
      lo_col->set_output_length( 6 ).  
  
      " 3. BSART - Document Type  
      lo_col = lo_columns->get_column( 'BSART' ).  
      lo_col->set_short_text( 'Typ' ).  
      lo_col->set_medium_text( 'Doc Type' ).  
      lo_col->set_long_text( 'Document Type' ).  
      lo_col->set_output_length( 10 ).  
  
      " 4. LIFNR - Vendor  
      lo_col = lo_columns->get_column( 'LIFNR' ).  
      lo_col->set_short_text( 'Vendor' ).  
      lo_col->set_medium_text( 'Vendor No.' ).  
      lo_col->set_long_text( 'Vendor Number' ).  
      lo_col->set_alignment( if_salv_c_alignment=>centered ). " Center align  
      lo_col->set_output_length( 12 ).  
  
      " 5. AEDAT - Creation Date  
      lo_col = lo_columns->get_column( 'AEDAT' ).  
      lo_col->set_short_text( 'Created' ).  
      lo_col->set_medium_text( 'Cre. Date' ).  
      lo_col->set_long_text( 'Creation Date' ).  
      lo_col->set_alignment( if_salv_c_alignment=>centered ).  
  
      " 6. WAERS - Currency  
      lo_col = lo_columns->get_column( 'WAERS' ).  
      lo_col->set_short_text( 'Cur' ).  
      lo_col->set_medium_text( 'Curr.' ).  
      lo_col->set_long_text( 'Currency' ).  
      lo_col->set_output_length( 5 ).  
  
      " 7. Hide unwanted columns  
      lo_columns->get_column( 'MANDT' )->set_visible( abap_false ). " Hide client  
      lo_columns->get_column( 'ERNAM' )->set_visible( abap_false ). " Hide created by  
      lo_columns->get_column( 'MEMORY' )->set_visible( abap_false ). " Hide incomplete flag  
  
    CATCH cx_salv_msg INTO DATA(lx_msg).  
      MESSAGE lx_msg->get_text( ) TYPE 'E'.  
  
    CATCH cx_salv_not_found.  
      MESSAGE 'Column not found' TYPE 'E'.  
  
  ENDTRY.  
  
ENDFORM.
```
# Result
![[Pasted image 20260119114504.png]]