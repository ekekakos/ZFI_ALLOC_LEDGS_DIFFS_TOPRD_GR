CLASS zcl_alloc_ledgs_diffs_toprd_vw DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          imo_alloc_ledgs_difs_toprd_mdl TYPE REF TO zcl_alloc_ledgs_difs_toprd_mdl,
      initialize_salv IMPORTING im_report_name TYPE syrepid
                      CHANGING  ch_table       TYPE ANY TABLE,
      customize_salv,
      display,
      disable_enable_toolbar_button
        IMPORTING
          i_button_name TYPE char10
          i_isvisable   TYPE boolean DEFAULT abap_false.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF display_data.
             INCLUDE TYPE zfi_tax_mm_cost.
    TYPES:   ledger1    TYPE fins_ledger,
             hsl1       TYPE fins_vwcur12,
             ledger2    TYPE fins_ledger,
             hsl2       TYPE fins_vwcur12,
             difference TYPE itmf_de_dec_15_2,
           END OF display_data.
    TYPES: ret_display_data TYPE TABLE OF display_data WITH EMPTY KEY.

** Local Objects
    DATA: lo_custom_salv                TYPE REF TO zcl_custom_salv,
          lo_alloc_ledgs_difs_toprd_mdl TYPE REF TO zcl_alloc_ledgs_difs_toprd_mdl.

    DATA: internal_table LIKE lo_alloc_ledgs_difs_toprd_mdl->it_final_data.

** Variables for Dynamic Selection Screen
    DATA: p_bukrs  TYPE bukrs,
          p_gjahr  TYPE gjahr,
          " select-options: type range of.. (must be dictionary type)
          s_werks  TYPE ckf_werks_table,
          s_matnrs TYPE wrb_matnr_range_table.

    METHODS:
      "! Button pressed action
      "! @parameter e_salv_function |The function name of the button
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      selection_criteria_screen,
      get_data_from_table RETURNING VALUE(ret_tax_mm_cost) TYPE ret_display_data,
      text_in_alv_top_of_page
        IMPORTING
          im_alv_no TYPE num2.

ENDCLASS.

CLASS zcl_alloc_ledgs_diffs_toprd_vw IMPLEMENTATION.
  METHOD constructor.
    IF lo_alloc_ledgs_difs_toprd_mdl IS INITIAL.
      lo_alloc_ledgs_difs_toprd_mdl = imo_alloc_ledgs_difs_toprd_mdl.
    ENDIF.
  ENDMETHOD.

  METHOD customize_salv.
    " Set the LAYOUT
    lo_custom_salv->enable_layout_settings(
        lf_variant = lo_alloc_ledgs_difs_toprd_mdl->select_options_structure-p_layout
    ).

    " Set some display settings like Pattern, Title, Column Optimization.
    lo_custom_salv->set_display_settings(
      EXPORTING "optimize = ''
                i_title = |{ sy-title }|
      ).

    " Set the Toolbar
    lo_custom_salv->set_toolbar(
      EXPORTING
        status_name         = 'SALV_STANDARD' ##no_text
        i_button_name       = 'ZEDITTABLE'  ##no_text
        i_button_visable    = abap_false
    ).


    lo_custom_salv->set_columns( field_name_hide = 'MANDT' ).

    lo_alloc_ledgs_difs_toprd_mdl->get_str_sum_acdoca_amounts( ).

    text_in_alv_top_of_page( im_alv_no = 1 ).

*    disable_enable_check_post( is_check_visable = abap_true ).

* Add USER COMMAND Handlers for Local Class extent event
    SET HANDLER on_user_command FOR lo_custom_salv->r_events.
  ENDMETHOD.

  METHOD text_in_alv_top_of_page.
    DATA: Ledger1_text                TYPE string,
          Ledger1_amount              TYPE string,
          Ledger2_text                TYPE string,
          Ledger2_amount              TYPE string,
          difference_text             TYPE string,
          difference_amount           TYPE string,
          ovh_3rd_parties_ifrs_text   TYPE string,
          ovh_3rd_parties_ifrs_amount TYPE string,
          ovh_3rd_parties_tax_text    TYPE string,
          ovh_3rd_parties_tax_amount  TYPE string.

    CASE im_alv_no.
      WHEN 1.
        Ledger1_text      = |Ledger { lo_alloc_ledgs_difs_toprd_mdl->select_options_structure-p_1rldnr }|.
        ledger1_amount    = |{ lo_alloc_ledgs_difs_toprd_mdl->get_str_sum_acdoca_amounts( )-hsl NUMBER = USER }|.
        ovh_3rd_parties_ifrs_text = |OVH 3rd Parties IFRS|.
        ovh_3rd_parties_ifrs_amount = |{ lo_alloc_ledgs_difs_toprd_mdl->get_str_sum_acdoca_amounts( )-hsl_3rd_ifrs NUMBER = USER }|.
        Ledger2_text      = |Ledger { lo_alloc_ledgs_difs_toprd_mdl->select_options_structure-p_2rldnr }|.
        Ledger2_amount    = |{ lo_alloc_ledgs_difs_toprd_mdl->get_str_sum_acdoca_amounts( )-hsl2 NUMBER = USER }|.
        ovh_3rd_parties_tax_text = |OVH 3rd Parties TAX|.
        ovh_3rd_parties_tax_amount = |{ lo_alloc_ledgs_difs_toprd_mdl->get_str_sum_acdoca_amounts( )-hsl_3rd_tax NUMBER = USER }|.
        difference_text   = |Ledgers Difference|.
        difference_amount = |{ lo_alloc_ledgs_difs_toprd_mdl->get_str_sum_acdoca_amounts( )-diffs NUMBER = USER }|.
      WHEN 2.
        Ledger1_text = |Ledger { lo_alloc_ledgs_difs_toprd_mdl->select_options_structure-p_1rldnr }|.
        Ledger2_text = |Ledger { lo_alloc_ledgs_difs_toprd_mdl->select_options_structure-p_1rldnr }|.
        difference_text = |Ledgers Difference|.
    ENDCASE.

    lo_custom_salv->set_top_of_page(
        EXPORTING
          i_label_row     = 1
          i_label_column  = 1
          i_label_text    = Ledger2_text
     ).
    lo_custom_salv->set_top_of_page(
        EXPORTING
          i_flow_row     = 2
          i_flow_column  = 1
          i_flow_text    = Ledger2_amount
     ).

    lo_custom_salv->set_top_of_page(
      EXPORTING
        i_label_row     = 1
        i_label_column  = 2
        i_label_text    = ovh_3rd_parties_tax_text
   ).
    lo_custom_salv->set_top_of_page(
        EXPORTING
          i_flow_row     = 2
          i_flow_column  = 2
          i_flow_text    = ovh_3rd_parties_tax_amount
     ).

    lo_custom_salv->set_top_of_page(
        EXPORTING
          i_label_row     = 1
          i_label_column  = 3
          i_label_text    = Ledger1_text
     ).
    lo_custom_salv->set_top_of_page(
        EXPORTING
          i_flow_row     = 2
          i_flow_column  = 3
          i_flow_text    = ledger1_amount
     ).

    lo_custom_salv->set_top_of_page(
      EXPORTING
        i_label_row     = 1
        i_label_column  = 4
        i_label_text    = ovh_3rd_parties_ifrs_text
   ).
    lo_custom_salv->set_top_of_page(
        EXPORTING
          i_flow_row     = 2
          i_flow_column  = 4
          i_flow_text    = ovh_3rd_parties_ifrs_amount
     ).

    lo_custom_salv->set_top_of_page(
      EXPORTING
        i_label_row     = 1
        i_label_column  = 5
        i_label_text    = difference_text
   ).
    lo_custom_salv->set_top_of_page(
        EXPORTING
          i_flow_row     = 2
          i_flow_column  = 5
          i_flow_text    = difference_amount
     ).

  ENDMETHOD.


  METHOD disable_enable_toolbar_button.
    lo_custom_salv->set_toolbar(
        EXPORTING
            i_button_name       = i_button_name
            i_button_visable    = i_isvisable
      ).
  ENDMETHOD.

  METHOD display.
    lo_custom_salv->display_alv( ).
  ENDMETHOD.

  METHOD initialize_salv.
    " If Object is initial.
    IF lo_custom_salv IS INITIAL.
      internal_table = ch_table.
* initialize the salv object
      CREATE OBJECT lo_custom_salv
        EXPORTING
          itab_type = internal_table.

* Initialize the ALV
      lo_custom_salv->initialize_alv(
        EXPORTING
          i_report_name        = im_report_name
        CHANGING
          itab_to_be_displayed = internal_table ).
    ELSE.
      lo_custom_salv->refresh_alv( ).
      cl_gui_cfw=>flush( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_user_command.
    DATA: user_response(1) TYPE c.
    CASE e_salv_function.
      WHEN '&IC1'.  "user will click one of rows in ALV
      WHEN 'ZFINALPOST'.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'User Prompt'
            text_question         = 'This will lock the period'
            text_button_1         = 'YES'(002)
*           icon_button_1         = space
            text_button_2         = 'NO'(002)
*           icon_button_2         = space
            default_button        = '2'
            display_cancel_button = ' '
          IMPORTING
            answer                = user_response
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        CHECK user_response = 1.
        lo_alloc_ledgs_difs_toprd_mdl->post_into_zfi_tax_mm_cost(
            EXPORTING
              im_final_post = 'X'
            CHANGING
              cht_final_data = internal_table
          ).
      WHEN 'ZDISPLTABL'.     " Display table
        selection_criteria_screen( ).
        DATA(lt_tax_mm_cost) = get_data_from_table( ).
        DATA(lo_tax_mm_cost_alv) = NEW zcl_custom_salv( lt_tax_mm_cost ).

        lo_tax_mm_cost_alv->initialize_alv(
            EXPORTING
              i_report_name = sy-repid
            CHANGING
              itab_to_be_displayed = lt_tax_mm_cost
          ).
        " Set the LAYOUT
        lo_tax_mm_cost_alv->enable_layout_settings( ).

        " Set some display settings like Pattern, Title, Column Optimization.
        lo_tax_mm_cost_alv->set_display_settings(
          EXPORTING "optimize = ''
                    i_title = |The splitted values { sy-title }|
          ).

        lo_tax_mm_cost_alv->set_columns(
            EXPORTING
              field_name_hide   = 'MANDT'
              fname_change_text = 'DIFFERENCE'
              field_short_text  = 'Difference'
              field_medium_text = 'Difference'
              field_long_text   = 'Difference'
               ).

        lo_tax_mm_cost_alv->set_columns(
            EXPORTING
              fname_change_text = 'LEDGER1'
              field_short_text  = '1st Ledger'
              field_medium_text = '1st Ledger'
              field_long_text   = '1st Ledger'
               ).

        lo_tax_mm_cost_alv->set_columns(
            EXPORTING
              fname_change_text = 'LEDGER2'
              field_short_text  = '2nd Ledger'
              field_medium_text = '2nd Ledger'
              field_long_text   = '2nd Ledger'
               ).

        lo_tax_mm_cost_alv->set_columns(
            EXPORTING
              fname_change_text = 'HSL1'
              field_short_text  = 'Ledger1 Am'
              field_medium_text = 'Ledger1 Amount'
              field_long_text   = 'Ledger1 Amount'
               ).

        lo_tax_mm_cost_alv->set_columns(
            EXPORTING
              fname_change_text = 'HSL2'
              field_short_text  = 'Ledger2 Am'
              field_medium_text = 'Ledger2 Amount'
              field_long_text   = 'Ledger2 Amount'
               ).
        " Set the Toolbar
        lo_tax_mm_cost_alv->set_toolbar( ).
        lo_tax_mm_cost_alv->display_alv( ).

      WHEN 'ZPOST'.      " Post
        lo_alloc_ledgs_difs_toprd_mdl->post_into_zfi_tax_mm_cost(
          EXPORTING
            im_final_post = ' '
          CHANGING
            cht_final_data = internal_table
        ).
        " Refresh ALV
        lo_custom_salv->refresh_alv( ).
        cl_gui_cfw=>flush( ).
      WHEN 'ZEDITTABLE'.
        lo_alloc_ledgs_difs_toprd_mdl->edit_table_sm30( ).

    ENDCASE.
  ENDMETHOD.                    "on_user_command

  METHOD selection_criteria_screen.
    p_bukrs = lo_alloc_ledgs_difs_toprd_mdl->select_options_structure-p_rbukrs.
    p_gjahr = lo_alloc_ledgs_difs_toprd_mdl->select_options_structure-p_gjahr.

    cl_ci_query_attributes=>generic(
        EXPORTING
          " unique screen ID
          p_name       = CONV #( sy-repid )
          " Screen title
          p_title      = 'Generic screen for input'
          " Screen fields
          p_attributes = VALUE #(
            " parameter field
           ( kind = 'S' obligatory = abap_true text = 'Company code'(001) ref = REF #( p_bukrs ) )
           ( kind = 'P' obligatory = abap_true text = 'Year'(004) ref = REF #( p_gjahr ) )
           " select-option
           ( kind = 'S' text = 'Plant'(002) ref = REF #( s_werks ) )
           ( kind = 'S' text = 'Materials'(003) ref = REF #( s_matnrs ) )
           )  " Table of Attribute Entries
          p_display    = abap_false    " General Flag
        ).
  ENDMETHOD.


  METHOD get_data_from_table.

    SELECT FROM zfi_tax_mm_cost AS zsplit INNER JOIN zfi_ledgers_diff AS rldnrs
        ON zsplit~bukrs = rldnrs~bukrs AND
           zsplit~gjahr = rldnrs~gjahr
      FIELDS zsplit~bukrs,zsplit~gjahr,zsplit~werks,zsplit~matnr,zsplit~prod_qty,zsplit~ovh_ifrs_value,
             zsplit~ovh_diff_ifrs_tax_val,zsplit~annual_ovh_cost_tax_val,zsplit~ending_stock,
             zsplit~total_remaining_stock_ifrs_val,zsplit~diff_of_stock_val,zsplit~total_remaining_stock_tax_val,
             zsplit~cogs_qty,zsplit~cogs_ifrs_value,zsplit~diff_of_cogs_value,zsplit~revenue,zsplit~cogs_tax_value,
             zsplit~avg_rm_ifrs_value,zsplit~avg_cogs_ifrs_value,zsplit~avg_tax_stock_value,zsplit~avg_tax_cogs_value,rldnrs~ledger1,
             rldnrs~hsl1,rldnrs~ledger2,rldnrs~hsl2,rldnrs~difference
    WHERE zsplit~bukrs = @p_bukrs AND
          zsplit~gjahr = @p_gjahr AND
          zsplit~werks IN @s_werks AND
          zsplit~matnr IN @s_matnrs
    INTO CORRESPONDING FIELDS OF TABLE @ret_tax_mm_cost.

  ENDMETHOD.

ENDCLASS.
