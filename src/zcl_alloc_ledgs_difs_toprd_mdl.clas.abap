"! <p class="shorttext synchronized" lang="en">
"! Bus. process (Model) of PRG ZFI_ALLOC_LEDGS_DIFFS_TOPRD_GR</p>
CLASS zcl_alloc_ledgs_difs_toprd_mdl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF sum_acdoca_amounts,
             rbukrs       TYPE bukrs,
             gjahr        TYPE gjahr,
             "! The amount of the 1st Ledger
             hsl          TYPE fins_vwcur12,
             hsl_3rd_ifrs TYPE fins_vwcur12,
             "! The amount of the 2nd Ledger
             hsl2         TYPE fins_vwcur12,
             hsl_3rd_tax  TYPE fins_vwcur12,
             "! The difference between HSL2 - HSL1
             diffs        TYPE fins_vwcur12,
           END OF sum_acdoca_amounts.

    TYPES: BEGIN OF select_options_parameters,
             p_rbukrs TYPE bukrs,
             p_1rldnr TYPE rldnr,
             p_2rldnr TYPE rldnr,
             p_rrcty  TYPE rrcty,
             p_gjahr  TYPE gjahr,
             p_3rldnr TYPE rldnr,
             p_layout TYPE disvariant-variant,
             s_budat  TYPE RANGE OF budat,
             s_racct  TYPE RANGE OF racct,
             s_rcntr  TYPE RANGE OF kostl,
             s_bttyp  TYPE RANGE OF fins_bttype,
             saccasty TYPE RANGE OF fins_accasty_n1,
             s_rfarea TYPE RANGE OF fkber,
             s_sfarea TYPE RANGE OF fkber,
             s_mtart  TYPE RANGE OF mtart,
             s_bklas  TYPE RANGE OF bklas,
             s_bttype TYPE RANGE OF fins_bttype,
             s_awtyp  TYPE RANGE OF awtyp,
             s_autyp  TYPE RANGE OF auftyp,
             s_auart  TYPE RANGE OF auart,
             s_3racct TYPE RANGE OF racct,
           END OF select_options_parameters.
    DATA: select_options_structure TYPE select_options_parameters READ-ONLY.

    TYPES: tt_final_data             TYPE TABLE OF zfi_tax_mm_cost.

    DATA:
      it_final_data     TYPE TABLE OF zfi_tax_mm_cost,
      str_final_data    TYPE zfi_tax_mm_cost,
      it_error_messages TYPE esp1_message_tab_type READ-ONLY,
      acdoca_ifrs_total TYPE fins_vwcur12 READ-ONLY.

    METHODS:
      constructor
        IMPORTING
          current_report TYPE rsvar-report OPTIONAL,
      "! <p class="shorttext synchronized" lang="en"><strong>Doing all the work until display of records</strong></p>
      "! This is the main method that call all other ones that get and process the data
      "! and returns the final ITAB
      "! @parameter rit_final_data | <p class="shorttext synchronized" lang="en">
      "! The ITAB with the final data</p>
      find_prepare_get_data
        RETURNING
          VALUE(rit_final_data) LIKE it_final_data,

      "! <p class="shorttext synchronized" lang="en"><strong>Post the displayed data</strong></p>
      "! This method will post/update (through MODIFY) the displayed records to the table ZFI_TAX_MM_COST
      "! @parameter im_final_post | <p class="shorttext synchronized" lang="en">Post or NOT in J_1GCONTROL</p>
      "! @parameter cht_final_data | <p class="shorttext synchronized" lang="en">The final ITAB</p>
      post_into_zfi_tax_mm_cost
        IMPORTING im_final_post  TYPE char1
        CHANGING  cht_final_data LIKE it_final_data,
      "! <p class="shorttext synchronized" lang="en"><strong>Return ACDOCA SUMs</strong></p>
      "! Returns a structure with the summary of IFRS & TAX ledgres and their difference
      "! @parameter r_result | <p class="shorttext synchronized" lang="en"></p>
      get_str_sum_acdoca_amounts RETURNING VALUE(r_result) TYPE sum_acdoca_amounts,
      check_authorization,
      set_alloc_ledgs_difs_toprd_vw
        IMPORTING
          imo_alloc_ledgs_difs_toprd_vw TYPE REF TO zcl_alloc_ledgs_diffs_toprd_vw,
      edit_table_sm30.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: it_sum_acdoca_amounts  TYPE TABLE OF sum_acdoca_amounts WITH DEFAULT KEY,
*          it_sum_3rd_parties_amounts TYPE TABLE OF sum_3rd_parties_amounts WITH DEFAULT KEY,
          str_sum_acdoca_amounts TYPE sum_acdoca_amounts.

    TYPES: BEGIN OF whb_data,
             werks     TYPE werks,
             matnr     TYPE matnr,
             docucateg TYPE j_1gvl_valeq,
             menge     TYPE j_1gvl_menge,
             dmbtr     TYPE j_1gvl_dmbtr,
           END OF whb_data,
           BEGIN OF production_materials,
             matnr TYPE matnr,
             werks TYPE werks_d,
           END OF production_materials.

    DATA: it_whb_data TYPE TABLE OF whb_data WITH EMPTY KEY.

    DATA: selection_table     TYPE rsparams_tt,
          current_report_name TYPE sy-repid.

    DATA: it_production_materials       TYPE TABLE OF production_materials,
          lo_alloc_ledgs_diffs_toprd_vw TYPE REF TO zcl_alloc_ledgs_diffs_toprd_vw.

    METHODS:
      "! <p class="shorttext synchronized" lang="en"><strong>Fill the SELECTION-SCREEN structure</strong></p>
      "! Fill a SELECTION-SCREEN structure from the FM RS_REFRESH_FROM_SELECTOPTIONS <br/>
      "! with the Selection Screen data (Parameters & Select-Options) through the Program Name<br/>
      "! and by processing the returned ITAB.
      fill_select_options_structure,
      "! <p class="shorttext synchronized" lang="en"><strong>Get Ledgers amounts and find the Difference</strong></p>
      "! It will select the SUM amounts, HSLs, from ACDOCA with criteria the ones from Selection Screen
      find_ledgers_amount_diff RAISING zcx_alloc_ledgs_diffs_toprd_gr,
      "! <p class="shorttext synchronized" lang="en"><strong>Get sum amount of 3rd parties & PM orders</strong></p>
      "! This method will find and sum the amounts of 3rd parties and PM orders from ACDOCA.
      find_ovh_expences_3rd_parties,
      "! <p class="shorttext synchronized" lang="en"><strong>Get Productive Materials</strong></p>
      "! It will get the Productive Final products, by putting them in it_production_materials,<br/>
      "! from MARA & MBEW by getting the ones that have
      "! the MTART in ZFG (CCHBC Finished Goods) & ZFGS (CCHBC Sensitive FG) and<br/>
      "! the BKLAS in 1000 (Finished Goods CCHBC) & 1020 (Finished Goods Chips Prod).<br/>
      "! It will also check that these combinations exists in table J_1GVL_WHB003
      "! and will return the itab it_production_materials.
      find_production_materials RAISING zcx_alloc_ledgs_diffs_toprd_gr,
      "! <p class="shorttext synchronized" lang="en"><strong>Get Quantities & Amounts from WHB</strong></p>
      "! Get the data from J_1GVL_WHB010 with inner join to GTT with productive materials<br/>
      "! with criteria:
      "! <ol><li> The TRACT (Transaction Category is in 'PD', 'SL', 'RM', 'RV', 'AE', 'EE'.</li>
      "! <li>The AUFNR will be empty, only total PDs, and</li>
      "! <li>Both AE and EE are not 0 (Zero).</li></ol>
      "! After the selection the itab ZGTT_PD_MATERIAL must be emptied otherwise we will have a short dump.
      get_data_from_whb RAISING zcx_alloc_ledgs_diffs_toprd_gr,
      "! <p class="shorttext synchronized" lang="en"><strong>
      "! Fill Final ITAB & calculate the remaining</strong></p>
      "! In this method the program will LOOP in the ITAB it_whb_data and will fill the appropriate fields<br/>
      "! of the final ITAB it_final_data.<br/>
      "! Then it will calculate all the remaining fields.
      prepare_final_data,
      display_error_messages IMPORTING lo_error_object TYPE REF TO cx_root,
      find_calculated_fields.
ENDCLASS.



CLASS zcl_alloc_ledgs_difs_toprd_mdl IMPLEMENTATION.

  METHOD set_alloc_ledgs_difs_toprd_vw.
    me->lo_alloc_ledgs_diffs_toprd_vw = imo_alloc_ledgs_difs_toprd_vw.
  ENDMETHOD.

  METHOD constructor.
    IF current_report <> ''.
      current_report_name = current_report.
      "Get the values from the SELECTION-SCREEN in an ITAB
      CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
        EXPORTING
          curr_report     = current_report      " Program for which selections are to be displayed
        TABLES
          selection_table = selection_table     " Table with ranges structure that contains selections
        EXCEPTIONS
          not_found       = 1                   " Program does not exist
          no_report       = 2                   " Program is not type 1
          OTHERS          = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      "Fill a SELECTION-SCREEN structure from the previous ITAB
      fill_select_options_structure( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_select_options_structure.
    FIELD-SYMBOLS <range> TYPE STANDARD TABLE.

    LOOP AT selection_table ASSIGNING FIELD-SYMBOL(<ls_selection>).
      CASE <ls_selection>-kind.
        WHEN 'P'.   "parameter
          ASSIGN COMPONENT <ls_selection>-selname OF STRUCTURE select_options_structure TO FIELD-SYMBOL(<lv_value>).
          IF sy-subrc EQ 0 AND <ls_selection>-low IS NOT INITIAL.
            <lv_value> = <ls_selection>-low.
          ENDIF.
        WHEN 'S'.   "select-option
          ASSIGN COMPONENT <ls_selection>-selname OF STRUCTURE select_options_structure TO <range>.
** If the <ls_selection>-low is initial, it will add it with empty values and
* will return only the records with EMPTY field.
          IF sy-subrc EQ 0 AND <ls_selection>-low IS NOT INITIAL.
            APPEND INITIAL LINE TO <range> ASSIGNING FIELD-SYMBOL(<ls_range>).
            IF sy-subrc EQ 0.
              MOVE-CORRESPONDING <ls_selection> TO <ls_range>.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD find_ledgers_amount_diff.
    SELECT FROM acdoca
    FIELDS rbukrs,gjahr,
           SUM( CASE rldnr "put the ledger's amount to the right field
                WHEN @select_options_structure-p_1rldnr THEN hsl
                END ) AS hsl,
           SUM( CASE rldnr
                WHEN @select_options_structure-p_2rldnr THEN hsl
                END ) AS hsl2
    WHERE rbukrs = @select_options_structure-p_rbukrs AND
        ( rldnr = @select_options_structure-p_1rldnr OR rldnr = @select_options_structure-p_2rldnr ) AND
          rrcty = @select_options_structure-p_rrcty AND
          gjahr = @select_options_structure-p_gjahr AND
          racct IN @select_options_structure-s_racct AND
          rcntr IN @select_options_structure-s_rcntr AND
          bttype IN @select_options_structure-s_bttyp  AND
          co_accasty_n1 IN  @select_options_structure-saccasty AND
          rfarea IN @select_options_structure-s_rfarea AND
          sfarea IN @select_options_structure-s_sfarea
    GROUP BY rbukrs,gjahr
      INTO CORRESPONDING FIELDS OF TABLE @it_sum_acdoca_amounts.

    IF it_sum_acdoca_amounts IS INITIAL.
      RAISE EXCEPTION NEW zcx_alloc_ledgs_diffs_toprd_gr(
        textid   = zcx_alloc_ledgs_diffs_toprd_gr=>zcx_no_data_found
        msgv1    = 'IT_SUM_ACDOCA_AMOUNTS'
      ).
    ENDIF.
  ENDMETHOD.

  METHOD find_ovh_expences_3rd_parties.
    SELECT SINGLE FROM acdoca
    FIELDS acdoca~rbukrs,acdoca~gjahr,
           SUM( hsl ) AS hsl_3rd_ifrs
    WHERE acdoca~rbukrs = @select_options_structure-p_rbukrs AND
          acdoca~rldnr =  @select_options_structure-p_3rldnr AND
          acdoca~gjahr =  @select_options_structure-p_gjahr   AND
          acdoca~bttype IN @select_options_structure-s_bttype AND
          acdoca~awtyp  IN @select_options_structure-s_awtyp  AND
          acdoca~racct  IN @select_options_structure-s_3racct AND
          acdoca~autyp  IN @select_options_structure-s_autyp
    GROUP BY acdoca~rbukrs,acdoca~gjahr
      INTO @DATA(str_acdoca_ifrs_3rd_party).
    IF sy-subrc = 0.
      it_sum_acdoca_amounts[ rbukrs = str_acdoca_ifrs_3rd_party-rbukrs
                             gjahr = str_acdoca_ifrs_3rd_party-gjahr ]-hsl_3rd_ifrs = str_acdoca_ifrs_3rd_party-hsl_3rd_ifrs.
    ENDIF.
  ENDMETHOD.

  METHOD find_prepare_get_data.
    TRY.
        find_ledgers_amount_diff( ).
        find_ovh_expences_3rd_parties( ).
        find_calculated_fields( ).
        find_production_materials( ).
        get_data_from_whb( ).
        prepare_final_data( ).
        rit_final_data = it_final_data.

      CATCH zcx_alloc_ledgs_diffs_toprd_gr
            cx_root INTO DATA(lo_cx_program_error).
        display_error_messages( lo_cx_program_error ).

    ENDTRY.
  ENDMETHOD.


  METHOD find_production_materials.

    SELECT DISTINCT marc~matnr, marc~werks
    FROM mara INNER JOIN marc
        ON mara~matnr = marc~matnr
      INNER JOIN t001k
        ON marc~werks = t001k~bwkey
      INNER JOIN mbew
        ON marc~matnr = mbew~matnr AND
           marc~werks = mbew~bwkey
      INNER JOIN j_1gvl_whb003 AS jwhb3
        ON mara~mtart = jwhb3~mtart AND
           mbew~bklas = jwhb3~bklas
    WHERE mara~mtart IN @select_options_structure-s_mtart AND
          mbew~bklas IN @select_options_structure-s_bklas AND
          t001k~bukrs = @select_options_structure-p_rbukrs
     ORDER BY marc~matnr, marc~werks
     INTO TABLE @it_production_materials.

    IF it_production_materials IS INITIAL.
      RAISE EXCEPTION NEW zcx_alloc_ledgs_diffs_toprd_gr(
        textid   = zcx_alloc_ledgs_diffs_toprd_gr=>zcx_no_data_found
        msgv1    = 'IT_PRODUCTION_MATERIALS'
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_data_from_whb.
    DATA: lv_periv   TYPE periv,
          lv_poper   TYPE poper,
          budat_low  TYPE sy-datum,
          budat_high TYPE sy-datum.

    INSERT zgtt_pd_material FROM TABLE @it_production_materials.
    FREE: it_production_materials.

    SELECT FROM j_1gvl_whb010 AS jwhb10
      INNER JOIN zgtt_pd_material AS pd_matnrs
        ON jwhb10~matnr = pd_matnrs~matnr AND
           jwhb10~werks = pd_matnrs~werks
    FIELDS jwhb10~werks, jwhb10~matnr, jwhb10~tract, SUM( jwhb10~menge ) AS menge, SUM( jwhb10~dmbtr ) AS dmbtr
    WHERE jwhb10~bukrs = @select_options_structure-p_rbukrs AND
          jwhb10~budat IN @select_options_structure-s_budat AND
*          jwhb10~aufnr = '' AND  "NO PD per Production order
          jwhb10~tract = 'AE' AND
          " If AE is 0, then we have a Subcontracting and no Production
          EXISTS ( SELECT * FROM j_1gvl_whb010 WHERE bukrs = jwhb10~bukrs AND
                                                     budat = jwhb10~budat AND
                                                     matnr = jwhb10~matnr AND
                                                     tract = 'AE' AND
                                                     dmbtr <> 0 )
    GROUP BY jwhb10~matnr, jwhb10~werks, jwhb10~tract
      INTO TABLE @it_whb_data.

    IF it_whb_data IS INITIAL.
      " The GTT must be deleted after its use otherwise we will have a short dump.
      DELETE FROM zgtt_pd_material.
      RAISE EXCEPTION NEW zcx_alloc_ledgs_diffs_toprd_gr(
        textid   = zcx_alloc_ledgs_diffs_toprd_gr=>zcx_no_data_found
        msgv1    = 'IT_WHB_DATA'
      ).
    ENDIF.

** Now the itab IT_WHB_DATA contains the AE & EE of the materials of the plants that are productive.
* So we will transfer to the GTT and will make them unique by deleting the duplicates compairing only the MATNR.
* And then we will get records from WHB010 with the TRACT we want but ;only for these material but for all plants.
    DELETE FROM zgtt_pd_material.
    it_production_materials = CORRESPONDING #( it_whb_data ).
    DELETE ADJACENT DUPLICATES FROM it_production_materials COMPARING matnr.
    INSERT zgtt_pd_material FROM TABLE @it_production_materials.
    FREE: it_production_materials.

    SELECT FROM j_1gvl_whb010 AS jwhb10
      INNER JOIN zgtt_pd_material AS pd_matnrs
        ON jwhb10~matnr = pd_matnrs~matnr "AND
    FIELDS jwhb10~werks, jwhb10~matnr, jwhb10~tract, SUM( jwhb10~menge ) AS menge, SUM( jwhb10~dmbtr ) AS dmbtr
    WHERE jwhb10~bukrs = @select_options_structure-p_rbukrs AND
          jwhb10~budat IN @select_options_structure-s_budat AND
*          jwhb10~aufnr <> '' AND  "PD Quantity only when is filled
          jwhb10~tract IN ( 'SL', 'RM', 'RV', 'DF', 'OP', 'ER', 'DS' )  "AND
    GROUP BY jwhb10~matnr, jwhb10~werks, jwhb10~tract
      APPENDING TABLE @it_whb_data.

*    SELECT FROM j_1gvl_whb010 AS jwhb10
*      INNER JOIN zgtt_pd_material AS pd_matnrs
*        ON jwhb10~matnr = pd_matnrs~matnr
*    FIELDS jwhb10~werks, jwhb10~matnr, jwhb10~tract, SUM( jwhb10~menge ) AS menge, SUM( jwhb10~dmbtr ) AS dmbtr
*    WHERE jwhb10~bukrs = @select_options_structure-p_rbukrs AND
*          jwhb10~budat IN @select_options_structure-s_budat AND
*          jwhb10~aufnr = '' AND  "NO PD per Production order
*          jwhb10~tract IN ( 'PD', 'SL', 'RM', 'RV', 'AE', 'EE', 'DF', 'OP', 'ER', 'DS' )  AND
*          " If both AE & EE are 0, then we have a Subcontracting and no Production
*          EXISTS ( SELECT * FROM j_1gvl_whb010 WHERE bukrs = jwhb10~bukrs AND
*                                                     budat = jwhb10~budat AND
*                                                     matnr = jwhb10~matnr AND
*                                                     tract IN ( 'AE', 'EE' ) AND
*                                                     dmbtr <> 0 )
*    GROUP BY jwhb10~werks, jwhb10~matnr, jwhb10~tract
*      INTO TABLE @it_whb_data.

    SORT it_whb_data BY matnr werks docucateg.

  ENDMETHOD.


  METHOD prepare_final_data.
    DATA: sum_ending_stock               TYPE zending_stock,
          sum_cogs_qty                   TYPE zending_stock,
          lv_total_rm_cogs               TYPE zending_stock,
          sum_ovh_dif_ifrs_tax_per_matnr TYPE zovh_ifrs_value,
          sum_OVH_IFRS                   TYPE zovh_ifrs_value,
          lv_group_tabix                 TYPE sy-tabix.

** In this loop we are going to put the field valuw in a line per BUKRS, GJAHR, WERKS & MATNR.
    LOOP AT it_whb_data INTO DATA(ls_whb_data) GROUP BY ( werks = ls_whb_data-werks
                                                          matnr = ls_whb_data-matnr ).
      APPEND INITIAL LINE TO it_final_data ASSIGNING FIELD-SYMBOL(<wa_final_data>).
      <wa_final_data> = VALUE #( bukrs = select_options_structure-p_rbukrs
                                 gjahr = select_options_structure-p_gjahr
                                 werks = ls_whb_data-werks
                                 matnr = ls_whb_data-matnr ).
      LOOP AT GROUP ls_whb_data ASSIGNING FIELD-SYMBOL(<per_werks_matnr>).
        CASE <per_werks_matnr>-docucateg.
*          WHEN 'PD'.  "Production Quantity
*            <wa_final_data>-prod_qty = <per_werks_matnr>-menge.
          WHEN 'AE'.  "OVerHead Production Amount
            <wa_final_data>-ovh_ifrs_value = <wa_final_data>-ovh_ifrs_value + <per_werks_matnr>-dmbtr.
            <wa_final_data>-prod_qty = <per_werks_matnr>-menge.
          WHEN 'RM'.  "Remaining values
            <wa_final_data>-ending_stock = <per_werks_matnr>-menge.
            <wa_final_data>-total_remaining_stock_ifrs_val = <per_werks_matnr>-dmbtr.
          WHEN 'SL' OR 'DF' OR 'OP' OR 'DS' OR 'ER'.  "Sales values
            <wa_final_data>-cogs_qty = <wa_final_data>-cogs_qty + ( <per_werks_matnr>-menge * -1 ).
            <wa_final_data>-cogs_ifrs_value = <wa_final_data>-cogs_ifrs_value + ( <per_werks_matnr>-dmbtr * -1 ).
            CASE <per_werks_matnr>-docucateg.
              WHEN 'SL'.  "Sales values
                <wa_final_data>-cogs_qty_sl = <per_werks_matnr>-menge.
                <wa_final_data>-cogs_ifrs_value_sl = <per_werks_matnr>-dmbtr.
              WHEN 'DF'.  "DEFICITS
                <wa_final_data>-cogs_qty_df = <per_werks_matnr>-menge.
                <wa_final_data>-cogs_ifrs_value_df = <per_werks_matnr>-dmbtr.
              WHEN 'OP'.  "SURPLUSES
                <wa_final_data>-cogs_qty_op = <per_werks_matnr>-menge.
                <wa_final_data>-cogs_ifrs_value_op = <per_werks_matnr>-dmbtr.
              WHEN 'DS'.  "DISTRUCTIONS
                <wa_final_data>-cogs_qty_ds = <per_werks_matnr>-menge.
                <wa_final_data>-cogs_ifrs_value_ds = <per_werks_matnr>-dmbtr.
              WHEN 'ER'.  "Equation Error
                <wa_final_data>-cogs_ifrs_value_er = <per_werks_matnr>-dmbtr.
            ENDCASE.
          WHEN 'RV'.  "Revenue amount
            <wa_final_data>-revenue = <per_werks_matnr>-dmbtr.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    " The GTT must be deleted after its use otherwise we will have a short dump.
    DELETE FROM zgtt_pd_material.
    "Delete the lines that COGS and RM quantities are zero because we don't want to split the Difference to this lines.
*    DELETE it_final_data WHERE cogs_qty = 0 AND ending_stock = 0.

    LOOP AT it_final_data ASSIGNING FIELD-SYMBOL(<ls_final_data>) GROUP BY ( matnr = <ls_final_data>-matnr
                                                                              size = GROUP SIZE
                                                                              index = GROUP INDEX
                                                                            )
                                                   ASCENDING
                                                   REFERENCE INTO DATA(matnr_group_final_data).
*      IF matnr_group_final_data->size = 1.
      LOOP AT GROUP matnr_group_final_data ASSIGNING FIELD-SYMBOL(<wa_matnr_group_final_data>).
        lv_group_tabix += 1.
        sum_ending_stock += <wa_matnr_group_final_data>-ending_stock.
        sum_cogs_qty += <wa_matnr_group_final_data>-cogs_qty.
        IF lv_group_tabix = matnr_group_final_data->size AND sum_ending_stock = 0 AND sum_cogs_qty = 0.
          MODIFY it_final_data FROM VALUE #( no_allocation = 'X' ) TRANSPORTING no_allocation WHERE matnr = <wa_matnr_group_final_data>-matnr.
        ENDIF.
*          IF "<wa_matnr_group_final_data>-cogs_ifrs_value_er = 0 AND <wa_matnr_group_final_data>-revenue = 0 AND
*             <wa_matnr_group_final_data>-cogs_qty = 0 AND <wa_matnr_group_final_data>-ending_stock = 0.
*            <wa_matnr_group_final_data>-no_allocation = 'X'.
*          ENDIF.
      ENDLOOP.
      CLEAR: sum_ending_stock, sum_cogs_qty, lv_group_tabix.
*      IF lv_rm_quantity = 0 AND lv_cogs_quantity = 0.
*        LOOP AT GROUP matnr_group_final_data ASSIGNING <wa_matnr_group_final_data>.
*          <wa_matnr_group_final_data>-no_allocation = 'X'.
*        ENDLOOP.
*      ENDIF.
*      ENDIF.
    ENDLOOP.

    sum_OVH_IFRS = REDUCE zovh_ifrs_value( INIT total_whb_ifrs TYPE zovh_ifrs_value FOR ls_final_data IN it_final_data WHERE ( no_allocation <> 'X' )
                                                                                          NEXT total_whb_ifrs = total_whb_ifrs + ls_final_data-ovh_ifrs_value ).
*    DATA(lv_ledgers_difference) = it_sum_acdoca_amounts[ 1 ]-diffs.
*    DATA(lv_1st_ledger_amount) = it_sum_acdoca_amounts[ 1 ]-hsl.

** In this LOOP we calculate the OVH IFRS Difference by putting the allocated difference between OVH IFRS & ACDOCA IFRS
* in order to reach ACDOCA IFRS. Also we are calculated the OVH DIFF IFRS TAX.
    LOOP AT it_final_data ASSIGNING <wa_final_data> WHERE no_allocation <> 'X'.
      <wa_final_data>-ovh_ifrs_diff_value = <wa_final_data>-ovh_ifrs_value * acdoca_ifrs_total / sum_OVH_IFRS.
      IF acdoca_ifrs_total <> 0.
        <wa_final_data>-ovh_diff_ifrs_tax_val = ( it_sum_acdoca_amounts[ 1 ]-diffs * <wa_final_data>-ovh_ifrs_diff_value ) / acdoca_ifrs_total.
      ENDIF.
    ENDLOOP.

** Here we are doing all the rest calculations that needed.
    LOOP AT it_final_data INTO DATA(group_final_data) GROUP BY ( bukrs = group_final_data-bukrs
                                                                 gjahr = group_final_data-gjahr
                                                                 matnr = group_final_data-matnr ).
** Summary of the ENDING STOCK, COGS QNTY & OVH DIFFERENCE IFRS TAX per BUKRS, GJAHR & MATNR.
      sum_ending_stock = REDUCE zending_stock( INIT total_whb_rm_qnty TYPE zending_stock
                                              FOR ls_group_final_data IN GROUP group_final_data
                                              NEXT total_whb_rm_qnty = total_whb_rm_qnty + ls_group_final_data-ending_stock ).
      sum_cogs_qty =     REDUCE zcogs_qty( INIT total_whb_cogs_qty TYPE zcogs_qty
                                              FOR ls_group_final_data IN GROUP group_final_data
                                              NEXT total_whb_cogs_qty = total_whb_cogs_qty + ls_group_final_data-cogs_qty ).
      sum_ovh_dif_ifrs_tax_per_matnr = REDUCE zovh_ifrs_value( INIT total_whb_ifrs TYPE zovh_ifrs_value
                                              FOR ls_group_final_data IN GROUP group_final_data
                                              NEXT total_whb_ifrs = total_whb_ifrs + ls_group_final_data-ovh_diff_ifrs_tax_val ).

      LOOP AT GROUP group_final_data ASSIGNING FIELD-SYMBOL(<group_final_data>).
        "Calculate the Difference of COGS & RM (STOCK) Values.
        <group_final_data>-annual_ovh_cost_tax_val = <group_final_data>-ovh_ifrs_diff_value + <group_final_data>-ovh_diff_ifrs_tax_val.
        lv_total_rm_cogs = sum_ending_stock + sum_cogs_qty.
        IF ( lv_total_rm_cogs ) <> 0.
          <group_final_data>-diff_of_stock_val = ( sum_ovh_dif_ifrs_tax_per_matnr * <group_final_data>-ending_stock ) / ( lv_total_rm_cogs ).
          <group_final_data>-diff_of_cogs_value = ( sum_ovh_dif_ifrs_tax_per_matnr * <group_final_data>-cogs_qty ) / ( lv_total_rm_cogs ).
        ELSE.
          IF ( abs( sum_ending_stock ) = abs( sum_cogs_qty ) ) AND ( sum_ending_stock <> 0 AND sum_cogs_qty <> 0 ).
            <group_final_data>-diff_of_stock_val = sum_ovh_dif_ifrs_tax_per_matnr / 2.
            <group_final_data>-diff_of_cogs_value = sum_ovh_dif_ifrs_tax_per_matnr / 2.
          ENDIF.
        ENDIF.
        <group_final_data>-total_remaining_stock_tax_val = <group_final_data>-total_remaining_stock_ifrs_val + <group_final_data>-diff_of_stock_val.

        <group_final_data>-cogs_tax_value = <group_final_data>-cogs_ifrs_value + <group_final_data>-diff_of_cogs_value.
        IF <group_final_data>-ending_stock <> 0.
          <group_final_data>-avg_tax_stock_value = <group_final_data>-total_remaining_stock_tax_val / <group_final_data>-ending_stock.
        ENDIF.
        IF <group_final_data>-cogs_qty <> 0.
          <group_final_data>-avg_tax_cogs_value = <group_final_data>-cogs_tax_value / <group_final_data>-cogs_qty.
        ENDIF.
        IF <group_final_data>-ending_stock <> 0.
          <group_final_data>-avg_rm_ifrs_value = <group_final_data>-total_remaining_stock_ifrs_val / <group_final_data>-ending_stock.
        ENDIF.
        IF <group_final_data>-cogs_qty <> 0.
          <group_final_data>-avg_cogs_ifrs_value = <group_final_data>-cogs_ifrs_value / <group_final_data>-cogs_qty.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF abs( sum_OVH_IFRS - acdoca_ifrs_total ) > 10.
      MESSAGE |There is a Difference more than 10 between OVH IFRS { sum_ovh_ifrs NUMBER = USER } and ACDOCA IFRS { acdoca_ifrs_total NUMBER = USER }.| TYPE 'I'.
    ENDIF.
  ENDMETHOD.

  METHOD display_error_messages.
    DATA(error_message) = lo_error_object->get_text( ).
    SORT it_error_messages BY lineno DESCENDING.
    DATA(lv_lineno) = VALUE #( it_error_messages[ 1 ]-lineno DEFAULT 1 ).
    DATA(remaining_length) = strlen( error_message ) - 50.
    IF remaining_length > 0.
      it_error_messages = VALUE #( BASE it_error_messages
              ( msgid = 'E4' msgno = '000'
                msgty = 'E'
                msgv1 = error_message(50)
                msgv2 = error_message+50(remaining_length)
                lineno = lv_lineno
              ) ).
    ELSE.
      it_error_messages = VALUE #( BASE it_error_messages
                ( msgid = 'E4' msgno = '000'
                  msgty = 'E'
                  msgv1 = error_message
                  lineno = lv_lineno
                ) ).
    ENDIF.
  ENDMETHOD.

  METHOD post_into_zfi_tax_mm_cost.

    SELECT SINGLE repid FROM j_1gcontrol
    WHERE repid = @current_report_name AND
          bukrs = @select_options_structure-p_rbukrs AND
          gjahr = @select_options_structure-p_gjahr
      INTO @DATA(lv_repid).
    IF sy-subrc <> 0.
      IF im_final_post = 'X'.
        MODIFY zfi_ledgers_diff FROM @( VALUE #( bukrs = select_options_structure-p_rbukrs
                                                 gjahr = select_options_structure-p_gjahr
                                                 ledger1 = select_options_structure-p_1rldnr
                                                 hsl1 = VALUE #( it_sum_acdoca_amounts[ 1 ]-hsl OPTIONAL )
                                                 ledger2 = select_options_structure-p_2rldnr
                                                 hsl2 = VALUE #( it_sum_acdoca_amounts[ 1 ]-hsl2 OPTIONAL )
                                                 difference = VALUE #( it_sum_acdoca_amounts[ 1 ]-diffs OPTIONAL )
                                                 ledger_3rd_party = select_options_structure-p_3rldnr
                                                 hsl_3rd_ifrs = VALUE #( it_sum_acdoca_amounts[ 1 ]-hsl_3rd_ifrs OPTIONAL )
                                                 hsl_3rd_tax = VALUE #( it_sum_acdoca_amounts[ 1 ]-hsl_3rd_tax OPTIONAL )
                                                 ) ).
        MODIFY zfi_tax_mm_cost FROM TABLE cht_final_data.

        IF sy-subrc = 0.
          INSERT INTO j_1gcontrol VALUES @( VALUE #( repid =  current_report_name
                                                     bukrs = select_options_structure-p_rbukrs
                                                     gjahr = select_options_structure-p_gjahr
                                                     budat = sy-datum
                                                    ) ).
          IF sy-subrc = 0.
            MESSAGE 'All tables have been updated' TYPE 'I'.
          ELSE.
            MESSAGE 'There was problem updating the J_1GCONTROL table' TYPE 'I'.
          ENDIF.  " Updating J_1GCONTROL
        ELSE.
          MESSAGE |The DB table has not been updated. | TYPE 'I'.
        ENDIF.  " Updating ZFI_TAX_MM_COST
      ELSE.
        MODIFY zfi_ledgers_diff FROM @( VALUE #( bukrs = select_options_structure-p_rbukrs
                                                 gjahr = select_options_structure-p_gjahr
                                                 ledger1 = select_options_structure-p_1rldnr
                                                 hsl1 = VALUE #( it_sum_acdoca_amounts[ 1 ]-hsl OPTIONAL )
                                                 ledger2 = select_options_structure-p_2rldnr
                                                 hsl2 = VALUE #( it_sum_acdoca_amounts[ 1 ]-hsl2 OPTIONAL )
                                                 difference = VALUE #( it_sum_acdoca_amounts[ 1 ]-diffs OPTIONAL ) ) ).
        MODIFY zfi_tax_mm_cost FROM TABLE cht_final_data.
        IF sy-subrc = 0.
          MESSAGE 'The DB tables have been updated.' TYPE 'I'.
        ELSE.
          MESSAGE 'The DB tables have not been updated.' TYPE 'I'.
        ENDIF.  " Updating J_1GCONTROL
      ENDIF.  " If it is Final Post
    ELSE.
      MESSAGE 'There is a final posting in J_1GCONTROL' TYPE 'I'.
      lo_alloc_ledgs_diffs_toprd_vw->disable_enable_toolbar_button(
              EXPORTING i_button_name = 'ZEDITTABLE'
                        i_isvisable   = abap_true
                         ).
    ENDIF.  " Check that there is a post in J_1GCONTROL

  ENDMETHOD.

  METHOD get_str_sum_acdoca_amounts.
    r_result = VALUE #( it_sum_acdoca_amounts[ 1 ] OPTIONAL ).
  ENDMETHOD.

  METHOD check_authorization.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
          ID 'BUKRS' FIELD select_options_structure-p_rbukrs
          ID 'ACTVT' FIELD '03'.

    IF sy-subrc <> 0.
      MESSAGE TEXT-006 TYPE 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD find_calculated_fields.
    "Variable TOTAL IFRS that is used in the calculation of the final amounts
    acdoca_ifrs_total = it_sum_acdoca_amounts[ 1 ]-hsl - it_sum_acdoca_amounts[ 1 ]-hsl_3rd_ifrs .
    "Find the 3rd Party TAX value in analogy between 3rd party IFRS and TAX.
    it_sum_acdoca_amounts[ 1 ]-hsl_3rd_tax = ( it_sum_acdoca_amounts[ 1 ]-hsl_3rd_ifrs *
                                               it_sum_acdoca_amounts[ 1 ]-hsl2 ) / it_sum_acdoca_amounts[ 1 ]-hsl.
    "The Difference between TAX and IFRS
    it_sum_acdoca_amounts[ 1 ]-diffs = ( it_sum_acdoca_amounts[ 1 ]-hsl2 - it_sum_acdoca_amounts[ 1 ]-hsl_3rd_tax ) -
                                       ( it_sum_acdoca_amounts[ 1 ]-hsl - it_sum_acdoca_amounts[ 1 ]-hsl_3rd_ifrs ).
  ENDMETHOD.

  METHOD edit_table_sm30.
    CONSTANTS: sm30_action            TYPE char1   VALUE 'U',
               sm30_display_selection TYPE char1   VALUE ' ',
               tobj_fiel1             TYPE char7   VALUE 'BUKRS',
               tobj_fiel2             TYPE char5   VALUE 'ACTVT',
               selection_operator     TYPE char2   VALUE 'EQ',
               sm30_viewname          TYPE tabname VALUE 'J_1GCONTROL'.
    DATA : message_part   TYPE string.
    DATA: sm30it_dba_sellist TYPE STANDARD TABLE OF vimsellist.
    DATA: sm30wa_dba_sellist TYPE vimsellist.

*authority check for warehouse number
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                ID tobj_fiel1 FIELD select_options_structure-p_rbukrs
                ID tobj_fiel2 FIELD '1'.
    IF sy-subrc NE 0.
* user not authorized
      MESSAGE TEXT-006 TYPE 'E'.
    ELSE.
*Clear Internal Table
      CLEAR sm30it_dba_sellist.
      CLEAR sm30wa_dba_sellist.

*passing the selection parameters to the function module
*view_maintenance_call.
      sm30wa_dba_sellist-viewfield = tobj_fiel1.
      sm30wa_dba_sellist-value     = select_options_structure-p_rbukrs.
      sm30wa_dba_sellist-operator  = selection_operator.
      APPEND sm30wa_dba_sellist TO sm30it_dba_sellist.

      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action                       = sm30_action            " Action (Display/Maintain/Transport(S/U/T))
          show_selection_popup         = sm30_display_selection " Flag: Display Selection Conditions Dialog Box
          view_name                    = sm30_viewname          " Name of the View/Table to be Edited
        TABLES
          dba_sellist                  = sm30it_dba_sellist  " Database Access Selection Conditions
        EXCEPTIONS
          client_reference             = 1                      " View is Chained to Another Client
          foreign_lock                 = 2                      " View/Table is Locked by Another User
          invalid_action               = 3                      " ACTION Contains Invalid Values
          no_clientindependent_auth    = 4                      " No Authorization for Maintaining Cross-Client Tables/Views
          no_database_function         = 5                      " No Data Capture/Disposal Function Module
          no_editor_function           = 6                      " Editor Function Module Missing
          no_show_auth                 = 7                      " No Display Authorization
          no_tvdir_entry               = 8                      " View/Table is Not in TVDIR
          no_upd_auth                  = 9                      " No Maintenance or Display Authorization
          only_show_allowed            = 10                     " Display but Not Maintenance Authorization
          system_failure               = 11                     " System Lock Error
          unknown_field_in_dba_sellist = 12                     " Selection Table Contains Unknown Fields
          view_not_found               = 13                     " View/Table Not Found in DDIC
          maintenance_prohibited       = 14                     " View/Table Cannot be Maintained acc. to DDIC
          OTHERS                       = 15.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        lo_alloc_ledgs_diffs_toprd_vw->disable_enable_toolbar_button(
              EXPORTING i_button_name = 'ZEDITTABLE'
                        i_isvisable   = abap_false
                         ).
      ENDIF.
    ENDIF.
*    p_butenb = ''.
  ENDMETHOD.

ENDCLASS.
