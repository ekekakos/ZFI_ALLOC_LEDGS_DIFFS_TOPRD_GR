*&---------------------------------------------------------------------*
*& Report ZFI_RLDNRS_AMOUNT_DIFF_GR
*&---------------------------------------------------------------------*
*&    DESCRIPTION
*&    The program will display a selection screen to the user which must be fulfilled.
*&  Then it starts the process of Finding, Prepare and Display the data following the below steps according to the selection criteria:
*&    1. It will sum the amounts of the 2 ledgers (IFRS & TAX) separately and will calculate the difference (TAX-IFRS) from the ACDOCA table .
*&    2. It will find the 3rd parties and the PM orders sum amounts.
*&    3. It will calculate the difference between TAX & IFRS by taking in account the 3rd parties and PM orders amounts.
*&    4. It will find the productive materials from the customizing.
*&    5. It will get the appropriate records from Hellenization table J_1GVL_WHB010.
*&    6. Finally, it will fill the final total table per Company Code (BKRS), Year(GJAHR), Plant(WERKS) and Material(MATNR).
*&  At the end, it will display the above ITAB to the user through the ALV.
*&  The user through the ALV has 2 options:
*&    I. To make the final post and
*&    II. To display old data from the tables ZFI_TAX_MM_COSt & ZFI_LEDGERS_DIFF.
*&---------------------------------------------------------------------*
*& Change log:
*& Date       Author        Action
*& 20200917  ekekakos     Program created
*&
*&---------------------------------------------------------------------*
REPORT zfi_alloc_ledgs_diffs_toprd_gr.

TABLES: acdoca, aufk, mara, mbew.

************************************************************************
* SELECTION SCREENS                                                    *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF BLOCK company WITH FRAME TITLE TEXT-003.
    PARAMETERS: p_rbukrs TYPE bukrs OBLIGATORY,
                p_1rldnr TYPE rldnr OBLIGATORY DEFAULT '1L',  "Calendar Local Ledger
                p_2rldnr TYPE rldnr OBLIGATORY DEFAULT 'TC',  "Calendar Tax Ledger
                p_rrcty  TYPE rrcty OBLIGATORY DEFAULT '0',   "Actual
                p_gjahr  TYPE gjahr OBLIGATORY.
    SELECT-OPTIONS: s_budat  FOR acdoca-budat OBLIGATORY,
                    s_racct  FOR acdoca-racct,
                    s_rcntr  FOR acdoca-rcntr,
                    s_bttyp  FOR acdoca-bttype,
                    saccasty FOR acdoca-co_accasty_n1,
                    s_rfarea FOR acdoca-rfarea,
                    s_sfarea FOR acdoca-sfarea.
  SELECTION-SCREEN END OF BLOCK company.

  SELECTION-SCREEN BEGIN OF BLOCK 3rd_party WITH FRAME TITLE TEXT-004.
    PARAMETERS: p_3rldnr TYPE rldnr OBLIGATORY DEFAULT '0L'.
    SELECT-OPTIONS: s_bttype FOR acdoca-bttype OBLIGATORY,
                    s_awtyp  FOR acdoca-awtyp OBLIGATORY,
                    s_autyp  FOR acdoca-autyp OBLIGATORY DEFAULT 10,
                    s_3racct FOR acdoca-racct DEFAULT 994300001 TO 994309999.
  SELECTION-SCREEN END OF BLOCK 3rd_party.
  SELECTION-SCREEN BEGIN OF BLOCK whbook WITH FRAME TITLE TEXT-005.
    SELECT-OPTIONS: s_mtart  FOR mara-mtart,
                    s_bklas  FOR mbew-bklas.
  SELECTION-SCREEN END OF BLOCK whbook.
SELECTION-SCREEN END OF BLOCK selection.
PARAMETERS: p_layout TYPE disvariant-variant.
************************************************************************
* SELECTION SCREENS - END                                              *
************************************************************************

************************************************************************
* EVENTS                                                               *
************************************************************************
INITIALIZATION.
  zcl_custom_salv=>get_default_layout(
    EXPORTING
      i_restrict    = if_salv_c_layout=>restrict_none
      i_report_name = sy-repid
    CHANGING
      c_layout      = p_layout
  ).

  APPEND VALUE #( sign = 'E' option = 'EQ' low = 'COFI' ) TO s_bttyp .
  APPEND VALUE #( sign = 'E' option = 'EQ' low = 'KS' ) TO saccasty .

  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'ZFG' ) TO s_mtart .
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'ZFGS' ) TO s_mtart .

  APPEND VALUE #( sign = 'I' option = 'EQ' low = '1000' ) TO s_bklas .
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '1020' ) TO s_bklas .

  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'RKLN' ) TO s_bttype .
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'RMRU' ) TO s_bttype .

  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'AFRU' ) TO s_awtyp .
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'COBK' ) TO s_awtyp .

AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  zcl_custom_salv=>layout_f4(
    EXPORTING
      i_report_name = sy-repid
      i_restrict    = if_salv_c_layout=>restrict_none
    CHANGING
      c_layout      = p_layout
  ).
*----------------------------------------------------------------------*
* START-OF-SELECTION event                                             *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA(o_alloc_ledgs_difs_toprd_mdl) = NEW zcl_alloc_ledgs_difs_toprd_mdl( current_report = sy-repid ).
  DATA(o_alloc_ledgs_diffs_toprd_vw) = NEW zcl_alloc_ledgs_diffs_toprd_vw( o_alloc_ledgs_difs_toprd_mdl ).
  o_alloc_ledgs_difs_toprd_mdl->set_alloc_ledgs_difs_toprd_vw( imo_alloc_ledgs_difs_toprd_vw = o_alloc_ledgs_diffs_toprd_vw ).

  o_alloc_ledgs_difs_toprd_mdl->check_authorization( ).
  DATA(it_final_data) = o_alloc_ledgs_difs_toprd_mdl->find_prepare_get_data( ).

  IF o_alloc_ledgs_difs_toprd_mdl->it_error_messages IS NOT INITIAL.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = o_alloc_ledgs_difs_toprd_mdl->it_error_messages.
  ENDIF.

  CHECK it_final_data IS NOT INITIAL.

  o_alloc_ledgs_diffs_toprd_vw->initialize_salv(
    EXPORTING
      im_report_name = sy-repid
    CHANGING
      ch_table = it_final_data
  ).
  o_alloc_ledgs_diffs_toprd_vw->customize_salv( ).
  o_alloc_ledgs_diffs_toprd_vw->display( ).
