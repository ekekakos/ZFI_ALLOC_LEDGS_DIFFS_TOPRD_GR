# ZFI_ALLOC_LEDGS_DIFFS_TOPRD_GR

# DESCRIPTION
  The program will display a selection screen to the user which must be fulfilled.  
  Then it starts the process of Finding, Prepare and Display the data following the below steps according to the selection criteria:  
    1. It will sum the amounts of the 2 ledgers (IFRS & TAX) separately and will calculate the difference (TAX-IFRS) from the ACDOCA table .  
    2. It will find the 3rd parties and the PM orders sum amounts.  
    3. It will calculate the difference between TAX & IFRS by taking in account the 3rd parties and PM orders amounts.  
    4. It will find the productive materials from the customizing.  
    5. It will get the appropriate records from Hellenization table J_1GVL_WHB010.  
    6. Finally, it will fill the final total table per Company Code (BKRS), Year(GJAHR), Plant(WERKS) and Material(MATNR).  
  At the end, it will display the above ITAB to the user through the ALV.  
  The user through the ALV has 2 options:  
    I. To make the final post and  
    II. To display old data from the tables ZFI_TAX_MM_COSt & ZFI_LEDGERS_DIFF.  
