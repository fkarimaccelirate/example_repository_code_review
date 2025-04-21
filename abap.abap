*&---------------------------------------------------------------------*
*& Report Z_BAD_STANDARD_REPORT
*&---------------------------------------------------------------------*
*& Author: A. Developer (Illustrative)
*& Date:   2025-04-21
*&
*& Description: A very basic report demonstrating poor practices
*&              and potential bugs. DO NOT USE AS EXAMPLE!
*&---------------------------------------------------------------------*
REPORT z_bad_standard_report.

* --- Global Data Declarations (Poor Naming, Lack of Structure) ---
DATA: itab TYPE TABLE OF mara, "Internal table for material data (using standard table directly is often discouraged for structure def)
      wa LIKE LINE OF itab,    "Work area, unclear name 'wa'
      counter TYPE i,          "Generic counter name
      some_value TYPE p DECIMALS 2, "Unclear purpose
      flag TYPE c.              "Single character flag, unclear meaning

* --- Selection Screen (Minimal, No Descriptions) ---
PARAMETERS: p_matnr TYPE matnr. "Parameter without description or checks

* --- START-OF-SELECTION Event ---
START-OF-SELECTION.

  "Select data - Using SELECT * is inefficient
  SELECT *
    FROM mara
    INTO TABLE itab
    UP TO 50 ROWS "Arbitrary limit
   WHERE matnr GE p_matnr. "Might select way too much data depending on input

  "Missing Check: What if no data is selected?
  "IF sy-subrc <> 0 OR itab IS INITIAL.
  "  MESSAGE 'No data found!' TYPE 'I'.
  "  LEAVE LIST-PROCESSING.
  "ENDIF.

  "Process the data
  LOOP AT itab INTO wa.
    counter = counter + 1. "Counter might not be initialized (implicit 0, but bad practice)

    "Perform some calculation (potential bug: division by zero if counter starts at 0 or somehow becomes 0)
    "Also, using SY-TABIX directly in complex calculations can be risky
    IF counter > 0. "Attempt to prevent division by zero, but maybe not robust
       some_value = sy-tabix / counter.
    ENDIF.

    "Modify table inside loop - Risky! This example is trivial, but deleting/inserting can break loop logic
    wa-mtart = 'ZBUG'. "Changing data in work area, but not updating the table itself unless MODIFY used
    "MODIFY itab FROM wa INDEX sy-tabix. "<- If this was added, it modifies the table being looped over

    "Write output directly - Poor Formatting, Magic Numbers
    WRITE: / wa-matnr, "Slash for new line is okay, but formatting is basic
             wa-ersda,
             wa-mtart,
             some_value, "Value might be unexpected due to calculation issues
             counter.

     "Bug: Forgetting to clear the work area can cause issues in complex scenarios
     "CLEAR wa. " <-- Missing! If loop logic relied on clean state, previous values could persist.

     "Unclear flag logic - What does 'X' mean?
     IF wa-mstae = '01'.
       flag = 'X'.
     ENDIF. "No ELSE to clear the flag? It stays 'X' once set.

  ENDLOOP.

  ULINE. "Basic line separator

  "Output based on unclear flag
  IF flag = 'X'.
    WRITE: / 'Flag was set at some point.'.
  ENDIF.

* --- END-OF-SELECTION ---
END-OF-SELECTION.

* --- Subroutine (Example of poor modularization if logic was complex) ---
* Usually more complex logic would go here, but keeping it simple.
* Could potentially access global variables directly (bad practice).
FORM process_something.
  " This form is currently unused, but demonstrates structure.
  " If it modified global variables like 'counter' or 'some_value'
  " without parameters, it would be poor practice.
ENDFORM.
