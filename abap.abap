REPORT z_bad_standard_report.

DATA: itab TYPE TABLE OF mara,
      wa LIKE LINE OF itab,
      counter TYPE i,
      some_value TYPE p DECIMALS 2,
      flag TYPE c.

PARAMETERS: p_matnr TYPE matnr.

START-OF-SELECTION.

  SELECT *
    FROM mara
    INTO TABLE itab
    UP TO 50 ROWS
   WHERE matnr GE p_matnr.

  LOOP AT itab INTO wa.
    counter = counter + 1.

    IF counter > 0.
       some_value = sy-tabix / counter.
    ENDIF.

    wa-mtart = 'ZBUG'.

    WRITE: / wa-matnr,
             wa-ersda,
             wa-mtart,
             some_value,
             counter.

     IF wa-mstae = '01'.
       flag = 'X'.
     ENDIF.

  ENDLOOP.

  ULINE.

  IF flag = 'X'.
    WRITE: / 'Flag was set at some point.'.
  ENDIF.

END-OF-SELECTION.

FORM process_something.
ENDFORM.
