CLASS zcl_fc_maint_exceptions_model DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_exception,
        ident TYPE tfain-ident,
        jahr  TYPE tfain-jahr,
        von   TYPE tfain-von,
        bis   TYPE tfain-bis,
        wert  TYPE tfain-wert,
        spra  TYPE tfait-spra,
        ltext TYPE tfait-ltext,
      END OF t_exception,
      tt_exception TYPE STANDARD TABLE OF t_exception
                        WITH NON-UNIQUE DEFAULT KEY.

    CLASS-METHODS:
      lock
        RAISING
          zcx_fc_error,

      unlock.

    METHODS:
      constructor
        IMPORTING
          i_calendar_id TYPE tfain-ident
          i_year        TYPE tfain-jahr OPTIONAL,

      existence_check
        RAISING
          zcx_fc_error,

      validate
        RAISING
          zcx_fc_error,

      get_description
        RETURNING
          VALUE(result) TYPE string,

      retrieve_exceptions
        RETURNING
          VALUE(result) TYPE tt_exception,

      save_exceptions
        IMPORTING
          i_exceptions TYPE tt_exception
        RAISING
          zcx_fc_error.

  PRIVATE SECTION.
    TYPES:
      tt_tfain TYPE STANDARD TABLE OF tfain
                    WITH NON-UNIQUE DEFAULT KEY,
      tt_tfait TYPE STANDARD TABLE OF tfait
                    WITH NON-UNIQUE DEFAULT KEY.

    DATA calendar_id TYPE tfain-ident.
    DATA year TYPE tfain-jahr.

ENDCLASS.


CLASS zcl_fc_maint_exceptions_model IMPLEMENTATION.

  METHOD constructor.

    calendar_id = i_calendar_id.
    year = i_year.

  ENDMETHOD.


  METHOD existence_check.

    TRY.
        cl_scal_factorycalendar=>create_instance( calendar_id ).

      CATCH cx_fhc_runtime.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e001(zfc_maint) WITH calendar_id.
    ENDTRY.

  ENDMETHOD.


  METHOD get_description.

    TRY.
        result = cl_scal_factorycalendar=>create_instance( calendar_id )->if_fhc_fcal_runtime~get_description( ).
      CATCH cx_fhc_runtime.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD retrieve_exceptions.

    SELECT
      FROM tfain
      LEFT OUTER JOIN tfait ON tfain~ident = tfait~ident
                            AND tfain~jahr = tfait~jahr
                            AND tfain~von  = tfait~von
                            AND tfait~spra = @sy-langu
      FIELDS tfain~*, tfait~spra, tfait~ltext
      WHERE tfain~ident = @calendar_id
      AND   tfain~jahr  = @year
      ORDER BY
        tfain~ident,
        tfain~jahr,
        tfain~von
      INTO CORRESPONDING FIELDS OF TABLE @result.

  ENDMETHOD.


  METHOD validate.

    IF year < sy-datum+0(4).
      RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e003(zfc_maint).
    ENDIF.

    SELECT
      SINGLE @abap_true AS valid
      FROM tfacd
      WHERE ident = @calendar_id
      AND   vjahr <= @year
      AND   bjahr >= @year
      INTO @FINAL(valid).
    IF valid = abap_false.
      RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e002(zfc_maint) WITH calendar_id year.
    ENDIF.

  ENDMETHOD.


  METHOD save_exceptions.

    DATA:
      tfain_inserts TYPE tt_tfain,
      tfain_updates TYPE tt_tfain,
      tfain_deletes TYPE tt_tfain,
      tfain         TYPE LINE OF tt_tfain,
      tfain_db      TYPE tt_tfain,

      tfait_inserts TYPE tt_tfait,
      tfait_updates TYPE tt_tfait,
      tfait_deletes TYPE tt_tfait,
      tfait         TYPE LINE OF tt_tfait,
      tfait_db      TYPE tt_tfait.

    existence_check( ).
    validate( ).

    SELECT
      FROM tfain
      FIELDS *
      WHERE ident = @calendar_id
      AND jahr = @year
      INTO TABLE @tfain_db.

    SELECT
      FROM tfait
      FIELDS *
      WHERE spra  = @sy-langu
      AND   ident = @calendar_id
      AND   jahr  = @year
      INTO TABLE @tfait_db.

    LOOP AT i_exceptions ASSIGNING FIELD-SYMBOL(<exception>).

      IF <exception>-von IS INITIAL.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e005(zfc_maint).
      ENDIF.

      IF <exception>-bis IS INITIAL.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e006(zfc_maint).
      ENDIF.

      IF <exception>-von(4) <> year.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e007(zfc_maint) WITH |{ <exception>-von DATE = USER }| year.
      ENDIF.

      IF <exception>-bis(4) <> year.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e007(zfc_maint) WITH |{ <exception>-bis DATE = USER }| year.
      ENDIF.

      IF <exception>-von > <exception>-bis.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e008(zfc_maint).
      ENDIF.

      tfain = CORRESPONDING #(  <exception> ).
      tfain-ident = calendar_id.
      tfain-jahr  = year.

      IF line_exists( tfain_db[ ident = tfain-ident
                                jahr  = tfain-jahr
                                von   = tfain-von
                                bis   = tfain-bis ] ).
        tfain_updates = VALUE #( BASE tfain_updates ( tfain ) ).
      ELSE.
        tfain_inserts = VALUE #( BASE tfain_inserts ( tfain ) ).
      ENDIF.

      tfait = CORRESPONDING #( <exception> ).
      tfait-spra  = sy-langu.
      tfait-ident = calendar_id.
      tfait-jahr  = year.

      IF line_exists( tfait_db[ spra  = tfait-spra
                                ident = tfait-ident
                                jahr  = tfait-jahr
                                von   = tfait-von ] ).
        tfait_updates = VALUE #( BASE tfait_updates ( tfait ) ).
      ELSE.
        tfait_inserts = VALUE #( BASE tfait_inserts ( tfait ) ).
      ENDIF.

    ENDLOOP.

    LOOP AT tfain_db ASSIGNING FIELD-SYMBOL(<tfain>).

      IF NOT line_exists( i_exceptions[ von   = <tfain>-von
                                        bis   = <tfain>-bis ] ).
        tfain_deletes = VALUE #( BASE tfain_deletes ( <tfain> ) ).
      ENDIF.

    ENDLOOP.

    LOOP AT tfait_db ASSIGNING FIELD-SYMBOL(<tfait>).

      IF NOT line_exists( i_exceptions[ von   = <tfait>-von ] ).
        tfait_deletes = VALUE #( BASE tfait_deletes ( <tfait> ) ).
      ENDIF.
    ENDLOOP.

    IF lines( tfain_deletes ) > 0.
      DELETE tfain FROM TABLE tfain_deletes.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e004(zfc_maint) WITH |DELETE|.
      ENDIF.
    ENDIF.

    IF lines( tfain_updates ) > 0.
      UPDATE tfain FROM TABLE tfain_updates.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e004(zfc_maint) WITH |UPDATE|.
      ENDIF.
    ENDIF.

    IF lines( tfain_inserts ) > 0.
      INSERT tfain FROM TABLE tfain_inserts.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e004(zfc_maint) WITH |INSERT|.
      ENDIF.
    ENDIF.

    IF lines( tfait_deletes ) > 0.
      DELETE tfait FROM TABLE tfait_deletes.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e004(zfc_maint) WITH |DELETE|.
      ENDIF.
    ENDIF.

    IF lines( tfait_updates ) > 0.
      UPDATE tfait FROM TABLE tfait_updates.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e004(zfc_maint) WITH |UPDATE|.
      ENDIF.
    ENDIF.

    IF lines( tfait_inserts ) > 0.
      INSERT tfait FROM TABLE tfait_inserts.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_fc_error MESSAGE e004(zfc_maint) WITH |INSERT|.
      ENDIF.
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD lock.

    CALL FUNCTION 'ENQUEUE_ESSCAL'
      EXPORTING
        tcode          = 'SCAL'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_fc_error USING MESSAGE.
    ENDIF.

  ENDMETHOD.


  METHOD unlock.

    CALL FUNCTION 'DEQUEUE_ESSCAL'
      EXPORTING
        mode_tstc = 'E'
        tcode     = 'SCAL'.

  ENDMETHOD.

ENDCLASS.
