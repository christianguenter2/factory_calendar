CLASS zcl_fc_maint_exceptions_model DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_calendar_id TYPE tfain-ident,

      existence_check
        RETURNING
          VALUE(result) TYPE abap_bool,

      get_description
        RETURNING
          VALUE(result) TYPE string.

  PRIVATE SECTION.
    DATA calendar_id TYPE wfcid.

ENDCLASS.


CLASS zcl_fc_maint_exceptions_model IMPLEMENTATION.

  METHOD constructor.

    calendar_id = i_calendar_id.

  ENDMETHOD.


  METHOD existence_check.

    TRY.
        cl_scal_factorycalendar=>create_instance( calendar_id ).
      CATCH cx_fhc_runtime.
        RETURN.
    ENDTRY.

    result = abap_true.

  ENDMETHOD.


  METHOD get_description.

    result = cl_scal_factorycalendar=>create_instance( calendar_id )->if_fhc_fcal_runtime~get_description(
*          EXPORTING
*            iv_language    = SY-LANGU
*          RECEIVING
*            rv_description =
    )..
  ENDMETHOD.

ENDCLASS.
