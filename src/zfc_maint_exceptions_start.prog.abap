*&---------------------------------------------------------------------*
*& Report zfc_maint_exceptions_start
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfc_maint_exceptions_start.

PARAMETERS:
  fiori    TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X',
  abap2ui5 TYPE abap_bool RADIOBUTTON GROUP r1.

CLASS controller DEFINITION.

  PUBLIC SECTION.
    METHODS: start.
  PRIVATE SECTION.
    METHODS call_browser
      IMPORTING
        i_url TYPE string.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    FINAL(path) = COND #(
                      WHEN fiori = abap_true THEN `/sap/bc/ui5_ui5/sap/zfc_exceptions/`
                      WHEN abap2ui5 = abap_true THEN `/sap/bc/ui5_ui5/sap/z2ui5/` ).

    cl_http_server=>if_http_server~get_location(
      EXPORTING
        application  = path
        protocol     = 'https'
      IMPORTING
        host         = FINAL(host_with_domain)
        port         = FINAL(port)
        out_protocol = FINAL(protocol) ).

    FINAL(url) = |{ protocol }://{ host_with_domain }:{ port }{ path }?app_start=zcl_fc_maint_exceptions_app&sap-client={ sy-mandt }|.

    call_browser( url ).

  ENDMETHOD.


  METHOD call_browser.

    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = i_url
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->start( ).
