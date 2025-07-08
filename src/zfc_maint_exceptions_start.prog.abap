*&---------------------------------------------------------------------*
*& Report zfc_maint_exceptions_start
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfc_maint_exceptions_start.

CLASS controller DEFINITION.

  PUBLIC SECTION.
    METHODS: start.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    FINAL(app) = NEW /ui2/cl_start_url( iv_icf_node_path = `/sap/bc/ui5_ui5/sap/z2ui5/` ).

    app->add_url_param( `app_start=zcl_fc_maint_exceptions_app`).
    app->add_url_param( `sap-client=` && sy-mandt ).

    app->start_browser( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->start( ).
