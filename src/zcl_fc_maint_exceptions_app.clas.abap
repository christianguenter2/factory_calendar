CLASS zcl_fc_maint_exceptions_app DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      z2ui5_if_app.
    TYPES:
      BEGIN OF t_exception,
        selkz TYPE abap_bool,
        von   TYPE string,
        bis   TYPE string,
        wert  TYPE abap_bool,
        ltext TYPE tfait-ltext,
      END OF t_exception.

    DATA:
      editable        TYPE abap_bool,

      sales_area      TYPE zws_de_supplier,
      calendarid      TYPE tfain-ident,
      year            TYPE c LENGTH 4, " tfain-jahr.
      exceptions      TYPE STANDARD TABLE OF t_exception WITH NON-UNIQUE DEFAULT KEY,

      placeholder_von TYPE string,
      placeholder_bis TYPE string,

      BEGIN OF message,
        type    TYPE string,
        text    TYPE string,
        visible TYPE abap_bool,
      END OF message,
      calendartext      TYPE string,
      salesarea_text    TYPE string,
      header_valid      TYPE abap_bool,
      check_initialized TYPE abap_bool READ-ONLY,

      dirty             TYPE abap_bool,

      scrollupdate      TYPE abap_bool,
      scroll_values     TYPE z2ui5_if_types=>ty_t_name_value.

  PRIVATE SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    DATA save_event TYPE string.
    DATA f4_active TYPE abap_bool.

    METHODS:
      render,

      go
        IMPORTING
          i_check_mandatory_fields TYPE abap_bool DEFAULT abap_true,

      save,

      date_in

        IMPORTING
          i_date        TYPE string
        RETURNING
          VALUE(result) TYPE datum,

      edit,

      detect_changes,

      retrieve_exceptions
        RETURNING
          VALUE(result) LIKE exceptions,

      confirm_dataloss_if_dirty
        IMPORTING
          i_event           TYPE csequence
        RETURNING
          VALUE(r_continue) TYPE abap_bool,

      ui5_callback,

      dispatch
        IMPORTING
          i_event TYPE string,

      get_text_for
        IMPORTING
          i_field       TYPE any
        RETURNING
          VALUE(result) TYPE text,

      date_out
        IMPORTING
          i_date        TYPE datum
        RETURNING
          VALUE(result) TYPE string,

      on_f4,

      init,

      scroll_to_top,

      get_model
        RETURNING VALUE(result) TYPE REF TO zcl_fc_maint_exceptions_model,

      handle_error
        IMPORTING
          i_error TYPE REF TO cx_root.

ENDCLASS.



CLASS zcl_fc_maint_exceptions_app IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->get( )-check_on_navigated = abap_true.
      ui5_callback( ).
    ENDIF.

    IF check_initialized = abap_false.
      check_initialized = abap_true.
      render( ).
    ENDIF.

    detect_changes( ).
    dispatch( client->get( )-event ).

    IF message-visible = abap_true.
      scroll_to_top( ).
    ENDIF.

    client->view_model_update( ).

  ENDMETHOD.


  METHOD render.

    DATA: view TYPE REF TO z2ui5_cl_xml_view.

    zcl_fc_maint_exceptions_model=>unlock( ).

    client->set_session_stateful( abap_true ).

    view = COND #(
             WHEN client->get( )-check_launchpad_active = abap_true
             THEN z2ui5_cl_xml_view=>factory( )
             ELSE z2ui5_cl_xml_view=>factory( )->shell( ) ).

    message-type = `Information`.
    message-visible = abap_false.

    FINAL(page) = view->page(
                    id             = `page`
                    title          = 'Factory Calendar - Maintain Exceptions'(002)
                    navbuttonpress = client->_event( 'BACK' )
                    class          = `sapUiContentPadding`
                    shownavbutton  = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL ) ).

    page->message_strip(
        type     = client->_bind( message-type )
        text     = client->_bind( message-text )
        visible  = client->_bind( message-visible )
        showicon = abap_true
        class    = `sapUiSmallMarginBottom`
    ).

    page->horizontal_layout(
       )->simple_form( editable = abap_true
       )->content( `form`
       )->label( text     = get_text_for( sales_area )
                 labelfor = `inputSalesArea`
                 required = abap_true
       )->input( id               = `inputSalesArea`
                 value            = client->_bind_edit( sales_area )
                 maxlength        = '4'
                 submit           = client->_event( `GO` )
                 valuehelponly    = abap_true
                 showvaluehelp    = abap_true
                 valuehelprequest = client->_event( `F4` )
       )->text( text = client->_bind( salesarea_text )
       )->label( text     = get_text_for( calendarid )
                 labelfor = `inputCalId`
                 required = abap_true
       )->input( id            = `inputCalId`
                 value         = client->_bind_edit( calendarid )
                 maxlength     = '2'
                 submit        = client->_event( `GO` )
                 valuehelponly = abap_true
                 showvaluehelp = abap_true
                 valuehelprequest = client->_event( `F4` )
       )->text( text = client->_bind( calendartext )
       )->label( text     = get_text_for( VALUE jahr( ) )
                 labelfor = `inputYear`
                 required = abap_true
       )->input( id        = `inputYear`
                 value     = client->_bind_edit( year )
                 maxlength = '4'
                 submit    = client->_event( `GO` )
       )->button(
         text  = `Go`
         type  = `Emphasized`
         press = client->_event( `GO` )
       ).

    FINAL(table) = page->horizontal_layout(
                      )->table(
                        items   = client->_bind_edit( exceptions )
                        mode    = 'MultiSelect'
                        class   = `sapUiSmallMarginTop`
                        visible = client->_bind( header_valid ) ).

    table->header_toolbar(
        )->overflow_toolbar(
        )->button(
            text    = 'Edit'(004)
            press   = client->_event( 'BUTTON_EDIT' )
            icon    = 'sap-icon://edit'
            enabled = client->_bind( header_valid )
        )->button(
            icon    = 'sap-icon://add'
            text    = 'add'(001)
            press   = client->_event( 'BUTTON_ADD' )
            enabled = client->_bind( editable )
        )->button(
            icon    = 'sap-icon://delete'
            text    = 'delete'(003)
            press   = client->_event( 'BUTTON_DELETE' )
            enabled = client->_bind( editable ) ).

    table->columns(
        )->column( )->text( get_text_for( VALUE tfain-von( ) ) )->get_parent(
        )->column( )->text( get_text_for( VALUE tfain-bis( ) ) )->get_parent(
        )->column( )->text( get_text_for( VALUE arbtag( ) ) )->get_parent(
        )->column( )->text( get_text_for( VALUE tfait-ltext( ) ) ).

    table->items( )->column_list_item( selected = `{SELKZ}`
        )->cells(
            )->date_picker( editable = client->_bind( editable ) value = `{VON}` placeholder = client->_bind( placeholder_von ) required = abap_true
            )->date_picker( editable = client->_bind( editable ) value = `{BIS}` placeholder = client->_bind( placeholder_bis ) required = abap_true
            )->checkbox( editable = client->_bind( editable ) selected = `{WERT}`
            )->input(  editable = client->_bind( editable ) value = `{LTEXT}` ).

    page->footer( )->overflow_toolbar(
                   )->toolbar_spacer(
                   )->button(
                       enabled = client->_bind( editable )
                       text    = 'Save'(005)
                       press   = client->_event( 'BUTTON_SAVE' )
                       icon    = 'sap-icon://save'
                       type    = 'Success' ).

    page->_z2ui5( )->dirty( client->_bind( dirty ) ).

    page->_z2ui5( )->scrolling( setupdate = client->_bind_edit( scrollupdate )
                                items     = client->_bind_edit( scroll_values ) ).

    client->view_display( page->stringify( ) ).

  ENDMETHOD.


  METHOD go.

    init( ).

    calendarid = to_upper( calendarid ).

    FINAL(factory_calendar) = get_model( ).

    TRY.
        factory_calendar->existence_check( ).
        factory_calendar->check_display_allowed( ).
        calendartext = factory_calendar->get_description( ).
        salesarea_text = factory_calendar->get_salesarea_text( ).

        IF year IS NOT INITIAL.
          factory_calendar->validate( ).
          header_valid = abap_true.
          exceptions = retrieve_exceptions( ).

          placeholder_von = |{ date_out( |{ year }0101| ) }|.
          placeholder_bis = |{ date_out( |{ year }0131| ) }|.
        ENDIF.

      CATCH zcx_fc_error INTO FINAL(error).
        handle_error( error ).
    ENDTRY.

    IF i_check_mandatory_fields = abap_true.
      IF calendarid IS INITIAL
      OR year IS INITIAL
      OR sales_area IS INITIAL.
        message-visible = abap_true.
        message-type = `Error`.
        message-text = 'Please enter mandatory fields'(009).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD save.

    CLEAR:
      message-text,
      message-visible.

    FINAL(factory_calendar) = get_model( ).

    TRY.
        factory_calendar->validate( ).
        factory_calendar->save_exceptions(
          VALUE #( FOR exception IN exceptions
                   ( von   = date_in( exception-von )
                     bis   = date_in( exception-bis )
                     ltext = exception-ltext
                     wert  = SWITCH #( exception-wert
                               WHEN abap_true  THEN '1'
                               WHEN abap_false THEN '0' ) ) ) ).

      CATCH zcx_fc_error INTO FINAL(error).
        message-visible = abap_true.
        message-type = `Error`.
        message-text = error->get_text( ).
        RETURN.
    ENDTRY.

    editable = abap_false.
    zcl_fc_maint_exceptions_model=>unlock( ).

    message-visible = abap_true.
    message-type = `Success`.
    message-text = 'Data saved'(008).

  ENDMETHOD.


  METHOD date_in.

    CALL FUNCTION 'CONVERSION_EXIT_IDATE_INPUT'
      EXPORTING
        input  = i_date
      IMPORTING
        output = result.

  ENDMETHOD.


  METHOD edit.

    TRY.
        FINAL(factory_calendar) = get_model( ).

        IF editable = abap_true.

          factory_calendar->unlock( ).
          go( ).
          editable = abap_false.

        ELSE.

          factory_calendar->existence_check( ).
          factory_calendar->check_edit_allowed( ).
          factory_calendar->lock( ).
          editable = abap_true.

        ENDIF.

      CATCH zcx_fc_error INTO FINAL(error).
        handle_error( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD detect_changes.

    IF year IS INITIAL
    OR header_valid = abap_false.
      RETURN.
    ENDIF.

    IF retrieve_exceptions( ) <> exceptions .
      dirty = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD retrieve_exceptions.

    IF year IS INITIAL.
      RETURN.
    ENDIF.

    result = VALUE #(
               FOR exception IN get_model( )->retrieve_exceptions( )
               (
                 von   = |{ exception-von DATE = USER }|
                 bis   = |{ exception-bis DATE = USER }|
                 ltext = exception-ltext
                 wert  = xsdbool( exception-wert = '1' )
               ) ).

  ENDMETHOD.


  METHOD confirm_dataloss_if_dirty.

    r_continue = abap_true.

    IF save_event IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF  dirty = abap_true
    AND editable = abap_true.
      r_continue = abap_false.
      save_event = i_event.
      client->nav_app_call( z2ui5_cl_pop_to_confirm=>factory(
                                i_icon          = 'sap-icon://warning'
                                i_question_text = |{ 'Unsaved data will be lost. Continue?'(006) }|
                                i_title         = |{ 'Unsaved data'(007) }| ) ).
    ENDIF.

  ENDMETHOD.


  METHOD ui5_callback.

    TRY.
        FINAL(prev) = client->get_app( client->get( )-s_draft-id_prev_app ).

        IF f4_active = abap_true.

          CLEAR: f4_active.

          FINAL(f4_app) = CAST z2ui5_cl_tool_app_shlp_gen( prev ).

          IF f4_app->mv_shlp_result IS NOT INITIAL.
            sales_area = f4_app->mv_shlp_result.
            calendarid = f4_app->mv_shlp_result2.
            go( i_check_mandatory_fields = abap_false ).
          ENDIF.

        ENDIF.

        IF save_event IS NOT INITIAL.
          FINAL(confirm_result) = CAST z2ui5_cl_pop_to_confirm( prev )->result( ).

          IF confirm_result = abap_true.
            dispatch( save_event ).
          ENDIF.

          CLEAR: save_event.

        ENDIF.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD dispatch.

    CASE i_event.
      WHEN 'BACK'.
        IF confirm_dataloss_if_dirty( i_event ).
          client->set_session_stateful( abap_false ).
          client->nav_app_leave( ).
        ENDIF.
      WHEN 'GO'.
        IF confirm_dataloss_if_dirty( i_event ).
          go( ).
        ENDIF.
      WHEN 'BUTTON_DELETE'.
        ASSERT editable = abap_true.
        DELETE exceptions WHERE selkz = abap_true.
        IF sy-subrc = 0.
          dirty = abap_true.
        ENDIF.
      WHEN 'BUTTON_ADD'.
        ASSERT editable = abap_true.
        INSERT INITIAL LINE INTO TABLE exceptions.
        dirty = abap_true.
      WHEN 'BUTTON_EDIT'.
        IF confirm_dataloss_if_dirty( i_event ).
          edit( ).
        ENDIF.
      WHEN 'BUTTON_SAVE'.
        ASSERT editable = abap_true.
        save( ).
      WHEN 'F4'.
        IF confirm_dataloss_if_dirty( i_event ).
          on_f4( ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD get_text_for.

    DATA:
      data_element         TYPE rollname,
      data_element_details TYPE rsddtel.


    data_element = cl_abap_datadescr=>describe_by_data( i_field )->get_relative_name( ).

    CALL FUNCTION 'RSD_DTEL_GET'
      EXPORTING
        i_dtelnm       = data_element
      IMPORTING
        e_s_dtel       = data_element_details
      EXCEPTIONS
        dtel_not_found = 1
        doma_not_found = 2
        illegal_input  = 3
        OTHERS         = 4.
    IF sy-subrc = 0.
      result = data_element_details-txtmd.
    ELSE.
      result = data_element.
    ENDIF.

  ENDMETHOD.


  METHOD date_out.

    result = |{ i_date DATE = USER }|.

  ENDMETHOD.


  METHOD on_f4.

    init( ).

    f4_active = abap_true.

    client->nav_app_call( z2ui5_cl_tool_app_shlp_gen=>factory(
                              iv_shlp_id     = 'ZWS_SH_FACTORY_CALENDAR'
                              iv_popup_title = 'Fabrikkalender auswählen'
                              iv_autoexec    = abap_true ) ).

  ENDMETHOD.


  METHOD get_model.

    result = NEW zcl_fc_maint_exceptions_model(
                     i_calendar_id = calendarid
                     i_sales_area  = sales_area
                     i_year        = EXACT #( year ) ).

  ENDMETHOD.


  METHOD init.

    CLEAR:
      message-text,
      message-visible,
      header_valid,
      dirty,
      editable,
      exceptions,
      calendartext,
      salesarea_text.

  ENDMETHOD.


  METHOD scroll_to_top.

    scroll_values = VALUE #( ( n = `page` v = '0' ) ).
    scrollupdate = abap_true.

  ENDMETHOD.


  METHOD handle_error.
    message-visible = abap_true.
    message-type    = `Error`.
    message-text    = i_error->get_text( ).
  ENDMETHOD.

ENDCLASS.
