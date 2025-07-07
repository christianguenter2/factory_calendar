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

      calendarId      TYPE tfain-ident,
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
      header_valid      TYPE abap_bool,
      check_initialized TYPE abap_bool READ-ONLY.

  PRIVATE SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS initialize.
    METHODS go.
    METHODS save.
    METHODS date_in
      IMPORTING
        i_date        TYPE string
      RETURNING
        VALUE(result) TYPE datum.
    METHODS edit.

ENDCLASS.



CLASS zcl_fc_maint_exceptions_app IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.
      initialize( ).
    ENDIF.

    CASE client->get( )-event.
      WHEN 'BACK'.
        client->set_session_stateful( abap_false ).
        client->nav_app_leave( ).
      WHEN 'GO'.
        go( ).
      WHEN 'BUTTON_DELETE'.
        DELETE exceptions WHERE selkz = abap_true.
      WHEN 'BUTTON_ADD'.
        INSERT INITIAL LINE INTO TABLE exceptions.
      WHEN 'BUTTON_EDIT'.
        edit( ).
      WHEN 'BUTTON_SAVE'.
        save( ).
    ENDCASE.

    client->view_model_update( ).

  ENDMETHOD.


  METHOD initialize.

    zcl_fc_maint_exceptions_model=>unlock( ).

    client->set_session_stateful( abap_true ).

*    IF client->get( )-check_launchpad_active = abap_false.
*    ENDIF.

    message-type = `Information`.
    message-visible = abap_false.

    FINAL(page) = z2ui5_cl_xml_view=>factory(
                                  )->shell(
                                  )->page(
                                     title          = `Fabrikkalender - Ausnahmen pflegen`
                                     navbuttonpress = client->_event( 'BACK' )
                                     shownavbutton  = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL ) ).



    page->message_strip(
        type     = client->_bind( message-type )
        text     = client->_bind( message-text )
        visible  = client->_bind( message-visible )
        showicon = abap_true
    ).

    page->horizontal_layout(
               class = `sapUiContentPadding`
       )->simple_form( editable = abap_true
       )->content( `form`
       )->label( text     = `Kalender ID`
                 labelfor = `inputCalId`
                 required = abap_true
       )->input( id = `inputCalId`
                 value = client->_bind_edit( calendarId )
                 maxlength = '2'
                 submit = client->_event( `GO` )
       )->text( text = client->_bind( calendarText )
       )->label( text     = `Jahr`
                 labelfor = `inputYear`
                 required = abap_true
       )->input( id = `inputYear`
                 value = client->_bind_edit( year )
                 maxlength = '4'
                 submit = client->_event( `GO` )
*                                  )->label( text     = `Kalender ID`
*                                            labelfor = `inputCalId`
*                                  )->input( id = `inputCalId`

       )->button(
         text  = `Go`
         type  = `Emphasized`
         press = client->_event( `GO` )
       ).

    FINAL(table) = page->horizontal_layout(
                        class = `sapUiContentPadding`
                      )->table(
                        items = client->_bind_edit( exceptions )
                        mode  = 'MultiSelect'
                        visible = client->_bind( header_valid ) ).

    table->header_toolbar(
        )->overflow_toolbar(
        )->button(
            icon    = 'sap-icon://add'
            text    = 'add'
            press   = client->_event( 'BUTTON_ADD' )
            enabled = client->_bind( editable )
        )->button(
            icon  = 'sap-icon://delete'
            text  = 'delete'
            press = client->_event( 'BUTTON_DELETE' )
            enabled = client->_bind( editable ) ).

    table->columns(
        )->column( )->text( `Gültig von` )->get_parent(
        )->column( )->text( `Gültig bis` )->get_parent(
        )->column( )->text( `Arbeitstag` )->get_parent(
        )->column( )->text( `Text` ).

    table->items( )->column_list_item( selected = `{SELKZ}`
        )->cells(
            )->date_picker( editable = client->_bind( editable ) value = `{VON}` placeholder = client->_bind( placeholder_von ) required = abap_true
            )->date_picker( editable = client->_bind( editable ) value = `{BIS}` placeholder = client->_bind( placeholder_bis ) required = abap_true
            )->checkbox( editable = client->_bind( editable ) selected = `{WERT}`
            )->input(  editable = client->_bind( editable ) value = `{LTEXT}` ).

    page->footer( )->overflow_toolbar(
                   )->button(
                       text    = 'Edit'
                       press   = client->_event( 'BUTTON_EDIT' )
                       icon    = 'sap-icon://edit'
                   )->toolbar_spacer(
                   )->button(
                       enabled = client->_bind( editable )
                       text    = 'Save'
                       press   = client->_event( 'BUTTON_SAVE' )
                       type    = 'Success' ).

    client->view_display( page->stringify( ) ).

  ENDMETHOD.


  METHOD go.

    CLEAR:
      message-text,
      message-visible,
      header_valid.

    calendarId = to_upper( calendarId ).

    FINAL(factory_calendar) = NEW zcl_fc_maint_exceptions_model(
                                      i_calendar_id = calendarId
                                      i_year        = EXACT #( year ) ).

    TRY.
        factory_calendar->existence_check( ).
        calendarText = factory_calendar->get_description( ).

        IF year IS NOT INITIAL.
          factory_calendar->validate( ).
          header_valid = abap_true.
          exceptions = VALUE #(
                         FOR exception IN factory_calendar->retrieve_exceptions( )
                         (
                           von = |{ exception-von DATE = USER }|
                           bis = |{ exception-bis DATE = USER }|
                           ltext = exception-ltext
                           wert  = xsdbool( exception-wert = '1' )
                         ) ).

          placeholder_von = |01.01.{ year }|.
          placeholder_bis = |31.01.{ year }|.
        ENDIF.

      CATCH zcx_fc_error INTO FINAL(error).
        message-visible = abap_true.
        message-type = `Error`.
        message-text = error->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD save.

    CLEAR:
      message-text,
      message-visible.

    FINAL(factory_calendar) = NEW zcl_fc_maint_exceptions_model(
                                      i_calendar_id = calendarId
                                      i_year        = EXACT #( year ) ).

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
    message-text = `Änderungen gespeichert`.

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
        IF editable = abap_true.
          editable = abap_false.
          zcl_fc_maint_exceptions_model=>unlock( ).
        ELSE.
          editable = abap_true.
          zcl_fc_maint_exceptions_model=>lock( ).
        ENDIF.

      CATCH zcx_fc_error INTO FINAL(error).
        message-visible = abap_true.
        message-type = `Error`.
        message-text = error->get_text( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
