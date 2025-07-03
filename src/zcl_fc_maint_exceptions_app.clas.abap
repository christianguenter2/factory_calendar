CLASS zcl_fc_maint_exceptions_app DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      z2ui5_if_app.
    TYPES:
      BEGIN OF t_tfain,
        selkz TYPE abap_bool.
        INCLUDE TYPE tfain.
    TYPES:
      END OF t_tfain.

    DATA:
      calendarId TYPE tfain-ident,
      year       TYPE c LENGTH 4, " tfain-jahr.
      exceptions TYPE STANDARD TABLE OF t_tfain WITH NON-UNIQUE DEFAULT KEY,
      BEGIN OF message,
        type    TYPE string,
        text    TYPE string,
        visible TYPE abap_bool,
      END OF message,
      calendartext TYPE string.

  PRIVATE SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS initialize.
    METHODS go.

ENDCLASS.



CLASS zcl_fc_maint_exceptions_app IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      initialize( ).
    ENDIF.

    CASE client->get( )-event.
      WHEN 'BACK'.
        client->nav_app_leave( ).
      WHEN 'GO'.
        go( ).
      WHEN 'BUTTON_DELETE'.
        DELETE exceptions WHERE selkz = abap_true.
      WHEN 'BUTTON_ADD'.
        INSERT INITIAL LINE INTO TABLE exceptions.
      WHEN 'BUTTON_SAVE'.
    ENDCASE.

    client->view_model_update( ).

  ENDMETHOD.


  METHOD initialize.

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
        type    = client->_bind( message-type )
        text    = client->_bind( message-text )
        visible = client->_bind( message-visible )
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
       )->text( text = client->_bind( calendarText )
       )->label( text     = `Jahr`
                 labelfor = `inputYear`
                 required = abap_true
       )->input( id = `inputYear`
                 value = client->_bind_edit( year )
                 maxlength = '4'
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
                        mode  = 'MultiSelect' ).

    table->header_toolbar(
        )->overflow_toolbar(
        )->button(
            icon  = 'sap-icon://delete'
            text  = 'delete selected row'
            press = client->_event( 'BUTTON_DELETE' )
        )->button(
            icon  = 'sap-icon://add'
            text  = 'add'
            press = client->_event( 'BUTTON_ADD' ) ).

    table->columns(
        )->column( )->text( `Gültig von` )->get_parent(
        )->column( )->text( `Gültig bis` ).

    table->items( )->column_list_item( selected = `{SELKZ}`
        )->cells(
            )->date_picker( value = `{VON}` )->get_parent(
            )->date_picker( value = `{BIS}` ).

    page->footer( )->overflow_toolbar(
                   )->toolbar_spacer(
                   )->button(
                       text  = 'Save'
                       press = client->_event( 'BUTTON_SAVE' )
                       type  = 'Success' ).

    client->view_display( page->stringify( ) ).

  ENDMETHOD.


  METHOD go.

    CLEAR:
      message-text,
      message-visible.

    FINAL(factory_calendar) = NEW zcl_fc_maint_exceptions_model( calendarId ).

    IF factory_calendar->existence_check( ) = abap_false.
      message-type = `Error`.
      message-text = |Kalender { calendarId } existiert nicht!|.
      message-visible = abap_true.
      RETURN.
    ENDIF.

    calendarText = factory_calendar->get_description( ).

  ENDMETHOD.

ENDCLASS.
