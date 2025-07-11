CLASS lhc_zr_fc_exceptions DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    TYPES:
      tt_failed_early   TYPE TABLE FOR FAILED EARLY zr_fc_exceptions,
      tt_failed_late    TYPE TABLE FOR FAILED LATE zr_fc_exceptions,
      tt_reported_early TYPE TABLE FOR REPORTED EARLY zr_fc_exceptions,
      tt_reported_late  TYPE TABLE FOR REPORTED LATE zr_fc_exceptions,

      BEGIN OF t_key,
        ident TYPE wfcid,
        jahr  TYPE kjahr,
        von   TYPE fabdatv,
      END OF t_key.

    METHODS: get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zr_fc_exceptions RESULT result,

      precheck_create FOR PRECHECK
        IMPORTING entities FOR CREATE zr_fc_exceptions,

      precheck_update FOR PRECHECK
        IMPORTING entities FOR UPDATE zr_fc_exceptions,

      precheck_delete FOR PRECHECK
        IMPORTING keys FOR DELETE zr_fc_exceptions,

      check_dates
        IMPORTING
          i_entity   TYPE zr_fc_exceptions
          i_key      TYPE t_key
        CHANGING
          c_failed   TYPE tt_failed_early
          c_reported TYPE tt_reported_early,

      insert_failed_early_message
        IMPORTING
          i_key      TYPE lhc_zr_fc_exceptions=>t_key
        CHANGING
          c_failed   TYPE tt_failed_early
          c_reported TYPE tt_reported_early,

      insert_failed_late_message
        IMPORTING
          i_key      TYPE lhc_zr_fc_exceptions=>t_key
        CHANGING
          c_failed   TYPE tt_failed_late
          c_reported TYPE tt_reported_late,

      new_error_message_from_sy
        RETURNING
          VALUE(result) TYPE REF TO if_abap_behv_message,

      overlapping_intervals FOR VALIDATE ON SAVE
        IMPORTING keys FOR zr_fc_exceptions~overlapping_intervals.

ENDCLASS.

CLASS lhc_zr_fc_exceptions IMPLEMENTATION.

  METHOD get_instance_authorizations.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).

*      AUTHORITY-CHECK
*             OBJECT 'ZFC_MAINT'
*             ID 'ZFC_IDENT' FIELD <key>-ident
*             ID 'ACTVT' FIELD '02'.
*      IF sy-subrc <> 0.
      " You are not authorized to maintain calendar &1
*        MESSAGE e011(zfc_maint) WITH <key>-ident INTO FINAL(dummy).
*        APPEND VALUE #( %tky = <key>-%tky ) TO failed-zr_fc_exceptions.
*        APPEND VALUE #( %tky = keys[ 1 ]-%tky
*                        %msg = new_error_message_from_sy( ) )
*               TO reported-zr_fc_exceptions.
*        APPEND VALUE #( %tky = <key>-%tky
*                        %delete = if_abap_behv=>auth-unauthorized
*                        %update = if_abap_behv=>auth-unauthorized
*                        %action-edit = if_abap_behv=>auth-unauthorized )
*               TO result.
*      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD precheck_create.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

*      AUTHORITY-CHECK
*             OBJECT 'ZFC_MAINT'
*             ID 'ZFC_IDENT' FIELD <entity>-ident
*             ID 'ACTVT' FIELD '02'.
*      IF sy-subrc <> 0.
*        " You are not authorized to maintain calendar &1
*        MESSAGE e011(zfc_maint) WITH <entity>-ident INTO FINAL(dummy).
*        insert_failed_early_message(
*          EXPORTING
*            i_key      = <entity>-%key
*          CHANGING
*            c_failed   = failed-zr_fc_exceptions
*            c_reported = reported-zr_fc_exceptions ).
*      ENDIF.

      SELECT
        SINGLE @abap_true AS valid
        FROM tfacd
        WHERE ident  = @<entity>-ident
        AND   vjahr <= @<entity>-jahr
        AND   bjahr >= @<entity>-jahr
        INTO @FINAL(valid).
      IF valid = abap_false.
        " Calendar &1 ist not maintained in year &2
        MESSAGE e002(zfc_maint) WITH <entity>-ident <entity>-jahr INTO FINAL(dummy).
        insert_failed_early_message(
          EXPORTING
            i_key      = <entity>-%key
          CHANGING
            c_failed   = failed-zr_fc_exceptions
            c_reported = reported-zr_fc_exceptions ).
      ENDIF.

      check_dates(
        EXPORTING
          i_entity   = CORRESPONDING #( <entity> )
          i_key      = <entity>-%key
        CHANGING
          c_failed   = failed-zr_fc_exceptions
          c_reported = reported-zr_fc_exceptions ).

    ENDLOOP.

  ENDMETHOD.


  METHOD precheck_update.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

      check_dates(
        EXPORTING
          i_entity   = CORRESPONDING #( <entity> )
          i_key      = <entity>-%key
        CHANGING
          c_failed   = failed-zr_fc_exceptions
          c_reported = reported-zr_fc_exceptions ).

    ENDLOOP.

  ENDMETHOD.


  METHOD precheck_delete.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).

      IF <key>-jahr < sy-datum+0(4).
        " Change not possible for past years
        MESSAGE e003(zfc_maint) INTO DATA(dummy).

        insert_failed_early_message(
          EXPORTING
            i_key      = CORRESPONDING #( <key> )
          CHANGING
            c_failed   = failed-zr_fc_exceptions
            c_reported = reported-zr_fc_exceptions ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_dates.

    IF i_entity-jahr < sy-datum+0(4).
      " Change not possible for past years
      MESSAGE e003(zfc_maint) INTO DATA(dummy).

      insert_failed_early_message(
        EXPORTING
          i_key      = i_key
        CHANGING
          c_failed   = c_failed
          c_reported = c_reported ).
    ENDIF.

    IF i_entity-jahr <> i_entity-von(4).
      " Date &1 is no possible for year &2
      MESSAGE e007(zfc_maint) INTO dummy.

      insert_failed_early_message(
        EXPORTING
          i_key      = i_key
        CHANGING
          c_failed   = c_failed
          c_reported = c_reported ).
    ENDIF.

    IF  i_entity-von IS NOT INITIAL
    AND i_entity-bis IS NOT INITIAL
    AND i_entity-bis < i_entity-von.
      " From date has to before to Date
      MESSAGE e008(zfc_maint) INTO dummy.

      insert_failed_early_message(
        EXPORTING
          i_key      = i_key
        CHANGING
          c_failed   = c_failed
          c_reported = c_reported ).
    ENDIF.

  ENDMETHOD.


  METHOD insert_failed_early_message.
    APPEND VALUE #(  %key =  i_key
                      %create = if_abap_behv=>mk-on
                      %update = if_abap_behv=>mk-on
                      %delete = if_abap_behv=>mk-on )
                 TO c_failed.
    APPEND VALUE #(  %key =  i_key
                     %msg = new_error_message_from_sy( )
                     %create = if_abap_behv=>mk-on
                     %update = if_abap_behv=>mk-on
                     %delete = if_abap_behv=>mk-on )
           TO c_reported.
  ENDMETHOD.


  METHOD new_error_message_from_sy.
    result = new_message(
                 id       = sy-msgid
                 number   = sy-msgno
                 severity = if_abap_behv_message=>severity-error
                 v1       = sy-msgv1
                 v2       = sy-msgv2
                 v3       = sy-msgv3
                 v4       = sy-msgv4 ).
  ENDMETHOD.


  METHOD overlapping_intervals.

    READ
      ENTITIES OF zc_fc_exceptions
      ENTITY zc_fc_exceptions
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(exceptions)
      FAILED DATA(failed_read).
    IF failed_read IS NOT INITIAL.
      failed = CORRESPONDING #( failed_read ).
      RETURN.
    ENDIF.

    SELECT
      FROM zc_fc_exceptions AS db
      INNER JOIN @exceptions AS exceptions
              ON  db~ident = exceptions~ident
              AND db~von  <> exceptions~von
              AND ( ( db~von <= exceptions~von AND db~bis >= exceptions~bis )
                OR  ( db~von <= exceptions~bis AND db~bis >= exceptions~von )
                OR  ( db~bis <= exceptions~von AND db~von >= exceptions~von )
                OR  ( db~bis <= exceptions~bis AND db~von >= exceptions~bis ) )
      FIELDS db~*
      INTO TABLE @FINAL(duplicates).

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).

      ASSIGN duplicates[ ident = <key>-ident ] TO FIELD-SYMBOL(<duplicate>).
      IF sy-subrc = 0.
        " Overlapping periods are not allowed. Already maintained: &1
        MESSAGE e009(zfc_maint) INTO FINAL(dummy) WITH |{ <duplicate>-von DATE = USER } - { <duplicate>-bis DATE = USER }|.

        insert_failed_late_message(
          EXPORTING
            i_key      = CORRESPONDING #( <key> )
          CHANGING
            c_failed   = failed-zr_fc_exceptions
            c_reported = reported-zr_fc_exceptions ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD insert_failed_late_message.
    APPEND VALUE #(  %key =  i_key
                     %create = if_abap_behv=>mk-on
                     %update = if_abap_behv=>mk-on
                     %delete = if_abap_behv=>mk-on )
                 TO c_failed.
    APPEND VALUE #(  %key =  i_key
                     %msg = new_error_message_from_sy( )
                     %create = if_abap_behv=>mk-on
                     %update = if_abap_behv=>mk-on
                     %delete = if_abap_behv=>mk-on )
           TO c_reported.
  ENDMETHOD.

ENDCLASS.
