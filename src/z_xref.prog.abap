REPORT z_xref.
*--------------------------------------------------------------------*
* Het programma onderkent de volgende unit ID's
* \PA:PACKAGE
* \TY:CLASS\ME:METHOD
* \TY:CLASS\IN:INTERFACE\ME:METHOD
* \TY:CLASS\TY:CLASS\ME:METHOD
* \TY:INTERFACE\ME:METHOD
* \FG:FUGR\FO:FORM
* \FG:FUGR\MO:PBO
* \FG:FUGR\MX:PAI
* \FG:FUGR\FU:FUNC
* \PR:PROG
* \PR:PROG\FO:FORM
* \PR:PROG\MO:PBO
* \PR:PROG\MX:PAI
* \PR:PROG\TY:CLASS\ME:METHOD
* \PR:PROG\TY:CLASS\IN:INTERFACE\ME:METHOD
*--------------------------------------------------------------------*

CLASS:
  lcl_unit  DEFINITION DEFERRED,
  lcl_units DEFINITION DEFERRED.

TYPES:
  BEGIN OF ts_unit,
    id      TYPE string,
    sap     TYPE abap_bool,
    package TYPE devclass,
  END OF ts_unit,
  tt_units TYPE HASHED TABLE OF REF TO lcl_unit WITH UNIQUE KEY table_line,

  BEGIN OF ts_call,
    source TYPE string,
    target TYPE string,
  END OF ts_call,
  tt_calls TYPE STANDARD TABLE OF ts_call WITH KEY table_line,

  BEGIN OF ts_xrefs,
    type                TYPE string,
    name                TYPE string,
    component           TYPE string,
    unit                TYPE string,
    depth_calls         TYPE i,
    depth_where_used    TYPE i,
    include_sap_objects TYPE abap_bool,
    system              TYPE string,
    release             TYPE sy-saprl,
    date                TYPE d,
    units               TYPE SORTED TABLE OF ts_unit WITH UNIQUE KEY id,
    calls               TYPE tt_calls,
  END OF ts_xrefs.

*--------------------------------------------------------------------*
CLASS lcl_cache DEFINITION.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    TYPES:
      tt_keys TYPE STANDARD TABLE OF zxref_cache WITH EMPTY KEY.

    CLASS-METHODS:
      add
        IMPORTING key TYPE zxref_cache,
      fill
        IMPORTING keys TYPE tt_keys,
      find
        IMPORTING unit                TYPE string
                  depth_calls         TYPE i
                  depth_where_used    TYPE i
                  include_sap_objects TYPE abap_bool
        RETURNING VALUE(key)          TYPE zxref_cache,
      get
        IMPORTING unit                TYPE string
                  depth_calls         TYPE i
                  depth_where_used    TYPE i
                  include_sap_objects TYPE abap_bool
        RETURNING VALUE(key)          TYPE zxref_cache,
      is_empty
        RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_cache IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD add.

    DATA(ls_key) = VALUE zxref_cache( BASE key
                                      created_on = sy-datum
                                      created_by = sy-uname ).
    MODIFY zxref_cache FROM ls_key.
    COMMIT WORK.

  ENDMETHOD.

  METHOD fill.

    INSERT zxref_cache FROM TABLE keys.
    COMMIT WORK.

  ENDMETHOD.

  METHOD find.

    DATA:
      lr_include_sap_objects TYPE RANGE OF abap_bool.

    IF include_sap_objects = abap_true.
      lr_include_sap_objects = VALUE #( ( sign = 'I' option = 'EQ' low = abap_true ) ).
    ENDIF.

    SELECT *
      FROM zxref_cache UP TO 1 ROWS
      INTO @key
     WHERE unit                 = @unit
       AND depth_calls         >= @depth_calls
       AND depth_where_used    >= @depth_where_used
       AND include_sap_objects IN @lr_include_sap_objects
     ORDER BY depth_calls ASCENDING,
              depth_calls ASCENDING.
    ENDSELECT.

  ENDMETHOD.

  METHOD get.

    SELECT SINGLE *
      FROM zxref_cache
     WHERE unit                = @unit
       AND depth_calls         = @depth_calls
       AND depth_where_used    = @depth_where_used
       AND include_sap_objects = @include_sap_objects
      INTO @key.

  ENDMETHOD.

  METHOD is_empty.

    DATA:
      lv_subrc TYPE i.

    CALL FUNCTION 'DD_EXISTS_DATA'
      EXPORTING
        tabclass = space
        tabname  = 'ZXREF_CACHE'
      IMPORTING
        subrc    = lv_subrc
      EXCEPTIONS
        OTHERS   = 0.

    IF lv_subrc <> 0.
      result = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_unit DEFINITION
*--------------------------------------------------------------------*
* Uitvoerbare unit
*--------------------------------------------------------------------*
  CREATE PRIVATE
  FRIENDS lcl_units.

  PUBLIC SECTION.
    DATA:
      id        TYPE string READ-ONLY,     "Eigen ID met kleine variaties op FULL_NAME
      type      TYPE trobjtype READ-ONLY,  "DEVC/CLAS/INTF/FUGR/FUNC/PROG
      full_name TYPE string READ-ONLY.     "Volledige naam zoals gebruikt door CL_ABAP_COMPILER

    METHODS:
      get_component
        IMPORTING comp             TYPE csequence
        RETURNING VALUE(component) TYPE REF TO lcl_unit,
      get_components
        RETURNING VALUE(components) TYPE tt_units,
      get_include
        RETURNING VALUE(include) TYPE program,
      get_name
        RETURNING VALUE(name) TYPE string,
      get_package
        RETURNING VALUE(package) TYPE devclass,
      get_program
        RETURNING VALUE(program) TYPE program,
      get_segment
        IMPORTING s              TYPE simple
        RETURNING VALUE(segment) TYPE string,
      get_segments
        RETURNING VALUE(segments) TYPE name2stringvalue_table,
      is_standard_sap
        RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS:
      init
        IMPORTING VALUE(full_name) TYPE string,
      parse_method
        IMPORTING VALUE(method)    TYPE csequence
        RETURNING VALUE(full_name) TYPE string.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_units DEFINITION.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    CLASS-METHODS:
      get
        IMPORTING VALUE(type) TYPE trobjtype OPTIONAL  "DEVC/CLAS/INTF/FUNC/FUGR/PROG
                  VALUE(name) TYPE csequence
                  comp        TYPE csequence OPTIONAL  "METH/FORM
        RETURNING VALUE(unit) TYPE REF TO lcl_unit,
      get_by_cross_ref
        IMPORTING ref         TYPE rsfindlst
        RETURNING VALUE(unit) TYPE REF TO lcl_unit,
      get_by_full_name
        IMPORTING VALUE(full_name) TYPE string
        RETURNING VALUE(unit)      TYPE REF TO lcl_unit,
      find_routine
        IMPORTING program        TYPE program
                  row            TYPE i
        RETURNING VALUE(routine) TYPE string.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_buffer,
        id   TYPE string,
        unit TYPE REF TO lcl_unit,
      END OF ts_buffer.

    CLASS-DATA:
      buffer TYPE HASHED TABLE OF ts_buffer WITH UNIQUE KEY id.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_unit IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD get_component.

    DATA(lt_components) = get_components( ).
    CHECK lt_components IS NOT INITIAL.

    "Technische notitatie opgegeven of af te leiden?
    DATA(lv_comp_id) = COND string( WHEN comp CS `\` THEN comp
                                    WHEN me->type = 'CLAS' OR me->type = 'INTF' THEN parse_method( comp ) ).

    IF lv_comp_id IS INITIAL.
      "Zoek of er een component is met de opgegeven naam
      LOOP AT lt_components INTO DATA(lo_component).
        IF lo_component->get_segment( 1 ) = comp AND me->type = 'DEVC' OR  "Package is geen onderdeel van de hiërarchie
           lo_component->get_segment( 2 ) = comp AND lo_component->get_segment( 3 ) IS INITIAL.
          component = lo_component.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      TRY.
          component = COND #( WHEN me->type = 'DEVC'
                        THEN lt_components[ table_line->id = lv_comp_id ]
                        ELSE lt_components[ table_line->id = me->id && lv_comp_id ] ).
        CATCH cx_sy_itab_line_not_found  ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD get_components.

    DATA:
      lt_objects         TYPE STANDARD TABLE OF rseui_set WITH EMPTY KEY,
      lt_supported_types TYPE RANGE OF trobjtype.

    "Alleen voor hoofdobjecten
    CHECK lines( get_segments( ) ) = 1.

    DATA(lo_repository) = cl_sca_repository_access=>get_local_access( ).

    CASE me->type.
      WHEN 'DEVC'.
        lt_supported_types = VALUE #( sign = 'I' option = 'EQ' ( low = 'CLAS' )
                                                               ( low = 'INTF' )
                                                               ( low = 'FUNC' )
                                                               ( low = 'FUGR' )
                                                               ( low = 'PROG' ) ).
        CALL FUNCTION 'RS_GET_OBJECTS_OF_DEVCLASS'
          EXPORTING
            devclass   = CONV devclass( get_name( ) )
          TABLES
            objectlist = lt_objects
          EXCEPTIONS
            OTHERS     = 0.
        LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<ls_object>) WHERE obj_type IN lt_supported_types.
          DATA(lo_component) = lcl_units=>get( type = <ls_object>-obj_type
                                               name = <ls_object>-obj_name ).
          IF lo_component IS BOUND.
            INSERT lo_component INTO TABLE components.
          ENDIF.
        ENDLOOP.

      WHEN 'CLAS' OR 'INTF'.
        DATA(lt_methods) = CAST cl_abap_objectdescr( cl_abap_objectdescr=>describe_by_name( get_name( ) ) )->methods.
        DELETE lt_methods WHERE is_inherited = abap_true AND is_redefined = abap_false.
        DELETE lt_methods WHERE alias_for IS NOT INITIAL.
        LOOP AT lt_methods INTO DATA(ls_method).
          DATA(lv_full_name) = me->full_name && parse_method( ls_method-name ).
          INSERT lcl_units=>get_by_full_name( lv_full_name ) INTO TABLE components.
        ENDLOOP.

        "Zoek naar lokale klassen
        cl_abap_compiler=>create( get_program( ) )->get_all_refs(
          EXPORTING p_local  = abap_true  "inclusief lokale klassen
                    p_types  = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>tag_type ) )
                    p_grades = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>grade_definition ) )
          IMPORTING p_result = DATA(lt_local_classes) ).
        DELETE lt_local_classes WHERE full_name NP '\PR:*'.  "Filter op daadwerkelijk lokale klassen
        DELETE lt_local_classes WHERE mode1 = cl_abap_compiler=>mode1_test.  "Negeer lokale testklassen

        "Zoek de bijbehorende lokale methodes
        cl_abap_compiler=>create( get_program( ) )->get_all_refs(
          EXPORTING p_local  = abap_true
                    p_types  = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>tag_method ) )
                    p_grades = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>grade_definition ) )
          IMPORTING p_result = DATA(lt_local_methods) ).

        LOOP AT lt_local_classes INTO DATA(ls_local_class).
          LOOP AT lt_local_methods INTO DATA(ls_local_method) WHERE full_name CS ls_local_class-full_name.
            "Dubbelingen kunnen voorkomen maar worden afgevangen door de hash-tabel
            INSERT lcl_units=>get_by_full_name( ls_local_method-full_name ) INTO TABLE components.
          ENDLOOP.
        ENDLOOP.

      WHEN 'FUGR'.
        "Functies
        DATA(lt_functions) = lo_repository->get_functions_of_function_pool( CONV #( get_name( ) ) ).
        components = VALUE #( FOR f IN lt_functions ( lcl_units=>get_by_full_name( `\FU:` && f-funcname ) ) ).
    ENDCASE.

    CASE me->type.
      WHEN 'FUGR' OR 'PROG'.
        "Form-routines en methodes
        cl_abap_compiler=>create( get_program( ) )->get_all_refs(
          EXPORTING p_local    = abap_true
                    p_types    = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>tag_form )
                                                                   ( low = cl_abap_compiler=>tag_method )
                                                                   ( low = cl_abap_compiler=>tag_module_in )
                                                                   ( low = cl_abap_compiler=>tag_module_out ) )
                    p_grades   = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>grade_definition ) )
                    p_extended = abap_true  "vult SYMBOL
          IMPORTING p_result   = DATA(lt_definitions) ).
        DELETE lt_definitions WHERE mode1 = cl_abap_compiler=>mode1_test.  "Negeer test-methodes

        "Verwijder aliassen voor interface-methodes
        LOOP AT lt_definitions INTO DATA(ls_def) WHERE tag = cl_abap_compiler=>tag_method.
          "Deze zijn te herkennen aan een super-methode binnen dezelfde unit (helaas geen ALIAS_FOR zoals hierboven)
          DATA(lo_method) = CAST cl_abap_comp_method( ls_def-symbol ).
          IF lo_method->super_method IS BOUND AND
             substring_before( val = lo_method->full_name                sub = `\ME` )   "(\PR:ZPROG25\TY:LCLASS3)\ME:ALIAS2
           = substring_before( val = lo_method->super_method->full_name  sub = `\IN` ).  "(\PR:ZPROG25\TY:LCLASS3)\IN:LINTERFACE3\ME:METHOD2
            DELETE lt_definitions.
          ENDIF.
        ENDLOOP.

        components = VALUE tt_units( BASE components FOR GROUPS g OF d IN lt_definitions
                                                         GROUP BY d-full_name WITHOUT MEMBERS ( lcl_units=>get_by_full_name( g ) ) ).
    ENDCASE.

    SORT components BY table_line->id.

  ENDMETHOD.

  METHOD get_include.

    CASE me->type.
      WHEN 'CLAS' OR 'INTF'.
        DATA(lv_method) = get_segment( cl_abap_compiler=>tag_method ).
        DATA(lv_interface) = get_segment( cl_abap_compiler=>tag_interface ).
        IF lv_interface IS NOT INITIAL.
          lv_method = |{ lv_interface }~{ lv_method }|.
        ENDIF.
        IF lv_method IS NOT INITIAL.
          cl_oo_classname_service=>get_method_include( EXPORTING mtdkey = VALUE #( clsname = get_name( )
                                                                                   cpdname = lv_method )
                                                       RECEIVING result = include
                                                       EXCEPTIONS OTHERS = 0 ).
        ENDIF.

      WHEN 'FUNC'.
        DATA(lv_function) = CONV funcname( get_name( ) ).  "string » char30
        CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
          CHANGING
            funcname = lv_function
            include  = include
          EXCEPTIONS
            OTHERS   = 0.

      WHEN 'FUGR' OR 'PROG'.
        "De ABAP compiler kan alle form-routines inclusief de includes teruggeven
        cl_abap_compiler=>create( get_program( ) )->get_all_refs(
          EXPORTING p_local  = abap_true
                    p_grades = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>grade_definition ) )
          IMPORTING p_result = DATA(lt_definitions) ).
        TRY.
            include = lt_definitions[ full_name = me->full_name ]-include.
          CATCH cx_sy_itab_line_not_found  ##NO_HANDLER.
        ENDTRY.
    ENDCASE.

  ENDMETHOD.

  METHOD get_name.

    name = COND #( WHEN me->type = 'FUNC'
                   THEN get_segment( 2 )  "functie wordt voorafgegaan door de functiegroep
                   ELSE get_segment( 1 ) ).

  ENDMETHOD.

  METHOD get_package.

    DATA(lv_type) = me->type .
    DATA(lv_name) = get_name( ).

    "Voor functies is de pakkettoewijzing op functiegroep niveau
    IF me->type = 'FUNC'.
      lv_type = 'FUGR'.
      lv_name = get_segment( 'FG' ).
    ENDIF.

    TRY.
        DATA(lo_repository) = cl_sca_repository_access=>get_local_access( ).
        DATA(ls_package) = lo_repository->get_package_for_object_key( VALUE #( pgmid    = 'R3TR'
                                                                               obj_type = lv_type
                                                                               obj_name = lv_name ) ).
        package = ls_package-package_name.

      CATCH cx_sca_object_not_found  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD get_program.

    CASE me->type.
      WHEN 'CLAS' OR 'INTF'.
        program = cl_oo_classname_service=>get_classpool_name( CONV #( get_name( ) ) ).

      WHEN 'FUGR'.
        program = 'SAPL' && get_name( ).

      WHEN 'FUNC'.
        DATA(lv_function) = CONV funcname( get_name( ) ).  "string » char30
        CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
          IMPORTING
            pname    = program  "Hoofdprogramma
          CHANGING
            funcname = lv_function
          EXCEPTIONS
            OTHERS   = 0.

      WHEN 'PROG'.
        program = get_name( ).
    ENDCASE.

  ENDMETHOD.

  METHOD get_segment.

    DATA(lt_segments) = get_segments( ).

    TRY.
        DESCRIBE FIELD s TYPE DATA(lv_type).
        segment = COND #( WHEN lv_type = 'I' THEN lt_segments[ s ]-value ELSE lt_segments[ name = s ]-value ).

      CATCH cx_sy_itab_line_not_found  ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD get_segments.

    DATA:
      lt_segments TYPE stringtab.

    SPLIT me->id AT '\' INTO TABLE lt_segments.
    DELETE lt_segments INDEX 1.
    segments = VALUE #( FOR s IN lt_segments ( name  = substring_before( val = s  sub = ':' )
                                               value = substring_after( val = s  sub = ':' ) ) ).

  ENDMETHOD.

  METHOD is_standard_sap.

    DATA(lt_custom_range) = VALUE packrange( sign = 'I' ( option = 'CP' low = '$*' )
                                                        ( option = 'CP' low = 'Z*' )
                                                        ( option = 'CP' low = 'Y*' ) ).

    result = COND #( WHEN get_package( ) NOT IN lt_custom_range THEN abap_true ).

  ENDMETHOD.

  METHOD init.

    "Klasse/interface indicator meegegeven vanuit de GET-methode?
    FIND REGEX 'TY([CI]):' IN full_name SUBMATCHES DATA(lv_type_kind).
    IF sy-subrc = 0.
      REPLACE REGEX 'TY([CI]):' IN full_name WITH 'TY:'.
    ENDIF.

    "Vervang in de volledige naam de (eigen) functiegroep notatie door het eigenlijke programma
    REPLACE '\FG:' IN full_name WITH '\PR:SAPL'.
    IF sy-subrc = 0 AND full_name CS 'FU:'.
      full_name = `\FU:` && substring_after( val = full_name  sub = `FU:` ).
    ENDIF.

    me->full_name = full_name.
    me->id = replace( val = me->full_name  sub = `\PR:SAPL` with = `\FG:` ).
    me->type = SWITCH #( me->id(3) WHEN `\PA` THEN 'DEVC'
                                   WHEN `\FU` THEN 'FUNC'
                                   WHEN `\FG` THEN 'FUGR'
                                   WHEN `\PR` THEN 'PROG'
                                   WHEN `\TY` THEN 'CLAS' ).

    CASE me->type.
      WHEN 'CLAS'.
        "Interface?
        IF lv_type_kind IS INITIAL.
          FIND REGEX 'TY:([\w/]+)' IN full_name SUBMATCHES DATA(lv_type).  "Inclusief /<namespace>/cl_class...
          "Soms komen er klassen voorbij die een TYPE_NOT_FOUND exception geven. Voorbeeld is /UIF/CL_LREP_LOAD_HANDLER_UT (die
          "gebruik maakt van CL_ABAP_COMPILER). Dit blijkt een testklasse te zijn. Misschien deze er nog een keer uitfilteren...
          TRY.
              CALL METHOD cl_abap_typedescr=>describe_by_name
                EXPORTING
                  p_name         = lv_type
                RECEIVING
                  p_descr_ref    = DATA(lo_descr)
                EXCEPTIONS
                  type_not_found = 1.
              IF sy-subrc = 0 AND lo_descr->kind = 'I'.
                me->type = 'INTF'.
              ENDIF.

            CATCH cx_sy_rtti_syntax_error  ##NO_HANDLER.
          ENDTRY.
        ENDIF.

      WHEN 'FUNC'.
        "Voeg de functiegroep als prefix toe
        DATA(lv_function) = CONV funcname( get_segment( cl_abap_compiler=>tag_function ) ).  "string » char30
        DATA(lv_group) = VALUE rs38l_area( ).
        CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
          CHANGING
            funcname = lv_function
            group    = lv_group
          EXCEPTIONS
            OTHERS   = 0.
        "\FU:FUNC » \FG:FUGR\FU:FUNC
        me->id = |\\FG:{ lv_group }| && me->id.

      WHEN 'PROG'.
        "Klasse-include?
        DATA(ls_name) = CONV progstruc( get_name( ) ).  "string » struct
        IF ls_name-categorya = seop_inctype_class.
          "\PR:ZCLASS========================CP\TY:... » \TY:ZCLASS\TY:...
          me->id = replace( val = me->full_name  sub = |PR:{ get_name( ) }| with = |TY:{ condense( val = ls_name-rootname  del = '=' ) }| ).
          me->type = 'CLAS'.
        ELSE.
          "\PR:ZINCL\FO:FORM » \PR:ZPROG\FO:FORM
          me->id = replace( val = me->full_name  sub = |PR:{ get_name( ) }| with = |PR:{ cl_art_include_services=>derive_mainprog_by_include( get_name( ) ) }| ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD parse_method.

    IF method CS '~'.
      SPLIT method AT '~' INTO DATA(lv_interface) method.
    ENDIF.
    full_name = COND #( WHEN lv_interface IS INITIAL
                        THEN |\\ME:{ method }|
                        ELSE |\\IN:{ lv_interface }\\ME:{ method }| ).

  ENDMETHOD.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_units IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD get.

    "Negeer gegenereerde infotypes
    IF type = 'PROG' AND name CP 'MP*'.
      RETURN.
    ENDIF.

    name = CONV trobj_name( name ).  "csequence » char120

    IF type IS INITIAL.
      DATA(ls_object) = cl_wb_quick_search_value_help=>call_value_help( name ).  "Zelfde routine als optie "Other Object..." in de editor
      type = COND #( WHEN ls_object-wb_type = swbm_c_type_function THEN 'FUNC' ELSE ls_object-object ).
      name = ls_object-obj_name.
    ENDIF.

    CASE type.
      WHEN 'FUNC'.
        SELECT SINGLE @abap_true
          FROM tfdir
         WHERE funcname = @name
          INTO @DATA(lv_exists).

      WHEN 'DEVC'.  "Bijv. $ABAPGIT
        SELECT SINGLE @abap_true
          FROM tdevc
         WHERE devclass = @name
          INTO @lv_exists.

      WHEN OTHERS.
        DATA(lo_repository) = cl_sca_repository_access=>get_local_access( ).
        lv_exists = lo_repository->exists_object( VALUE #( pgmid    = 'R3TR'
                                                           obj_type = type
                                                           obj_name = name ) ).
    ENDCASE.

    IF lv_exists = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_full_name) = SWITCH #( type
                           WHEN 'DEVC' THEN |\\PA:{ name }|   "Package is een eigen concept (niet bekend bij de compiler)
                           WHEN 'CLAS' THEN |\\TYC:{ name }|  "Geef het reeds bekende type (Class/Interface) door
                           WHEN 'INTF' THEN |\\TYI:{ name }|
                           WHEN 'FUNC' THEN |\\FU:{ name }|
                           WHEN 'FUGR' THEN |\\PR:SAPL{ name }|
                           WHEN 'PROG' THEN |\\PR:{ name }| ).
    IF lv_full_name IS INITIAL.
      RETURN.
    ENDIF.

    unit = get_by_full_name( lv_full_name ).

    "Component opgegeven?
    IF comp IS NOT INITIAL.
      unit = unit->get_component( comp ).  "Unit is leeg indien component niet gevonden
    ENDIF.

  ENDMETHOD.

  METHOD get_by_cross_ref.

    DATA:
      lv_program   TYPE program,
      lv_function  TYPE funcname,
      lv_full_name TYPE string.

    DATA(lv_object_cls) = ref-object_cls.
    DATA(lo_repository) = cl_sca_repository_access=>get_local_access( ).

    IF lv_object_cls = 'OM'.  "Mogelijke waarden in tabel EUOBJEDIT
      TRY.
          DATA(lv_method) = lo_repository->get_class_method_by_include( CONV #( ref-object ) )-cpdname.
        CATCH cx_sca_object_not_found.
          "Methode uit lokale klasse, deze worden als programma behandeld
          lv_object_cls = 'P'.
      ENDTRY.
    ENDIF.

    CASE lv_object_cls.
      WHEN 'OM'.
        IF lv_method CS '~'.
          SPLIT lv_method AT '~' INTO DATA(lv_interface) lv_method.
        ENDIF.
        lv_full_name = COND #( WHEN lv_interface IS INITIAL
                               THEN |\\TY:{ ref-encl_objec }\\ME:{ lv_method }|
                               ELSE |\\TY:{ ref-encl_objec }\\IN:{ lv_interface }\\ME:{ lv_method }| ).

      WHEN 'P'.
        DATA(lv_routine) = find_routine( program = ref-program
                                         row     = CONV #( ref-object_row ) ).  "numc6 > int
        "Vervang meteen een include door het hoofdprogramma indien dit eenduidig kan, zo niet dat wordt de oorspronkelijke naam gebruikt
        lv_full_name = |\\PR:{ cl_art_include_services=>derive_mainprog_by_include( ref-program ) }| && lv_routine.

        "Functie?
        IF cl_art_include_services=>include_belongs_to_function( ref-program ).
          lv_program = ref-program.
          CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
            CHANGING
              include  = lv_program
              funcname = lv_function
            EXCEPTIONS
              OTHERS   = 0.
          lv_full_name = lv_full_name && |\\FU:{ lv_function }|.
        ENDIF.

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    unit = get_by_full_name( lv_full_name ).

  ENDMETHOD.

  METHOD get_by_full_name.

    READ TABLE buffer ASSIGNING FIELD-SYMBOL(<ls_buffer>) WITH TABLE KEY id = full_name.
    IF sy-subrc = 0.
      unit = <ls_buffer>-unit.
      RETURN.
    ENDIF.

    unit = NEW #( ).
    unit->init( full_name ).

    INSERT VALUE #( id   = full_name
                    unit = unit ) INTO TABLE buffer.

  ENDMETHOD.

  METHOD find_routine.

    DATA:
      lt_lines TYPE stringtab.

    READ REPORT program INTO lt_lines.

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<lv_line>) TO row.
      IF routine IS INITIAL.
        "Zoek het begin van de routine:
        "  CLASS lclass IMPLEMENTATION.
        "  METHOD linterface~method.
        "  FORM form.
        "  MODULE module INPUT.
        FIND REGEX `^\s*(CLASS|METHOD|FORM|MODULE)\s+([\w~]+)\s*(\w*)\s*`
          IN to_upper( <lv_line> )
          SUBMATCHES DATA(lv_type) DATA(lv_name) DATA(lv_addition).
        CHECK sy-subrc = 0.
        DATA(lv_tag) = SWITCH #( lv_type && SWITCH #( lv_type WHEN 'CLASS' OR 'MODULE' THEN lv_addition )
                                   WHEN `CLASSIMPLEMENTATION` THEN cl_abap_compiler=>tag_class_pool
                                   WHEN `METHOD`              THEN cl_abap_compiler=>tag_method
                                   WHEN `FORM`                THEN cl_abap_compiler=>tag_form
                                   WHEN `MODULEINPUT`         THEN cl_abap_compiler=>tag_module_in
                                   WHEN `MODULEOUTPUT`        THEN cl_abap_compiler=>tag_module_out ).
        CHECK lv_tag IS NOT INITIAL.
        IF lv_tag = cl_abap_compiler=>tag_class_pool.
          DATA(lv_class) = lv_name.
          CONTINUE.
        ENDIF.
        routine = |\\{ lv_tag }:{ lv_name }|.
      ELSE.
        "Of is de routine al weer afgelopen?
        FIND REGEX |^\\s*END{ lv_type }| IN <lv_line> IGNORING CASE.
        IF sy-subrc = 0.
          CLEAR: lv_type, lv_name, lv_addition, routine.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lv_type = 'METHOD'.
      "Vervang een eventuele interface-notatie met ~ door de technische notatie
      IF routine CS '~'.
        REPLACE '\ME:' IN routine WITH '\IN:'.
        REPLACE '~' IN routine WITH '\ME:'.
      ENDIF.
      routine = `\TY:` && lv_class && routine.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_util DEFINITION.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    CLASS-METHODS:
      get_system
        RETURNING VALUE(system) TYPE string,
      get_token
        RETURNING VALUE(token) TYPE string.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_util IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD get_system.

    "Hostnaam inclusief het domein
    cl_http_server=>get_location( IMPORTING host = DATA(lv_host) ).

    "DEV@company.com
    system = |{ sy-sysid }@{ substring_after( val = lv_host  sub = '.' ) }|.

  ENDMETHOD.

  METHOD get_token.

    SELECT token UP TO 1 ROWS
      FROM zxref_auth
      INTO @token.
    ENDSELECT.

  ENDMETHOD.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_task_analyze DEFINITION.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS:
      run
        IMPORTING unit                TYPE REF TO lcl_unit
                  depth_calls         TYPE i OPTIONAL
                  depth_where_used    TYPE i OPTIONAL
                  include_sap_objects TYPE abap_bool OPTIONAL
        RETURNING VALUE(xrefs)        TYPE ts_xrefs.

  PRIVATE SECTION.
    DATA:
      include_sap_objects TYPE abap_bool.

    METHODS:
      get_calls
        IMPORTING unit  TYPE REF TO lcl_unit
                  depth TYPE i
        CHANGING  calls TYPE tt_calls,
      get_where_used
        IMPORTING unit  TYPE REF TO lcl_unit
                  depth TYPE i
        CHANGING  calls TYPE tt_calls.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_task_analyze IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD run.

    me->include_sap_objects = include_sap_objects.

    xrefs = VALUE #( type                = unit->type
                     name                = unit->get_name( )
                     component           = substring_after( val = unit->id  sub = unit->get_name( ) )
                     unit                = unit->id
                     depth_calls         = depth_calls
                     depth_where_used    = depth_where_used
                     include_sap_objects = include_sap_objects
                     system              = lcl_util=>get_system( )
                     release             = sy-saprl
                     date                = sy-datum ).

    DATA(lt_units) = VALUE tt_units( ( unit ) ).
    DATA(lt_calls) = VALUE tt_calls( ).

    "Alleen hoofdobject opgegeven?
    IF unit->get_segment( 2 ) IS INITIAL.
      CASE unit->type.
        WHEN 'DEVC'.
          CLEAR lt_units.
          LOOP AT unit->get_components( ) INTO DATA(lo_component).
            LOOP AT lo_component->get_components( ) INTO DATA(lo_unit).
              INSERT lo_unit INTO TABLE lt_units.
            ENDLOOP.
          ENDLOOP.
        WHEN 'PROG'.
          "Vul aan met lokale routines en klassen
          INSERT LINES OF unit->get_components( ) INTO TABLE lt_units.
        WHEN 'CLAS' OR 'INTF' OR 'FUGR'.
          "Alle aanroepen komen vanuit de componenten
          lt_units = unit->get_components( ).
      ENDCASE.
    ENDIF.

    "Aanroepen
    DATA(lv_index) = 0.
    IF depth_calls > 0.
      LOOP AT lt_units INTO lo_unit.
        "Alternatieve voortgangsindicator is CL_RECA_GUI_STATUS_BAR maar deze bestaat niet op CRM
        cl_crm_bsp_cu_progind=>display( iv_act_value = lv_index
                                        iv_max_value = lines( lt_units )
                                        iv_info_text = |Calls van { lo_unit->id }| ).
        lv_index = lv_index + 1.

        get_calls( EXPORTING unit  = lo_unit
                             depth = depth_calls
                   CHANGING  calls = lt_calls ).
      ENDLOOP.
    ENDIF.

    "Where-used
    lv_index = 0.
    IF depth_where_used > 0.
      LOOP AT lt_units INTO lo_unit.
        cl_crm_bsp_cu_progind=>display( iv_act_value = lv_index
                                        iv_max_value = lines( lt_units )
                                        iv_info_text = |Where-used van { lo_unit->id }| ).
        lv_index = lv_index + 1.

        get_where_used( EXPORTING unit  = lo_unit
                                  depth = depth_where_used
                        CHANGING  calls = lt_calls ).
      ENDLOOP.
    ENDIF.

    "Alle units
    LOOP AT lt_calls ASSIGNING FIELD-SYMBOL(<ls_call>).
      INSERT lcl_units=>get_by_full_name( <ls_call>-source ) INTO TABLE lt_units.
      INSERT lcl_units=>get_by_full_name( <ls_call>-target ) INTO TABLE lt_units.
    ENDLOOP.
    LOOP AT lt_units INTO lo_unit.  "uniek op table_line
      INSERT VALUE #( id      = lo_unit->id
                      sap     = lo_unit->is_standard_sap( )
                      package = lo_unit->get_package( ) ) INTO TABLE xrefs-units.  "uniek op ID

    ENDLOOP.

    "Alle calls
    SORT lt_calls.
    DELETE ADJACENT DUPLICATES FROM lt_calls.
    xrefs-calls = lt_calls.

  ENDMETHOD.

  METHOD get_calls.

    DATA:
      lt_aliases TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line,
      lt_calls   TYPE tt_calls.

    DATA(lo_compiler) = cl_abap_compiler=>create( p_name    = unit->get_program( )
                                                  p_include = unit->get_include( ) ).

    lo_compiler->get_all_refs( EXPORTING p_local    = abap_true  "Inclusief lokale aanroepen
                                         p_types    = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>tag_method )
                                                                                        ( low = cl_abap_compiler=>tag_function )
                                                                                        ( low = cl_abap_compiler=>tag_program )
                                                                                        ( low = cl_abap_compiler=>tag_form ) )
                                         p_grades   = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>grade_direct ) )
                                         p_extended = abap_true  "vult SYMBOL
                               IMPORTING p_result   = DATA(lt_refs) ).

    "Bij programma's zijn alleen de submits relevant, andere referenties worden ge-tagged als form-routine
    DELETE lt_refs WHERE tag = cl_abap_compiler=>tag_program AND mode1 <> cl_abap_compiler=>mode1_submit.

    LOOP AT lt_refs INTO DATA(ls_ref).
      DATA(lo_target) = lcl_units=>get_by_full_name( ls_ref-full_name ).

      "Alleen maatwerk?
      IF me->include_sap_objects = abap_false.
        CHECK NOT lo_target->is_standard_sap( ).
      ENDIF.

      "Geïmplementeerde interface-methode?
      IF unit->id CP '*\IN*\ME*'.
        "De verwijzing naar de interface-methode is niet interessant
        "Dus zoiets: \TY:ZCLASS\IN:ZINTERFACE\ME:METHOD 'calls' \TY:ZINTERFACE\ME:METHOD
        CHECK substring_after( val = unit->id  sub = `IN:` ) <> substring_after( val = lo_target->id  sub = `TY:` ).
      ENDIF.

      IF unit->full_name CP '\PR:*'.
        "De compiler geeft alle calls (op include-niveau) terug maar daarmee is de specifieke routine nog niet bekend
        "Onderstaande aanroep geeft het regelnummer van de aanroep waarmee de routine is te bepalen
        lo_compiler->get_single_ref( EXPORTING p_full_name = ls_ref-full_name
                                               p_grade     = cl_abap_compiler=>grade_components
                                     IMPORTING p_result    = DATA(lt_single_refs) ).

        DATA(lv_routine_ok) = abap_false.
        LOOP AT lt_single_refs INTO DATA(ls_single_ref) WHERE grade = cl_abap_compiler=>grade_direct.
          DATA(lv_routine) = lcl_units=>find_routine( program = ls_ref-include
                                                      row     = ls_single_ref-line ).
          IF substring_after( val = unit->full_name  sub = |PR:{ unit->get_program( ) }| ) = lv_routine.
            lv_routine_ok = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.
        CHECK lv_routine_ok = abap_true.
      ENDIF.

      "Is de methode-aanroep toevallig een alias?
      IF ls_ref-tag = cl_abap_compiler=>tag_method.
        "Deze zijn te herkennen aan een super-methode binnen dezelfde unit
        DATA(lo_method) = CAST cl_abap_comp_method( ls_ref-symbol ).
        IF lo_method->super_method IS BOUND AND
           substring_before( val = lo_method->full_name                sub = `\ME` )   "(\TY:ZCLASS4B)\ME:ALIAS2
         = substring_before( val = lo_method->super_method->full_name  sub = `\IN` ).  "(\TY:ZCLASS4B)\IN:ZINTERFACE4B\ME:METHOD2
          "Zoek omhoog in de klasse hiërarchie om de 1e implementatie te vinden
          "Functie SEO_METHOD_GET_YOUNGEST doet overigens hetzelfde maar dan alleen voor globale klassen
          WHILE lo_method IS BOUND.
            DATA(lv_unit) = `\` && segment( val = lo_method->full_name  index = 2  sep = `\` ).
            lo_target = lcl_units=>get_by_full_name( lv_unit )->get_component( substring_after( val = lo_method->full_name  sub = lv_unit ) ).
            IF lo_target IS BOUND.  "de GET_COMPONENT filtert op geïmplementeerde methodes
              INSERT lo_target->id INTO TABLE lt_aliases.
              EXIT.
            ENDIF.
            lo_method = lo_method->super_method.
          ENDWHILE.
          "Verwerk de (unieke lijst van) aliassen later
          "De compiler geeft aliassen op alle niveau's in de klasse hiërarchie terug
          CONTINUE.
        ENDIF.
      ENDIF.

      INSERT VALUE #( source = unit->id  target = lo_target->id ) INTO TABLE lt_calls.
    ENDLOOP.

    "Verwerk de aliassen
    LOOP AT lt_aliases INTO DATA(lv_alias).
      "Aanroep van de daadwerkelijke implementatie
      INSERT VALUE #( source = unit->id  target = lv_alias ) INTO TABLE lt_calls.

      "Aanroep van de interface (zoals ook gebeurt zonder gebruik van een alias)
      "(\TY:ZCLASS4\IN):ZINTERFACE4B\ME:METHOD2 => \TY:ZINTERFACE4B\ME:METHOD2
      "\PR:ZPROG25(\TY:LCLASS3\IN):LINTERFACE3\ME:METHOD2 => \PR:ZPROG25\TY:LINTERFACE3\ME:METHOD2
      DATA(lv_interface) = replace( val = lv_alias  regex = `\\TY.*\\IN`  with = `\\TY` ).
      lv_unit = `\` && segment( val = lv_interface  index = 2  sep = `\` ).
      lo_target = lcl_units=>get_by_full_name( lv_unit )->get_component( substring_after( val = lv_interface  sub = lv_unit ) ).
      INSERT VALUE #( source = unit->id   target = lo_target->id ) INTO TABLE lt_calls.
    ENDLOOP.

    INSERT LINES OF lt_calls INTO TABLE calls.

    IF depth > 1.
      LOOP AT lt_calls INTO DATA(ls_call).
        CHECK NOT line_exists( calls[ source = ls_call-target ] ).
        get_calls( EXPORTING unit  = lcl_units=>get_by_full_name( ls_call-target )
                             depth = depth - 1
                   CHANGING  calls = calls ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD get_where_used.

    DATA:
      lt_calls TYPE tt_calls,
      lt_scope TYPE stringtab,
      lt_dummy TYPE rinfoobj,
      lt_refs  TYPE sci_findlst.

    lt_scope = VALUE #( ( `PROG` ) ( `FUNC` ) ( `METH` ) ).  "Zie tabel EUOBJ

    CALL FUNCTION 'RS_EU_CROSSREF'
      EXPORTING
        i_find_obj_cls               = ''
        i_full_name                  = unit->full_name
        no_dialog                    = abap_true
        expand_source_in_online_mode = abap_true  "Geeft o.a. het regelnummer in de code terug
      TABLES
        i_findstrings                = lt_dummy  "Zoeken gebeurt via I_FULL_NAME
        i_scope_object_cls           = lt_scope
        o_founds                     = lt_refs
      EXCEPTIONS
        OTHERS                       = 0.

    SORT lt_refs.
    DELETE ADJACENT DUPLICATES FROM lt_refs.
    DELETE lt_refs WHERE grade <> cl_abap_compiler=>grade_direct.  "Alleen aanroepen, geen definities etc.
    DELETE lt_refs WHERE cntnd = abap_true.  "Negeer vervolgregels, alleen de 1e regel van het statement is relevant

    LOOP AT lt_refs INTO DATA(ls_ref).
      DATA(lo_source) = lcl_units=>get_by_cross_ref( ls_ref ).

      "Alleen maatwerk?
      IF me->include_sap_objects = abap_false.
        CHECK NOT lo_source->is_standard_sap( ).
      ENDIF.

      "Bij programma's (zonder specifieke routine) zijn alleen de submits relevant
      IF unit->type = 'PROG' AND unit->get_segment( 2 ) IS INITIAL.
        CHECK to_upper( ls_ref-source ) CS 'SUBMIT'.
      ENDIF.

      "Negeer test-methodes
      IF lo_source->id CP '\TY*\TY*\ME*'.  "in globale klasse
        DATA(ls_name) = CONV progstruc( ls_ref-object ).  "char72 » struct
        IF ls_name-categorya && ls_name-codea = seop_ext_class_testclasses.
          CONTINUE.
        ENDIF.
      ELSEIF lo_source->id CP '\PR*\TY*\ME*'.  "in lokale klasse
        "De GET_COMPONENT filtert test-methodes eruit
        DATA(lv_first) = substring_before( val = lo_source->id  sub = '\TY:' ).
        CHECK lcl_units=>get_by_full_name( lv_first )->get_component( substring_after( val = lo_source->id  sub = lv_first ) ) IS BOUND.
      ENDIF.

      INSERT VALUE #( source = lo_source->id
                      target = unit->id ) INTO TABLE lt_calls.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lt_calls.  "Meerdere identieke calls zijn mogelijk vanuit dezelfde routine
    INSERT LINES OF lt_calls INTO TABLE calls.

    IF depth > 1.
      LOOP AT lt_calls INTO DATA(ls_call).
        CHECK NOT line_exists( calls[ target = ls_call-source ] ).
        get_where_used( EXPORTING unit  = lcl_units=>get_by_full_name( ls_call-source )
                                  depth = depth - 1
                        CHANGING  calls = calls ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_task_show DEFINITION.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS:
      run
        IMPORTING key   TYPE zxref_cache
                  xrefs TYPE ts_xrefs.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_task_show IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD run.

    TYPES:
      BEGIN OF ts_key,
        unit                TYPE string,
        depth_calls         TYPE i,
        depth_where_used    TYPE i,
        include_sap_objects TYPE abap_bool,
        system              TYPE string,
      END OF ts_key.

    IF key IS NOT INITIAL.
      DATA(ls_key) = VALUE ts_key( BASE CORRESPONDING #( key ) system = lcl_util=>get_system( ) ).
      DATA(lv_key_json) = /ui2/cl_json=>serialize( data        = ls_key
                                                   pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    ENDIF.

    IF xrefs IS NOT INITIAL.
      DATA(lv_xrefs_json) = /ui2/cl_json=>serialize( data        = xrefs
                                                     pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    ENDIF.

    "Extra escaping is nodig omdat de json allereerst naar een string wordt geplaatst
    REPLACE ALL OCCURRENCES OF '\' IN lv_key_json   WITH '\\'.
    REPLACE ALL OCCURRENCES OF '\' IN lv_xrefs_json WITH '\\'.

    SET EXTENDED CHECK OFF.
    DATA(lv_html) =
      `<!DOCTYPE html>` &&
      `<html>` &&
      `` &&
      `<head>` &&
      `    <meta charset="UTF-8" />` &&
      `    <script src="https://cdn.jsdelivr.net/npm/lodash@4/lodash.min.js"></script>` &&
      `</head>` &&
      `` &&
      `<body>` &&
      `    <p id="message">Processing...</p>` &&
      `    <script>` &&
      |        const keyJson = '{ lv_key_json }';| &&
      |        const xrefJson = '{ lv_xrefs_json }';| &&
      |        const token = '{ lcl_util=>get_token( ) }';| &&
      `` &&
      `        const message = document.getElementById('message');` &&
      `        let id = '';` &&
      `` &&
      `        (async () => {` &&
      `            if (keyJson) {` &&
      `                const key = JSON.parse(keyJson);` &&
      `                const response = await fetch('https://luukpohlmann.nl/apps/xref-api/xrefs');` &&
      `                const xrefs = await response.json();` &&
      `                const xref = _.find(xrefs, key);` &&
      `                if (!xref) {` &&
      `                    message.textContent = 'Not found';` &&
      `                    return;` &&
      `                }` &&
      `                id = xref.id;` &&
      `            }` &&
      `` &&
      `            if (xrefJson) {` &&
      `                const suffix = (id) ? ('/' + id) : '';` &&
      `                response = await fetch('https://luukpohlmann.nl/apps/xref-api/xrefs' + suffix, {` &&
      `                    method: (id) ? 'PUT' : 'POST',` &&
      `                    headers: {` &&
      `                        authorization: 'Bearer ' + token,` &&
      `                        'content-type': 'application/json',` &&
      `                        accept: 'application/json'` &&
      `                    },` &&
      `                    body: xrefJson` &&
      `                });` &&
      `                result = await response.json();` &&
      `                if (!response.ok) {` &&
      `                    message.textContent = result.message;` &&
      `                    return;` &&
      `                }` &&
      `                id = result.id;` &&
      `            }` &&
      `` &&
      `            location.href = 'https://luukpohlmann.nl/apps/xref/#/' + id;` &&
      `        })();` &&
      `    </script>` &&
      `</body>` &&
      `` &&
      `</html>`.
    SET EXTENDED CHECK ON.

    cl_abap_browser=>show_html(
      html_string = lv_html
      container   = cl_gui_container=>default_screen ).

    "Trigger het scherm
    WRITE space.

  ENDMETHOD.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_task_sync DEFINITION.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS:
      on_pbo,
      on_sapevent FOR EVENT sapevent OF cl_gui_html_viewer
        IMPORTING postdata,
      run.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_task_sync IMPLEMENTATION.
*--------------------------------------------------------------------*
  METHOD on_pbo.

    DATA:
      lv_url TYPE bds_uri.

    DATA(lo_container) = NEW cl_gui_docking_container( side      = cl_gui_docking_container=>dock_at_bottom
                                                       extension = 500
                                                       metric    = cl_gui_control=>metric_pixel ).

    DATA(lo_html_viewer) = NEW cl_gui_html_viewer( parent               = lo_container
                                                   query_table_disabled = abap_true ).  "voorkomt een overflow bij veel data

    SET EXTENDED CHECK OFF.
    DATA(lv_html) =
      `<!DOCTYPE html>` &&
      `<html>` &&
      `` &&
      `<head>` &&
      `    <meta charset="UTF-8" />` &&
      `    <style>` &&
      `        html,` &&
      `        body {` &&
      `            margin: 0;` &&
      `            padding: 0;` &&
      `            width: 100%;` &&
      `            height: 100%;` &&
      `        }` &&
      `` &&
      `        body {` &&
      `            display: grid;` &&
      `            justify-items: center;` &&
      `            align-items: center;` &&
      `        }` &&
      `` &&
      `        form {` &&
      `            display: grid;` &&
      `            justify-items: center;` &&
      `        }` &&
      `    </style>` &&
      `</head>` &&
      `` &&
      `<body>` &&
      `    <form` &&
      `        id="form"` &&
      `        action="sapevent:sync"` &&
      `        method=post` &&
      `    >` &&
      `        <p id="message">...</p>` &&
      `        <input` &&
      `            name="xrefs"` &&
      `            type="hidden"` &&
      `        />` &&
      `        <input` &&
      `            id="button"` &&
      `            type="submit"` &&
      `            value="Synchronize"` &&
      `            disabled="true"` &&
      `        >` &&
      `    </form>` &&
      `` &&
      `    <script>` &&
      |        const system = '{ lcl_util=>get_system( ) }';| &&
      `        const message = document.getElementById('message');` &&
      `` &&
      `        (async () => {` &&
      `            const response = await fetch('https://luukpohlmann.nl/apps/xref-api/xrefs', {` &&
      `                headers: {` &&
      `                    accept: "application/json"` &&
      `                }` &&
      `            });` &&
      `            const result = await response.json();` &&
      `` &&
      `            if (!response.ok) {` &&
      `                message.textContent = result.message;` &&
      `                return;` &&
      `            }` &&
      `` &&
      `            let xrefs = result.filter(item => item.system == system);` &&
      `            message.textContent = xrefs.length + ' already analyzed';` &&
      `            if (xrefs.length) {` &&
      `                form.xrefs.value = window.btoa(JSON.stringify(xrefs));` &&
      `                form.button.disabled = false;` &&
      `            }` &&
      `        })();` &&
      `    </script>` &&
      `</body>` &&
      `` &&
      `</html>`.
    SET EXTENDED CHECK ON.

    DATA(lt_html) = cl_bcs_convert=>string_to_soli( lv_html ).
    lo_html_viewer->load_data( IMPORTING assigned_url = lv_url
                               CHANGING  data_table   = lt_html ).

    lo_html_viewer->set_registered_events( VALUE #( ( eventid = cl_gui_html_viewer=>m_id_sapevent ) ) ).
    SET HANDLER on_sapevent FOR lo_html_viewer.

    lo_html_viewer->show_url( lv_url ).

  ENDMETHOD.

  METHOD on_sapevent.

    DATA:
      lt_xrefs TYPE STANDARD TABLE OF ts_xrefs.

    DATA(lv_xrefs_encoded) = substring_after( val = concat_lines_of( postdata ) sub = 'xrefs=' ) ##NO_TEXT.
    DATA(lv_xrefs_json) = cl_http_utility=>decode_base64( lv_xrefs_encoded ).
    /ui2/cl_json=>deserialize( EXPORTING json        = lv_xrefs_json
                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data        = lt_xrefs ).

    lcl_cache=>fill( CORRESPONDING #( lt_xrefs MAPPING created_on = date ) ).
    MESSAGE s106(sai_cache).
    LEAVE TO SCREEN 0.

  ENDMETHOD.

  METHOD run.

    DATA(ls_popup) = VALUE swftagstru(
      LET w = 50
          h = 5
          x = floor( '0.5' * ( sy-scols - w ) ) - 2
          y = floor( '0.5' * ( sy-srows - h ) ) - 2
      IN  start_col = x
          start_row = y
          end_col   = x + w
          end_row   = y + h ).

    CALL SELECTION-SCREEN 1001
      STARTING AT ls_popup-start_col ls_popup-start_row
      ENDING AT   ls_popup-end_col   ls_popup-end_row.

  ENDMETHOD.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_main DEFINITION.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS:
      run
        IMPORTING unit                TYPE REF TO lcl_unit
                  depth_calls         TYPE i
                  depth_where_used    TYPE i
                  include_sap_objects TYPE abap_bool
                  always_reanalyze    TYPE abap_bool.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD run.

    DATA ls_key TYPE zxref_cache.

    IF always_reanalyze = abap_true.
      ls_key = lcl_cache=>get( unit                = unit->id
                               depth_calls         = depth_calls
                               depth_where_used    = depth_where_used
                               include_sap_objects = include_sap_objects ).
    ELSE.
      ls_key = lcl_cache=>find( unit                = unit->id
                                depth_calls         = depth_calls
                                depth_where_used    = depth_where_used
                                include_sap_objects = include_sap_objects ).
    ENDIF.

    IF ls_key IS INITIAL OR always_reanalyze = abap_true.
      DATA(ls_xrefs) = NEW lcl_task_analyze( )->run( unit                = unit
                                                     depth_calls         = depth_calls
                                                     depth_where_used    = depth_where_used
                                                     include_sap_objects = include_sap_objects ).
      lcl_cache=>add( CORRESPONDING #( ls_xrefs ) ).
    ENDIF.

    NEW lcl_task_show( )->run( key   = ls_key
                               xrefs = ls_xrefs  ).

  ENDMETHOD.

ENDCLASS.

*--------------------------------------------------------------------*
INCLUDE:
*--------------------------------------------------------------------*
  z_xref_test IF FOUND.  "Unittesten

*--------------------------------------------------------------------*
SELECTION-SCREEN
*--------------------------------------------------------------------*
BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-bl1.
PARAMETERS:
  pa_name TYPE trobj_name OBLIGATORY,
  pa_comp TYPE c LENGTH 200.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-bl2.
PARAMETERS:
  pa_dep_c TYPE i DEFAULT 1,
  pa_dep_w TYPE i DEFAULT 1.
SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE TEXT-bl3.
PARAMETERS:
  pa_sap  AS CHECKBOX DEFAULT abap_true,
  pa_rean AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl3.

*--------------------------------------------------------------------*
SELECTION-SCREEN
*--------------------------------------------------------------------*
BEGIN OF SCREEN 1001 TITLE TEXT-sc1 AS WINDOW.
"HTML viewer wordt dynamisch toegevoegd
SELECTION-SCREEN END OF SCREEN 1001.

*--------------------------------------------------------------------*
DATA:
*--------------------------------------------------------------------*
  gt_exclude    TYPE syucomm_t,
  go_task_sync  TYPE REF TO lcl_task_sync,
  go_unit       TYPE REF TO lcl_unit,
  gt_components TYPE name2stringvalue_table.

*--------------------------------------------------------------------*
INITIALIZATION.
*--------------------------------------------------------------------*
  IF lcl_cache=>is_empty( ).
    go_task_sync = NEW lcl_task_sync( ).
    go_task_sync->run( ).
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*--------------------------------------------------------------------*
  PERFORM set_pf_status IN PROGRAM rsdbrunt.  "Standaard GUI status
  gt_exclude = COND #( WHEN sy-dynnr <> '1000' THEN VALUE #( ( 'CRET' ) ( 'NONE' ) ( 'SPOS' ) ) ).  "Execute / Check Input / Save as Variant
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = gt_exclude.

  IF sy-dynnr = '1001'.
    go_task_sync->on_pbo( ).
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK bl1.
*--------------------------------------------------------------------*
  go_unit = lcl_units=>get( name = pa_name
                            comp = pa_comp ).
  IF go_unit IS INITIAL.
    MESSAGE TEXT-e01 TYPE 'E'.
  ENDIF.

  "Lees de naam van de gekozen unit (initieel mag deze wildcards bevatten)
  pa_name = go_unit->get_name( ).
  pa_comp = substring_after( val = go_unit->id  sub = pa_name ).

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_comp.
*--------------------------------------------------------------------*
  "Lees de opgegeven unit naam (onafhankelijk van roundtrip)
  DATA(gt_fields) = VALUE dynpread_t( ( fieldname = 'PA_NAME' ) ).
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = abap_true
    TABLES
      dynpfields         = gt_fields
    EXCEPTIONS
      OTHERS             = 0.

  go_unit = lcl_units=>get( gt_fields[ 1 ]-fieldvalue ).

  IF go_unit IS BOUND.
    gt_components = COND #( WHEN go_unit->type = 'DEVC'
                      THEN VALUE #( FOR c IN go_unit->get_components( ) ( name = c->id ) )
                      ELSE VALUE #( FOR c IN go_unit->get_components( ) ( name = substring_after( val = c->id  sub = go_unit->id ) ) ) ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'NAME'
        value_org   = 'S'
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = 'PA_COMP'
      TABLES
        value_tab   = gt_components
      EXCEPTIONS
        OTHERS      = 0.
  ENDIF.

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*
  NEW lcl_main( )->run( unit                = go_unit
                        depth_calls         = pa_dep_c
                        depth_where_used    = pa_dep_w
                        include_sap_objects = pa_sap
                        always_reanalyze    = pa_rean ).
