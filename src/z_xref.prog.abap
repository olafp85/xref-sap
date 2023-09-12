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
* Known issues
* 1) \PR:ZCM_GET_BENTYPES==============E » Enhancement implementation ZCM_GET_BENTYPES
* 2) In de where-used van PR:/Z_CROSS_REFERENCES/TY:LCL_UNIT/ME:INIT zit nu ook
*    methode LTCL_UNIT/ME:SET_UNIT omdat dit een reguliere methode is
* 3) ZCL_*DIP_DATA->GET_ITEMS heeft nu geen where-used, overigens in lijn
*    met de where-used vanuit de class-builder, terwijl dit wellicht
*    handiger/wenselijk is: <BRON> -> ZIF_*DIP_DATA -> ZCL_*DIP_DATA
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

  BEGIN OF ts_data,
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
  END OF ts_data.

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
          EXPORTING p_local  = abap_true
                    p_types  = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>tag_method ) )
                    p_grades = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>grade_definition ) )
          IMPORTING p_result = DATA(lt_definitions) ).
        "Negeer lokale testklassen
        cl_abap_compiler=>create( get_program( ) )->get_all_refs(
          EXPORTING p_local  = abap_true
                    p_types  = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>tag_type ) )
                    p_grades = VALUE #( sign = 'I' option = 'EQ' ( low = cl_abap_compiler=>grade_definition ) )
          IMPORTING p_result = DATA(lt_local_classes) ).
        LOOP AT lt_local_classes INTO DATA(ls_test_class) WHERE mode1 = cl_abap_compiler=>mode1_test.
          DELETE lt_definitions WHERE full_name CS ls_test_class-full_name.
        ENDLOOP.
        components = VALUE #( BASE components FOR d IN lt_definitions WHERE ( full_name CP '\PR*' ) ( lcl_units=>get_by_full_name( d-full_name ) ) ).

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
          IMPORTING p_result   = lt_definitions ).
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
CLASS lcl_main DEFINITION.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS:
      run
        IMPORTING unit                TYPE REF TO lcl_unit
                  depth_calls         TYPE i OPTIONAL
                  depth_where_used    TYPE i OPTIONAL
                  include_sap_objects TYPE abap_bool OPTIONAL
        RETURNING VALUE(result)       TYPE ts_data.

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
CLASS lcl_main IMPLEMENTATION.
*--------------------------------------------------------------------*

  METHOD run.

    me->include_sap_objects = include_sap_objects.

    cl_http_server=>if_http_server~get_location( IMPORTING host = DATA(lv_host) ).  "Inclusief het domain

    result = VALUE #( type                = unit->type
                      name                = unit->get_name( )
                      component           = substring_after( val = unit->id  sub = unit->get_name( ) )
                      unit                = unit->id
                      depth_calls         = depth_calls
                      depth_where_used    = depth_where_used
                      include_sap_objects = include_sap_objects
                      system              = |{ sy-sysid }@{ match( val = lv_host  regex = '\w+\.\w+$' ) }|  "DEV@company.com
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
                      package = lo_unit->get_package( ) ) INTO TABLE result-units.  "uniek op ID

    ENDLOOP.

    "Alle calls
    SORT lt_calls.
    DELETE ADJACENT DUPLICATES FROM lt_calls.
    result-calls = lt_calls.

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
            IF lo_target IS BOUND.  "de GET_COMPONENT filter op geïmplementeerde methoden
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
CLASS lcl_alv_view DEFINITION.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS:
      show
        IMPORTING data TYPE ts_data,
      on_before_salv_function
                  FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING e_salv_function,
      download.

  PRIVATE SECTION.
    DATA:
      data TYPE ts_data.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_alv_view IMPLEMENTATION.
*--------------------------------------------------------------------*
  METHOD show.

    TRY.
        me->data = data.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = DATA(lo_alv)
          CHANGING  t_table      = me->data-calls ).

        lo_alv->get_columns( )->set_optimize( ).
        lo_alv->get_columns( )->get_column( 'SOURCE' )->set_medium_text( 'Source' ).
        lo_alv->get_columns( )->get_column( 'TARGET' )->set_medium_text( 'Target' ).

        lo_alv->get_layout( )->set_key( VALUE #( report = sy-repid ) ).
        lo_alv->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
        lo_alv->get_layout( )->set_default( abap_true ).

        "Activeer de standaard functie "Local File..."
        lo_alv->get_functions( )->set_export_localfile( ).

        SET HANDLER on_before_salv_function FOR lo_alv->get_event( ).

        lo_alv->display( ).

      CATCH cx_salv_error INTO DATA(lx_alv).
        MESSAGE lx_alv TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD on_before_salv_function.

    "Kaap de standaard functie "Local File..."
    CHECK e_salv_function = '%PC'.

    download( ).

    "Voorkom de standaard afhandeling
    MESSAGE '' TYPE 'E'.

  ENDMETHOD.

  METHOD download.

    DATA:
      lv_file        TYPE string,
      lv_path        TYPE string,
      lv_filename    TYPE string,
      lv_user_action TYPE i.

    DATA(lv_default) = |{ me->data-type }-{ me->data-name }-{ me->data-depth_calls }-{ me->data-depth_where_used }.json|.
    lv_default = translate( val = lv_default from = '/' to = '^' ).
    cl_gui_frontend_services=>file_save_dialog( EXPORTING default_file_name = lv_default
                                                CHANGING  filename          = lv_file
                                                          path              = lv_path
                                                          fullpath          = lv_filename
                                                          user_action       = lv_user_action ).
    IF lv_user_action = cl_gui_frontend_services=>action_cancel.
      RETURN.
    ENDIF.

    "Download as JSON
    DATA(lv_json) = /ui2/cl_json=>serialize( data          = me->data
                                             pretty_name   = abap_true  "Camel case
                                             "format_output = abap_true
                                             ).
    DATA(lt_json) = cl_bcs_convert=>string_to_soli( lv_json ).
    cl_gui_frontend_services=>gui_download( EXPORTING  filename = lv_filename
                                                       write_lf = abap_false
                                            CHANGING   data_tab = lt_json
                                            EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc <> 0.
      "Error writing file
      MESSAGE e016(fes).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_web_view DEFINITION.
*--------------------------------------------------------------------*

  PUBLIC SECTION.
    METHODS:
      show
        IMPORTING data TYPE ts_data.

ENDCLASS.

*--------------------------------------------------------------------*
CLASS lcl_web_view IMPLEMENTATION.
*--------------------------------------------------------------------*
  METHOD show.

    DATA:
      lv_url  TYPE swk_url,
      lt_html TYPE STANDARD TABLE OF so_text255 WITH DEFAULT KEY.

    DATA(lv_json) = /ui2/cl_json=>serialize( data        = data
                                             pretty_name = abap_true ).  "Camel case

    "Extra escaping is nodig omdat de json allereerst naar een string wordt geplaatst
    REPLACE ALL OCCURRENCES OF '\' IN lv_json WITH '\\'.
    DATA(lt_json) = cl_bcs_convert=>string_to_soli( lv_json ).

    DATA(lv_html) =
      `<!DOCTYPE html>` &&
      `<html>` &&
      `` &&
      `<head>` &&
      `    <meta charset="UTF-8" />` &&
      `    <style>` &&
      `        html {` &&
      `            height: 100%;` &&
      `        }` &&
      `` &&
      `        body {` &&
      `            height: 100%;` &&
      `            margin: 0;` &&
      `            font-family: Arial, Helvetica, sans-serif;` &&
      `            display: grid;` &&
      `            justify-items: center;` &&
      `            align-items: center;` &&
      `            background-color: #3a3a3a;` &&
      `        }` &&
      `` &&
      `        main {` &&
      `            width: 50%;` &&
      `            height: 50%;` &&
      `            display: grid;` &&
      `            justify-items: center;` &&
      `            align-items: center;` &&
      `            background-color: white;` &&
      `            border-radius: 7px;` &&
      `            box-shadow: 0px 0px 5px 2px black;` &&
      `        }` &&
      `` &&
      `        #login-form {` &&
      `            align-self: flex-start;` &&
      `            display: grid;` &&
      `            justify-items: center;` &&
      `            align-items: center;` &&
      `        }` &&
      `` &&
      `        #login-error {` &&
      `            color: red;` &&
      `            opacity: 0;` &&
      `        }` &&
      `` &&
      `        .login-form-field {` &&
      `            border: none;` &&
      `            border-bottom: 1px solid #3a3a3a;` &&
      `            margin-bottom: 10px;` &&
      `            border-radius: 3px;` &&
      `            outline: none;` &&
      `            padding: 0px 0px 5px 5px;` &&
      `        }` &&
      `` &&
      `        #login-form-submit {` &&
      `            width: 100%;` &&
      `            padding: 7px;` &&
      `            border: none;` &&
      `            border-radius: 5px;` &&
      `            color: white;` &&
      `            font-weight: bold;` &&
      `            background-color: #3a3a3a;` &&
      `            cursor: pointer;` &&
      `            outline: none;` &&
      `        }` &&
      `    </style>` &&
      `</head>` &&
      `` &&
      `<body>` &&
      `    <main>` &&
      `        <h1 id="login-header">Login</h1>` &&
      `        <form id="login-form">` &&
      `            <p id="login-error">Invalid credentials</p>` &&
      `            <input type="email" name="email" class="login-form-field" placeholder="E-mail" required>` &&
      `            <input type="password" name="password" class="login-form-field" placeholder="Password" required>` &&
      `            <input type="submit" value="Login" id="login-form-submit">` &&
      `        </form>` &&
      `    </main>` &&
      `</body>` &&
      `` &&
      `<script>` &&
      `    const loginForm = document.getElementById('login-form');` &&
      `    const loginError = document.getElementById("login-error");` &&
      `` &&
      `    loginForm.onsubmit = async (event) => {` &&
      `        event.preventDefault();` &&
      `        loginError.style.opacity = 0;` &&
      `` &&
      `        let response = await fetch('https://luukpohlmann.nl/apps/xref-api/login', {` &&
      `            method: 'POST',` &&
      `            body: new FormData(loginForm)` &&
      `        });` &&
      `        if (!response.ok) {` &&
      `            loginError.style.opacity = 1;` &&
      `            return;` &&
      `        }` &&
      `` &&
      `        let result = await response.json();` &&
      |        const json = '{ lv_json }';| &&
      `` &&
      `        response = await fetch('https://luukpohlmann.nl/apps/xref-api/xrefs', {` &&
      `            method: 'POST',` &&
      `            headers: {` &&
      `                authorization: 'Bearer ' + result.token,` &&
      `                "content-type": "application/json",` &&
      `                accept: "application/json"` &&
      `            },` &&
      `            body: json` &&
      `        });` &&
      `` &&
      `        result = await response.json();` &&
      `` &&
      `        if (!response.ok) {` &&
      `            loginError.textContent = result.message;` &&
      `            loginError.style.opacity = 1;` &&
      `            return;` &&
      `        }` &&
      `` &&
      `        location.href = 'https://luukpohlmann.nl/apps/xref/#/' + result.id;` &&
      `    };` &&
      `</script>` &&
      `` &&
      `</html>`.

    cl_abap_browser=>show_html(
      html_string = lv_html
      container   = cl_gui_container=>default_screen ).

    "Trigger the default screen
    WRITE space.

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
  pa_sap AS CHECKBOX DEFAULT abap_true,
  cb_web RADIOBUTTON GROUP a,
  cb_alv RADIOBUTTON GROUP a.
SELECTION-SCREEN END OF BLOCK bl3.

*--------------------------------------------------------------------*
DATA:
*--------------------------------------------------------------------*
  go_unit       TYPE REF TO lcl_unit,
  gt_components TYPE name2stringvalue_table.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK bl1.
*--------------------------------------------------------------------*
  go_unit = lcl_units=>get( name = pa_name
                            comp = pa_comp ).
  IF go_unit IS INITIAL.
    MESSAGE 'Unit niet gevonden' TYPE 'E'.
  ENDIF.

  "Lees de naam van de gekozen unit (initieel mag deze wildcards bevatten)
  pa_name = go_unit->get_name( ).
  pa_comp = substring_after( val = go_unit->id  sub = pa_name ).

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_comp.
*--------------------------------------------------------------------*
  TRY.
      pa_name = to_upper( cl_cnv_frm_ui_shlp_utils=>get_parameter_value( 'PA_NAME' ) ).  "Actuele waarde (onafhankelijk van roundtrip)
      go_unit = lcl_units=>get( pa_name ).

    CATCH cx_cnv_frm_ui INTO DATA(gx_exc).
      MESSAGE gx_exc->msg TYPE 'E'.
  ENDTRY.

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
  DATA(gs_result) = NEW lcl_main( )->run( unit                = go_unit
                                          depth_calls         = pa_dep_c
                                          depth_where_used    = pa_dep_w
                                          include_sap_objects = pa_sap ).

  CASE abap_true.
    WHEN cb_web.
      NEW lcl_web_view( )->show( gs_result ).
    WHEN cb_alv.
      NEW lcl_alv_view( )->show( gs_result ).
  ENDCASE.
