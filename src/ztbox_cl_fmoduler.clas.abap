class ZTBOX_CL_FMODULER definition
  public
  final
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .

  data FUNCTION_PARAMETERS type ABAP_FUNC_PARMBIND_TAB .

  methods ADD_PARAMETER
    importing
      !I_PARAM_VALUE type ANY
      !I_PARAM_NAME type STRING .
  methods GET_PARAMETER
    importing
      !I_PARAM_NAME type STRING
    changing
      !C_PARAM_VALUE type ANY optional .
  methods EXECUTE
    exporting
      !EV_RC type SY-SUBRC .
  methods CONSTRUCTOR
    importing
      !I_FUNCTION_NAME type FUNCNAME .
  methods GET_ERRORS
    returning
      value(R_ERRS) type STRING_TABLE .
protected section.
private section.

  types:
    ty_generic_types_t TYPE RANGE OF rs38l_typ .
  types:
    BEGIN OF ty_signature,
      parameter  TYPE fupararef-parameter,
      paramtype  TYPE fupararef-paramtype,
      structure  TYPE fupararef-structure,
      type       TYPE fupararef-type,
      ref_class  TYPE fupararef-ref_class,
      defaultval TYPE fupararef-defaultval,
    END OF ty_signature .
  types:
    ty_signature_t TYPE TABLE OF ty_signature .

  data _EXCEPT_TABLE type ABAP_FUNC_EXCPBIND_TAB .
  data _FUNCTION_NAME type FUNCNAME .
  data _ERRORS type STRING_TABLE .
  data _GENERIC_TYPES type TY_GENERIC_TYPES_T .
  data _SIGNATURE type TY_SIGNATURE_T .

  methods _SET_SIGNATURE .
  methods _ADD_ERROR
    importing
      !I_ERR type STRING optional .
  methods _SET_GENERIC_TYPES .
  methods _SET_EXCEPTIONS .
  methods _SET_PARAMETERS .
  methods _SET_DEFAULT_VALUES .
  methods _GET_KIND
    importing
      !I_KIND type RS38L_KIND
    returning
      value(R_KIND) type I .
ENDCLASS.



CLASS ZTBOX_CL_FMODULER IMPLEMENTATION.


  METHOD add_parameter.

    DATA lo_ref TYPE REF TO data.

    READ TABLE function_parameters ASSIGNING FIELD-SYMBOL(<fs_par>) WITH KEY name = i_param_name.
    IF sy-subrc EQ 0.

      IF <fs_par>-value IS NOT BOUND.
        CREATE DATA <fs_par>-value LIKE i_param_value.
      ENDIF.

      ASSIGN <fs_par>-value->* TO FIELD-SYMBOL(<fs_param_value>).
      <fs_param_value> = i_param_value.

    ELSE.

      DATA(lv_type) = VALUE #( _signature[ parameter = i_param_name ]-paramtype OPTIONAL ).
      CHECK lv_type IS NOT INITIAL.

      CREATE DATA lo_ref LIKE i_param_value.
      GET REFERENCE OF i_param_value INTO lo_ref.

      function_parameters = VALUE #( BASE function_parameters (
        name  = i_param_name
        kind  = _get_kind( lv_type )
        value = lo_ref ) ).

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    _function_name = i_function_name.

    _set_generic_types( ).

    _set_signature( ).

  ENDMETHOD.


  METHOD execute.

    CLEAR _errors.

    TRY.

        CLEAR ev_rc.
        CALL FUNCTION _function_name PARAMETER-TABLE function_parameters EXCEPTION-TABLE _except_table.
        IF sy-subrc NE 0.
          ev_rc = sy-subrc.
          _add_error( ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_root).
        _add_error( lx_root->get_text( ) ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_errors.

    r_errs = _errors.

  ENDMETHOD.


  METHOD get_parameter.

    READ TABLE function_parameters ASSIGNING FIELD-SYMBOL(<fs_param>) WITH KEY name = i_param_name.
    CHECK sy-subrc EQ 0.

    IF <fs_param>-value IS BOUND.

      ASSIGN <fs_param>-value->* TO FIELD-SYMBOL(<fs_val>).
      c_param_value = <fs_val>.

    ELSE.

      CREATE DATA <fs_param>-value LIKE c_param_value.
      GET REFERENCE OF c_param_value INTO <fs_param>-value.

    ENDIF.

  ENDMETHOD.


  METHOD _add_error.

    IF i_err IS SUPPLIED.

      INSERT i_err INTO TABLE _errors.

    ELSE.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_err).
      INSERT lv_err INTO TABLE _errors.

    ENDIF.

  ENDMETHOD.


  METHOD _set_signature.

    SELECT parameter, paramtype, structure, type, ref_class, defaultval
      FROM fupararef INTO TABLE @_signature
      WHERE funcname EQ @_function_name
        AND r3state  EQ 'A'.
    CHECK sy-subrc EQ 0.

    _set_exceptions( ).

    _set_parameters( ).

    _set_default_values( ).

  ENDMETHOD.


  METHOD _get_kind.

    CASE i_kind.

      WHEN 'I'.
        r_kind = abap_func_exporting.

      WHEN 'E'.
        r_kind = abap_func_importing.

      WHEN 'C'.
        r_kind = abap_func_changing.

      WHEN 'T'.
        r_kind = abap_func_tables.

    ENDCASE.

  ENDMETHOD.


  METHOD _set_default_values.

    DATA lv_def TYPE string.

    LOOP AT function_parameters ASSIGNING FIELD-SYMBOL(<fs_para_def>) WHERE value IS BOUND.

      READ TABLE _signature INTO DATA(ls_sign_def) WITH KEY parameter = <fs_para_def>-name.
      IF sy-subrc NE 0 OR ls_sign_def-defaultval IS INITIAL.
        CONTINUE.
      ENDIF.

      CLEAR lv_def.
      lv_def = ls_sign_def-defaultval.

      REPLACE ALL OCCURRENCES OF |'| IN lv_def WITH space.
      CONDENSE lv_def.

      IF strlen( lv_def ) > 3 AND lv_def(3) EQ 'SY-'.
        ASSIGN (lv_def) TO FIELD-SYMBOL(<fs_def>).
        lv_def = <fs_def>.
      ELSEIF lv_def EQ 'SPACE'.
        lv_def = ' '.
      ENDIF.

      ASSIGN <fs_para_def>-value->* TO FIELD-SYMBOL(<fs_val>).
      <fs_val> = lv_def.

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_exceptions.

    DATA lv_ix TYPE i.

    LOOP AT _signature INTO DATA(ls_excep) WHERE paramtype EQ abap_true.
      lv_ix += 1.

      _except_table = VALUE #( BASE _except_table (
        name  = ls_excep-parameter
        value = lv_ix ) ).
    ENDLOOP.

    _except_table = VALUE #( BASE _except_table (
            name  = 'ERROR_MESSAGE'
            value = lv_ix + 1 ) ).

  ENDMETHOD.


  METHOD _set_generic_types.

    _generic_types = VALUE #( sign = 'I' option = 'EQ'
      ( low = 'ANY' )
      ( low = 'ANY TABLE' )
      ( low = 'HASHED TABLE' )
      ( low = 'INDEX TABLE' )
      ( low = 'STANDARD TABLE' )
      ( low = 'SORTED TABLE' )
      ( low = 'TABLE' )
      ( low = 'DATA' )
      ( low = 'NUMERIC' )
      ( low = 'CLIKE' )
      ( low = 'CSEQUENCE' )
      ( low = 'XSEQUENCE' )
      ( low = 'SIMPLE' )
      ( low = 'DECFLOAT' )
      ( low = 'C' )
      ( low = 'N' )
      ( low = 'P' )
      ( low = 'X' ) ).

  ENDMETHOD.


  METHOD _set_parameters.

    DATA lo_ref TYPE REF TO data.

    LOOP AT _signature INTO DATA(ls_sign) WHERE paramtype NE abap_true AND structure NOT IN _generic_types.

      CLEAR lo_ref.

      IF ls_sign-structure EQ space.
        CREATE DATA lo_ref TYPE string.

      ELSEIF ls_sign-ref_class EQ abap_true.
        CREATE DATA lo_ref TYPE REF TO (ls_sign-structure).

      ELSEIF ls_sign-type EQ abap_false AND ls_sign-paramtype EQ 'T'.
        CREATE DATA lo_ref TYPE TABLE OF (ls_sign-structure).

      ELSE.
        CREATE DATA lo_ref TYPE (ls_sign-structure).

      ENDIF.

      function_parameters = VALUE #( BASE function_parameters (
        name  = ls_sign-parameter
        kind  = _get_kind( ls_sign-paramtype )
        value = lo_ref ) ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
