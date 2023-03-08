class ZTBOX_CL_FMODULER definition
  public
  final
  create public

  global friends ZTBOX_CL_FMOD_PARAM .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .

  types:
    BEGIN OF ty_funct_except,
             subrc   TYPE sy-subrc,
             except  TYPE rs38l_par_,
             message TYPE string,
           END OF ty_funct_Except .

  data FUNCTION_PARAMETERS type ABAP_FUNC_PARMBIND_TAB .

  class-methods CLASS_CONSTRUCTOR .
  methods EXECUTE
    exporting
      !EV_RC type SY-SUBRC .
  methods CONSTRUCTOR
    importing
      !I_FUNCTION_NAME type FUNCNAME .
  methods GET_ERRORS
    returning
      value(R_ERRS) type STRING_TABLE .
  methods GET_PARAM
    importing
      !I_NAME type RS38L_PAR_
    returning
      value(R_RES) type ref to ZTBOX_CL_FMOD_PARAM .
  methods EXCEPTION
    returning
      value(R_EXCEPT) type TY_FUNCT_EXCEPT .
protected section.
PRIVATE SECTION.

  TYPES:
    ty_generic_types_t TYPE RANGE OF rs38l_typ .
  TYPES:
    BEGIN OF ty_passed_params,
      parameter TYPE fupararef-parameter,
      param_obj TYPE REF TO ztbox_cl_fmod_param,
    END OF ty_passed_params .
  TYPES:
    ty_passed_params_t TYPE TABLE OF ty_passed_params WITH DEFAULT KEY .
  TYPES ty_signature_t TYPE TABLE OF fupararef WITH DEFAULT KEY.

  DATA _except_table TYPE abap_func_excpbind_tab .
  DATA _function_name TYPE funcname .
  DATA _errors TYPE string_table .
  CLASS-DATA _generic_types TYPE ty_generic_types_t .
  DATA _signature TYPE ty_signature_t .
  CONSTANTS c_status_active TYPE r3state VALUE 'A' ##NO_TEXT.
  DATA _passed_params TYPE ty_passed_params_t .
  DATA _exception TYPE ty_funct_except .

  METHODS _set_signature .
  METHODS _add_error
    IMPORTING
      !i_err TYPE string OPTIONAL .
  CLASS-METHODS _set_generic_types .
  METHODS _set_exceptions .
  METHODS _set_parameters .
  METHODS _get_kind
    IMPORTING
      !i_kind       TYPE rs38l_kind
    RETURNING
      VALUE(r_kind) TYPE i .
  CLASS-METHODS _get_value_reference
    IMPORTING
      !i_sign      TYPE fupararef
    RETURNING
      VALUE(r_ref) TYPE REF TO data .
  METHODS _get_std_msg
    RETURNING
      VALUE(r_msg) TYPE string .
ENDCLASS.



CLASS ZTBOX_CL_FMODULER IMPLEMENTATION.


  METHOD constructor.

    _function_name = i_function_name.

    _set_signature( ).

    _set_exceptions( ).

  ENDMETHOD.


  METHOD execute.

    CLEAR _errors.

    _set_parameters( ).

    TRY.

        CLEAR ev_rc.
        CALL FUNCTION _function_name PARAMETER-TABLE function_parameters EXCEPTION-TABLE _except_table.
        IF sy-subrc NE 0.

          _exception = VALUE #(
            subrc   = sy-subrc
            except  = _except_table[ value = sy-subrc ]-name
            message = _get_std_msg( ) ).

          ev_rc = sy-subrc.

        ENDIF.

      CATCH cx_root INTO DATA(lx_root).
        _add_error( lx_root->get_text( ) ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_errors.

    r_errs = _errors.

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

    CLEAR _signature.

    SELECT *
      FROM fupararef INTO CORRESPONDING FIELDS OF TABLE @_signature
      WHERE funcname EQ @_function_name
        AND r3state  EQ @c_status_active.

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


  METHOD _set_exceptions.

    SORT _signature BY paramtype DESCENDING.

    LOOP AT _signature INTO DATA(excep) WHERE paramtype EQ abap_true.

      _except_table = VALUE #( BASE _except_table (
        name  = excep-parameter
        value = sy-tabix ) ).

    ENDLOOP.

    _except_table = VALUE #( BASE _except_table (
      name  = 'ERROR_MESSAGE'
      value = -1 ) ).

    DELETE _signature WHERE paramtype EQ abap_true.

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

    function_parameters = VALUE #(
      FOR param IN _passed_params
      ( name  = param-parameter
        value = param-param_obj->_value
        kind  = _get_kind( param-param_obj->_sign-paramtype ) ) ).

  ENDMETHOD.


  METHOD class_constructor.

    _set_generic_types( ).

  ENDMETHOD.


  METHOD exception.

    r_except = _exception.

  ENDMETHOD.


  METHOD get_param.

    DATA(sign) = _signature[ parameter = i_name ].

    r_res = NEW #(
      i_sign  = sign
      i_fmod  = me ).

  ENDMETHOD.


  METHOD _get_std_msg.

    CLEAR r_msg.
    CHECK sy-msgty IS NOT INITIAL.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO r_msg.

  ENDMETHOD.


  METHOD _get_value_reference.

    CHECK i_sign-structure NOT IN _generic_types.

    IF i_sign-structure EQ space.

      CREATE DATA r_ref TYPE string.

    ELSEIF i_sign-ref_class EQ abap_true.

      CREATE DATA r_ref TYPE REF TO (i_sign-structure).

    ELSEIF i_sign-type EQ abap_false AND i_sign-paramtype EQ 'T'.

      CREATE DATA r_ref TYPE TABLE OF (i_sign-structure).

    ELSE.

      CREATE DATA r_ref TYPE (i_sign-structure).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
