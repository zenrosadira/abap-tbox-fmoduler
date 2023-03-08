class ZTBOX_CL_FMOD_PARAM definition
  public
  final
  create private

  global friends ZTBOX_CL_FMODULER .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .

  methods SET_VALUE
    importing
      !I_VALUE type ANY optional
    returning
      value(R_RES) type ref to ZTBOX_CL_FMOD_PARAM .
  methods GET_VALUE
    changing
      !C_VALUE type ANY .
  methods IS_OPTIONAL
    returning
      value(R_RES) type FLAG .
  methods DEFAULT_VALUE
    returning
      value(R_RES) type STRING .
  methods IS_GENERIC_TYPED
    returning
      value(R_RES) type FLAG .
  methods KIND
    returning
      value(R_RES) type I .
protected section.
private section.

  data _VALUE type ref to DATA .
  data _SIGN type FUPARAREF .
  data _FMOD type ref to ZTBOX_CL_FMODULER .

  methods CONSTRUCTOR
    importing
      !I_SIGN type FUPARAREF
      !I_FMOD type ref to ZTBOX_CL_FMODULER .
ENDCLASS.



CLASS ZTBOX_CL_FMOD_PARAM IMPLEMENTATION.


  METHOD constructor.

    _sign = i_sign.
    _fmod = i_fmod.

  ENDMETHOD.


  METHOD default_value.

    ASSIGN (_sign-defaultval) TO FIELD-SYMBOL(<def>).

    r_res = COND #(
      WHEN <def> IS ASSIGNED THEN <def>
      ELSE _sign-defaultval ).

    REPLACE ALL OCCURRENCES OF |'| IN r_res WITH ||.

  ENDMETHOD.


  METHOD get_value.

    CHECK _value IS BOUND.
    c_value = _value->*.

  ENDMETHOD.


  METHOD is_generic_typed.

    r_res = xsdbool( _sign-structure IN ztbox_cl_fmoduler=>_generic_types ).

  ENDMETHOD.


  METHOD is_optional.

    r_Res = _sign-optional.

  ENDMETHOD.


  METHOD kind.

    r_res = _fmod->_get_kind( _sign-paramtype ).

  ENDMETHOD.


  METHOD set_value.

    _value = ztbox_cl_fmoduler=>_get_value_reference( _sign ).

    IF _value IS NOT BOUND.
      CREATE DATA _value LIKE i_value.
    ENDIF.

    _value->* = i_value.

    APPEND VALUE #(
      parameter = _sign-parameter
      param_obj = me ) TO _fmod->_passed_params.

    r_res = me.

  ENDMETHOD.
ENDCLASS.
