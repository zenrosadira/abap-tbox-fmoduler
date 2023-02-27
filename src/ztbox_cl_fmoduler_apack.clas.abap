class ZTBOX_CL_FMODULER_APACK definition
  public
  final
  create public .

public section.

  interfaces ZIF_APACK_MANIFEST .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZTBOX_CL_FMODULER_APACK IMPLEMENTATION.


  METHOD constructor.

    zif_apack_manifest~descriptor = VALUE #(
      group_id    = 'ztbox'
      artifact_id = 'abap-tbox-fmoduler'
      version     = '0.1'
      git_url     = 'https://github.com/zenrosadira/abap-tbox-fmoduler.git' ).

  ENDMETHOD.
ENDCLASS.
