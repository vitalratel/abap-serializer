class ZCL_SERIALIZER definition
  public
  final
  create public .

public section.

  INTERFACES zif_serializer.
protected section.
private section.

  types:
    begin of ty_buffer,
        descr TYPE xstring,
        value TYPE xstring,
      end of ty_buffer .
  types:
    begin of ty_component,
        name       TYPE abap_compname,
        descr      TYPE xstring,
        as_include TYPE abap_bool,
        suffix     TYPE string,
      end of ty_component .
  types:
    ty_components TYPE STANDARD TABLE OF ty_component WITH DEFAULT KEY .
  types:
    begin of ty_descriptor,
        kind  TYPE abap_typecategory,
        keys  TYPE xstring,
        descr TYPE xstring,
      end of ty_descriptor .

  methods BYTE_TO_CHAR_LENGTH
    importing
      !TYPE_KIND type ABAP_TYPEKIND
      !LENGTH type I
    returning
      value(RESULT) type I .
  methods COMPRESS
    importing
      !TYPE type ref to CL_ABAP_TYPEDESCR
    returning
      value(RESULT) type XSTRING
    raising
      CX_SY_DYN_CALL_ILLEGAL_TYPE .
  methods COMPRESS_ELEMENT
    importing
      !TYPE type ref to CL_ABAP_ELEMDESCR
    returning
      value(RESULT) type XSTRING .
  methods COMPRESS_STRUCTURE
    importing
      !TYPE type ref to CL_ABAP_STRUCTDESCR
    returning
      value(RESULT) type XSTRING
    raising
      CX_SY_DYN_CALL_ILLEGAL_TYPE .
  methods COMPRESS_TABLE
    importing
      !TYPE type ref to CL_ABAP_TABLEDESCR
    returning
      value(RESULT) type TY_DESCRIPTOR
    raising
      CX_SY_DYN_CALL_ILLEGAL_TYPE .
  methods CREATE_ELEMENT_TYPE
    importing
      !TYPE_KIND type ABAP_TYPEKIND
      !LENGTH type I
      !DECIMALS type I
    returning
      value(RESULT) type ref to CL_ABAP_ELEMDESCR
    raising
      CX_SY_DYN_CALL_ILLEGAL_TYPE
      CX_PARAMETER_INVALID_RANGE .
  methods DECOMPRESS
    importing
      !DESCRIPTOR type XSTRING
    returning
      value(RESULT) type ref to CL_ABAP_DATADESCR
    raising
      CX_SY_IMPORT_MISMATCH_ERROR
      CX_SY_STRUCT_CREATION
      CX_SY_TABLE_CREATION
      CX_SY_DYN_CALL_ILLEGAL_TYPE
      CX_PARAMETER_INVALID_RANGE .
  methods DECOMPRESS_ELEMENT
    importing
      !DESCRIPTOR type TY_DESCRIPTOR
    returning
      value(RESULT) type ref to CL_ABAP_DATADESCR
    raising
      CX_SY_IMPORT_MISMATCH_ERROR
      CX_SY_DYN_CALL_ILLEGAL_TYPE
      CX_PARAMETER_INVALID_RANGE .
  methods DECOMPRESS_STRUCTURE
    importing
      !DESCRIPTOR type TY_DESCRIPTOR
    returning
      value(RESULT) type ref to CL_ABAP_DATADESCR
    raising
      CX_SY_IMPORT_MISMATCH_ERROR
      CX_SY_STRUCT_CREATION
      CX_SY_TABLE_CREATION
      CX_SY_DYN_CALL_ILLEGAL_TYPE
      CX_PARAMETER_INVALID_RANGE .
  methods DECOMPRESS_TABLE
    importing
      !DESCRIPTOR type TY_DESCRIPTOR
    returning
      value(RESULT) type ref to CL_ABAP_DATADESCR
    raising
      CX_SY_IMPORT_MISMATCH_ERROR
      CX_SY_STRUCT_CREATION
      CX_SY_TABLE_CREATION
      CX_SY_DYN_CALL_ILLEGAL_TYPE
      CX_PARAMETER_INVALID_RANGE .
ENDCLASS.



CLASS ZCL_SERIALIZER IMPLEMENTATION.


METHOD zif_serializer~deserialize.

  IF data IS INITIAL.
    RETURN.
  ENDIF.

  DATA(buffer) = VALUE ty_buffer( ).
  IMPORT p1 = buffer FROM DATA BUFFER data.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE cx_sy_import_mismatch_error.
  ENDIF.

  DATA(type_descr) = decompress( buffer-descr ).

  IF type_descr IS NOT BOUND.
    RAISE EXCEPTION TYPE cx_sy_import_mismatch_error.
  ENDIF.

  CREATE DATA result TYPE HANDLE type_descr.
  ASSIGN result->* TO FIELD-SYMBOL(<data>).
  IMPORT p1 = <data> FROM DATA BUFFER buffer-value.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE cx_sy_import_mismatch_error.
  ENDIF.

ENDMETHOD.


METHOD zif_serializer~serialize.

  IF data IS NOT BOUND.
    RETURN.
  ENDIF.

  DATA(buffer) = VALUE ty_buffer( ).
  ASSIGN data->* TO FIELD-SYMBOL(<data>).
  EXPORT p1 = <data> TO DATA BUFFER buffer-value.

  DATA(type_descr) = cl_abap_typedescr=>describe_by_data_ref( data ).

  buffer-descr = compress( type_descr ).

  EXPORT p1 = buffer TO DATA BUFFER result.

ENDMETHOD.


METHOD byte_to_char_length.

  CASE type_kind.
    WHEN cl_abap_typedescr=>typekind_char
      OR cl_abap_typedescr=>typekind_num
      OR cl_abap_typedescr=>typekind_date
      OR cl_abap_typedescr=>typekind_time.
      IF cl_abap_char_utilities=>charsize > 1.
        result = length DIV cl_abap_char_utilities=>charsize.
        RETURN.
      ENDIF.
  ENDCASE.

  result = length.

ENDMETHOD.


METHOD compress.

  DATA(descr) = VALUE ty_descriptor( kind = type->kind ).

  CASE type->kind.
    WHEN cl_abap_typedescr=>kind_elem.
      descr-descr = compress_element( CAST #( type ) ).

    WHEN cl_abap_typedescr=>kind_struct.
      descr-descr = compress_structure( CAST #( type ) ).

    WHEN cl_abap_typedescr=>kind_table.
      descr = compress_table( CAST #( type ) ).

    WHEN OTHERS.
      RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_type.
  ENDCASE.

  EXPORT p1 = descr TO DATA BUFFER result.

ENDMETHOD.


METHOD compress_element.

  DATA(abap_descr) = VALUE abap_compdescr(
    type_kind = type->type_kind
    length    = type->length
    decimals  = type->decimals ).

  EXPORT p1 = abap_descr TO DATA BUFFER result.

ENDMETHOD.


METHOD compress_structure.

  DATA(components) = VALUE ty_components(
    FOR comp IN type->get_components( )
    ( name       = comp-name
      descr      = compress( comp-type )
      as_include = comp-as_include
      suffix     = comp-suffix ) ).

  EXPORT p1 = components TO DATA BUFFER result.

ENDMETHOD.


METHOD compress_table.

  DATA(line_type) = type->get_table_line_type( ).
  DATA(keys) = type->get_keys( ).

  EXPORT p1 = keys TO DATA BUFFER result-keys.
  result-kind = type->kind.
  result-descr = compress( line_type ).

ENDMETHOD.


METHOD create_element_type.

  DATA(len) = byte_to_char_length(
    type_kind = type_kind
    length    = length ).

  CASE type_kind.
    WHEN cl_abap_typedescr=>typekind_num.
      result = cl_abap_elemdescr=>get_n( len ).

    WHEN cl_abap_typedescr=>typekind_char.
      result = cl_abap_elemdescr=>get_c( len ).

    WHEN cl_abap_typedescr=>typekind_packed.
      result = cl_abap_elemdescr=>get_p(
        p_length   = len
        p_decimals = decimals ).

    WHEN cl_abap_typedescr=>typekind_date.
      result = cl_abap_elemdescr=>get_d( ).

    WHEN cl_abap_typedescr=>typekind_float.
      result = cl_abap_elemdescr=>get_f( ).

    WHEN cl_abap_typedescr=>typekind_int.
      result = cl_abap_elemdescr=>get_i( ).

    WHEN cl_abap_typedescr=>typekind_int1.
      result = cl_abap_elemdescr=>get_int1( ).

    WHEN cl_abap_typedescr=>typekind_int2.
      result = cl_abap_elemdescr=>get_int2( ).

    WHEN cl_abap_typedescr=>typekind_int8.
      result = cl_abap_elemdescr=>get_int8( ).

    WHEN cl_abap_typedescr=>typekind_time.
      result = cl_abap_elemdescr=>get_t( ).

    WHEN cl_abap_typedescr=>typekind_string.
      result = cl_abap_elemdescr=>get_string( ).

    WHEN cl_abap_typedescr=>typekind_xstring.
      result = cl_abap_elemdescr=>get_xstring( ).

    WHEN cl_abap_typedescr=>typekind_hex.
      result = cl_abap_elemdescr=>get_x( len ).

    WHEN cl_abap_typedescr=>typekind_decfloat16.
      result = cl_abap_elemdescr=>get_decfloat16( ).

    WHEN cl_abap_typedescr=>typekind_decfloat34.
      result = cl_abap_elemdescr=>get_decfloat34( ).

    WHEN cl_abap_typedescr=>typekind_utclong.
      result = cl_abap_elemdescr=>get_utclong( ).

    WHEN OTHERS.
      RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_type.

  ENDCASE.

ENDMETHOD.


METHOD decompress.

  IF descriptor IS INITIAL.
    RETURN.
  ENDIF.

  DATA(descr) = VALUE ty_descriptor( ).
  IMPORT p1 = descr FROM DATA BUFFER descriptor.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE cx_sy_import_mismatch_error.
  ENDIF.

  CASE descr-kind.
    WHEN cl_abap_typedescr=>kind_elem.
      result = decompress_element( descr ).

    WHEN cl_abap_typedescr=>kind_struct.
      result = decompress_structure( descr ).

    WHEN cl_abap_typedescr=>kind_table.
      result = decompress_table( descr ).

    WHEN OTHERS.
      RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_type.
  ENDCASE.

ENDMETHOD.


METHOD decompress_element.

  DATA(abap_descr) = VALUE abap_compdescr( ).
  IMPORT p1 = abap_descr FROM DATA BUFFER descriptor-descr.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE cx_sy_import_mismatch_error.
  ENDIF.

  result = create_element_type(
    type_kind = abap_descr-type_kind
    length    = abap_descr-length
    decimals  = abap_descr-decimals ).

ENDMETHOD.


METHOD decompress_structure.

  DATA(comp_list) = VALUE ty_components( ).
  IMPORT p1 = comp_list FROM DATA BUFFER descriptor-descr.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE cx_sy_import_mismatch_error.
  ENDIF.

  DATA(components) = VALUE abap_component_tab(
    FOR comp IN comp_list
    ( name       = comp-name
      type       = decompress( comp-descr )
      as_include = comp-as_include
      suffix     = comp-suffix ) ).

  result = cl_abap_structdescr=>get( p_components = components ).

ENDMETHOD.


METHOD decompress_table.

  DATA(keys) = VALUE abap_table_keydescr_tab( ).
  IMPORT p1 = keys FROM DATA BUFFER descriptor-keys.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE cx_sy_import_mismatch_error.
  ENDIF.

  READ TABLE keys ASSIGNING FIELD-SYMBOL(<key>)
    WITH KEY is_primary = abap_true.
  IF <key> IS ASSIGNED AND <key>-components IS NOT INITIAL.
    <key>-key_kind = cl_abap_tabledescr=>keydefkind_user.
  ENDIF.

  DATA(line_type) = decompress( descriptor-descr ).

  result = cl_abap_tabledescr=>get_with_keys(
    p_line_type = line_type
    p_keys      = keys ).

ENDMETHOD.
ENDCLASS.