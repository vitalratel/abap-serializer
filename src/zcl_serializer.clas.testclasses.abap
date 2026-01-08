CLASS ltc_serializer DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zif_serializer.

    METHODS setup.
    METHODS serialize_string FOR TESTING.
    METHODS serialize_integer FOR TESTING.
    METHODS serialize_date FOR TESTING.
    METHODS serialize_packed FOR TESTING.
    METHODS serialize_structure FOR TESTING.
    METHODS serialize_table FOR TESTING.
    METHODS serialize_table_empty_key FOR TESTING.
    METHODS serialize_nested_structure FOR TESTING.
    METHODS serialize_nested_table FOR TESTING.
    METHODS serialize_hashed_table FOR TESTING.
    METHODS serialize_sorted_table FOR TESTING.
    METHODS serialize_secondary_key FOR TESTING.
    METHODS serialize_secondary_unique FOR TESTING.
    METHODS serialize_float FOR TESTING.
    METHODS serialize_time FOR TESTING.
    METHODS serialize_xstring FOR TESTING.
    METHODS serialize_numc FOR TESTING.
    METHODS serialize_char FOR TESTING.
    METHODS serialize_hex FOR TESTING.
    METHODS serialize_decfloat16 FOR TESTING.
    METHODS serialize_decfloat34 FOR TESTING.
    METHODS serialize_int1 FOR TESTING.
    METHODS serialize_int2 FOR TESTING.
    METHODS serialize_int8 FOR TESTING.
    METHODS serialize_utclong FOR TESTING.
    METHODS serialize_empty_table FOR TESTING.
    METHODS serialize_composite_key FOR TESTING.
    METHODS serialize_include FOR TESTING.
    METHODS serialize_deep_nesting FOR TESTING.
    METHODS serialize_initial_values FOR TESTING.
    METHODS serialize_max_min_values FOR TESTING.
    METHODS serialize_long_string FOR TESTING.
    METHODS deserialize_corrupted FOR TESTING.
    METHODS deserialize_initial FOR TESTING.
    METHODS deserialize_unbound FOR TESTING.

ENDCLASS.


CLASS ltc_serializer IMPLEMENTATION.

  METHOD setup.
    cut = NEW zcl_serializer( ).
  ENDMETHOD.


  METHOD serialize_string.
    DATA(value) = `Test string`.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_integer.
    DATA(value) = 42.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_date.
    DATA(value) = sy-datum.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_packed.
    DATA value TYPE p LENGTH 10 DECIMALS 2 VALUE '12345.67'.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_structure.
    TYPES:
      BEGIN OF ty_struct,
        name  TYPE string,
        value TYPE i,
      END OF ty_struct.

    DATA(value) = VALUE ty_struct( name = `Test` value = 123 ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_table.
    TYPES ty_table TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    DATA(value) = VALUE ty_table( ( 1 ) ( 2 ) ( 3 ) ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_table_empty_key.
    TYPES ty_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA(value) = VALUE ty_table( ( `A` ) ( `B` ) ( `C` ) ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_nested_structure.
    TYPES:
      BEGIN OF ty_inner,
        field TYPE string,
      END OF ty_inner,
      BEGIN OF ty_outer,
        inner TYPE ty_inner,
        value TYPE i,
      END OF ty_outer.

    DATA(value) = VALUE ty_outer(
      inner = VALUE #( field = `Nested` )
      value = 99 ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_nested_table.
    TYPES:
      ty_inner TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      BEGIN OF ty_struct,
        items TYPE ty_inner,
      END OF ty_struct,
      ty_outer TYPE STANDARD TABLE OF ty_struct WITH DEFAULT KEY.

    DATA(value) = VALUE ty_outer(
      ( items = VALUE #( ( `A` ) ( `B` ) ) )
      ( items = VALUE #( ( `C` ) ( `D` ) ( `E` ) ) ) ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_hashed_table.
    TYPES:
      BEGIN OF ty_entry,
        key   TYPE string,
        value TYPE i,
      END OF ty_entry,
      ty_table TYPE HASHED TABLE OF ty_entry WITH UNIQUE KEY key.

    DATA(value) = VALUE ty_table(
      ( key = `A` value = 1 )
      ( key = `B` value = 2 ) ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_sorted_table.
    TYPES:
      BEGIN OF ty_entry,
        key   TYPE string,
        value TYPE i,
      END OF ty_entry,
      ty_table TYPE SORTED TABLE OF ty_entry WITH UNIQUE KEY key.

    DATA(value) = VALUE ty_table(
      ( key = `A` value = 1 )
      ( key = `B` value = 2 ) ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_secondary_key.
    TYPES:
      BEGIN OF ty_entry,
        id    TYPE i,
        name  TYPE string,
        value TYPE i,
      END OF ty_entry,
      ty_table TYPE SORTED TABLE OF ty_entry
        WITH UNIQUE KEY id
        WITH NON-UNIQUE SORTED KEY by_name COMPONENTS name.

    DATA(value) = VALUE ty_table(
      ( id = 1 name = `Alpha` value = 10 )
      ( id = 2 name = `Alpha` value = 20 )
      ( id = 3 name = `Beta`  value = 30 ) ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_secondary_unique.
    TYPES:
      BEGIN OF ty_entry,
        id   TYPE i,
        code TYPE string,
        name TYPE string,
      END OF ty_entry,
      ty_table TYPE HASHED TABLE OF ty_entry
        WITH UNIQUE KEY id
        WITH UNIQUE HASHED KEY by_code COMPONENTS code.

    DATA(value) = VALUE ty_table(
      ( id = 1 code = `A01` name = `First` )
      ( id = 2 code = `B02` name = `Second` ) ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_float.
    DATA value TYPE f VALUE '3.14159265358979'.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_time.
    DATA(value) = sy-uzeit.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_xstring.
    DATA value TYPE xstring VALUE '48454C4C4F'.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_numc.
    DATA value TYPE n LENGTH 10 VALUE '0000012345'.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_char.
    DATA value TYPE c LENGTH 20 VALUE 'Fixed length char'.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_hex.
    DATA value TYPE x LENGTH 8 VALUE 'DEADBEEFCAFE0123'.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_decfloat16.
    DATA value TYPE decfloat16 VALUE '1234567890.123456'.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_decfloat34.
    DATA value TYPE decfloat34 VALUE '12345678901234567890.12345678901234'.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_int1.
    DATA value TYPE int1 VALUE 255.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_int2.
    DATA value TYPE int2 VALUE 32767.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_int8.
    DATA value TYPE int8 VALUE 9223372036854775807.
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_utclong.
    DATA value TYPE utclong.
    value = utclong_current( ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_empty_table.
    TYPES ty_table TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    DATA(value) = VALUE ty_table( ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_composite_key.
    TYPES:
      BEGIN OF ty_entry,
        key1  TYPE string,
        key2  TYPE i,
        value TYPE string,
      END OF ty_entry,
      ty_table TYPE SORTED TABLE OF ty_entry WITH UNIQUE KEY key1 key2.

    DATA(value) = VALUE ty_table(
      ( key1 = `A` key2 = 1 value = `A1` )
      ( key1 = `A` key2 = 2 value = `A2` )
      ( key1 = `B` key2 = 1 value = `B1` ) ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_include.
    TYPES:
      BEGIN OF ty_address,
        street TYPE string,
        city   TYPE string,
      END OF ty_address,
      BEGIN OF ty_person.
        INCLUDE TYPE ty_address.
    TYPES:
        name TYPE string,
      END OF ty_person.

    DATA(value) = VALUE ty_person(
      street = `Main St`
      city   = `Berlin`
      name   = `John` ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_deep_nesting.
    TYPES:
      BEGIN OF ty_level3,
        value TYPE string,
      END OF ty_level3,
      BEGIN OF ty_level2,
        level3 TYPE ty_level3,
      END OF ty_level2,
      BEGIN OF ty_level1,
        level2 TYPE ty_level2,
      END OF ty_level1,
      BEGIN OF ty_root,
        level1 TYPE ty_level1,
      END OF ty_root.

    DATA(value) = VALUE ty_root(
      level1 = VALUE #(
        level2 = VALUE #(
          level3 = VALUE #( value = `Deep` ) ) ) ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_initial_values.
    TYPES:
      BEGIN OF ty_struct,
        str  TYPE string,
        int  TYPE i,
        date TYPE d,
        time TYPE t,
        dec  TYPE p LENGTH 10 DECIMALS 2,
      END OF ty_struct.

    DATA(value) = VALUE ty_struct( ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_max_min_values.
    TYPES:
      BEGIN OF ty_struct,
        max_int  TYPE i,
        min_int  TYPE i,
        max_int8 TYPE int8,
        min_int8 TYPE int8,
      END OF ty_struct.

    DATA(value) = VALUE ty_struct(
      max_int  = 2147483647
      min_int  = -2147483648
      max_int8 = 9223372036854775807
      min_int8 = -9223372036854775808 ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD serialize_long_string.
    DATA(value) = repeat( val = `ABCDEFGHIJ` occ = 10000 ).
    DATA(ref) = REF #( value ).

    DATA(serialized) = cut->serialize( ref ).
    DATA(result) = cut->deserialize( serialized ).

    ASSIGN result->* TO FIELD-SYMBOL(<result>).
    cl_abap_unit_assert=>assert_equals(
      exp = value
      act = <result> ).
  ENDMETHOD.


  METHOD deserialize_corrupted.
    DATA(corrupted) = CONV xstring( 'DEADBEEF' ).

    TRY.
        cut->deserialize( corrupted ).
        cl_abap_unit_assert=>fail( `Expected exception not raised` ).
      CATCH cx_sy_import_mismatch_error.
        " Expected
    ENDTRY.
  ENDMETHOD.


  METHOD deserialize_initial.
    DATA(result) = cut->deserialize( VALUE #( ) ).

    cl_abap_unit_assert=>assert_not_bound( result ).
  ENDMETHOD.


  METHOD deserialize_unbound.
    DATA ref TYPE REF TO data.

    DATA(result) = cut->serialize( ref ).

    cl_abap_unit_assert=>assert_initial( result ).
  ENDMETHOD.

ENDCLASS.