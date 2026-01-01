# abap-serializer

Serialize any ABAP data to `xstring` and back, preserving full type information.

## Features

- Supports all elementary types (string, int, date, time, packed, float, hex, decfloat, utclong, etc.)
- Supports structures (including nested and with INCLUDEs)
- Supports all table types (standard, sorted, hashed) with keys
- Supports secondary keys (unique and non-unique)
- Supports nested tables
- Interface-based design for easy mocking in unit tests

## Use Cases

- Storing complex typed data in cluster databases (INDX tables)
- Binary payloads for RFC/HTTP communication
- Caching in shared memory areas
- Persisting session state

## Installation

Install using [abapGit](https://abapgit.org/):

1. Open abapGit in your SAP system
2. Create a new online repository with this URL
3. Select a target package and pull

## Usage

```abap
DATA serializer TYPE REF TO zif_serializer.
serializer = NEW zcl_serializer( ).

" Serialize any data
DATA(my_data) = VALUE some_structure( field1 = 'A' field2 = 123 ).
DATA(xstr) = serializer->serialize( REF #( my_data ) ).

" Deserialize back
DATA(result) = serializer->deserialize( xstr ).
ASSIGN result->* TO FIELD-SYMBOL(<data>).
```

## API

### `serialize`
```abap
METHODS serialize
  IMPORTING data TYPE REF TO data
  RETURNING VALUE(result) TYPE xstring
  RAISING cx_sy_dyn_call_illegal_type.
```

### `deserialize`
```abap
METHODS deserialize
  IMPORTING data TYPE xstring
  RETURNING VALUE(result) TYPE REF TO data
  RAISING cx_sy_import_mismatch_error
         cx_sy_struct_creation
         cx_sy_table_creation
         cx_sy_dyn_call_illegal_type
         cx_parameter_invalid_range.
```

## Testing

The class includes 28 unit tests covering all supported types and edge cases. Run them using ABAP Unit in your system.

## License

MIT
