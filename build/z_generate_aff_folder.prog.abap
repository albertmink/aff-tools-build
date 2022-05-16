*&---------------------------------------------------------------------*
*& Report z_generate_json_schema
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_generate_aff_folder.
INTERFACE zif_aff_writer DEFERRED.
INTERFACE zif_aff_log DEFERRED.
CLASS zcl_aff_writer_xslt DEFINITION DEFERRED.
CLASS zcl_aff_writer_json_schema DEFINITION DEFERRED.
CLASS zcl_aff_writer DEFINITION DEFERRED.
CLASS zcl_aff_test_types DEFINITION DEFERRED.
CLASS zcl_aff_log DEFINITION DEFERRED.
CLASS zcl_aff_generator DEFINITION DEFERRED.
CLASS zcl_aff_abap_doc_reader DEFINITION DEFERRED.
CLASS zcl_aff_abap_doc_parser DEFINITION DEFERRED.
CLASS zcx_aff_tools DEFINITION
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    METHODS constructor
      IMPORTING
        message   TYPE string OPTIONAL
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.
CLASS zcx_aff_tools IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    " trigger task
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF message IS NOT INITIAL.
      cl_message_helper=>set_msg_vars_for_clike( message ).
      if_t100_message~t100key = VALUE #( msgid = 'ZAFF_TOOLS'
                                         msgno = '000'
                                         attr1 = 'IF_T100_DYN_MSG~MSGV1'
                                         attr2 = 'IF_T100_DYN_MSG~MSGV2'
                                         attr3 = 'IF_T100_DYN_MSG~MSGV3'
                                         attr4 = 'IF_T100_DYN_MSG~MSGV4' ).
      if_t100_dyn_msg~msgty = 'E'.
      if_t100_dyn_msg~msgv1 = sy-msgv1.
      if_t100_dyn_msg~msgv2 = sy-msgv2.
      if_t100_dyn_msg~msgv3 = sy-msgv3.
      if_t100_dyn_msg~msgv4 = sy-msgv4.
    ELSEIF textid IS INITIAL.
      if_t100_message~t100key = VALUE #( msgid = 'ZAFF_TOOLS' msgno = '001' ).
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

INTERFACE zif_aff_log.

  CONSTANTS:
    BEGIN OF c_message_type,
      error   TYPE symsgty VALUE 'E',
      warning TYPE symsgty VALUE 'W',
      info    TYPE symsgty VALUE 'I',
    END OF c_message_type.

  TYPES:
    "! A single message entry in the log
    BEGIN OF ty_log_out,
      "! The name of the component for which the message was logged
      component_name TYPE string,
      "! The type of the message
      type           TYPE symsgty,
      "! The text of the message
      text           TYPE string,
      "! The message
      message        TYPE symsg,
    END OF ty_log_out,
    tt_log_out TYPE STANDARD TABLE OF ty_log_out WITH NON-UNIQUE DEFAULT KEY.

  METHODS:
    "! Adds an info message (type I) to the log.
    "!
    "! @parameter message | the message
    "! @parameter component_name | the name of the element for which the log entry is created
    add_info
      IMPORTING message        TYPE symsg
                component_name TYPE string,

    "! Adds a warning message (type W) to the log.
    "!
    "! @parameter message | the message
    "! @parameter component_name | the name of the element for which the log entry is created
    add_warning
      IMPORTING message        TYPE symsg
                component_name TYPE string,

    "! Adds an error message (type E) to the log.
    "!
    "! @parameter message | the message
    "! @parameter component_name | the name of the element for which the log entry is created
    add_error
      IMPORTING message        TYPE symsg
                component_name TYPE string,

    "! Adds an exception to the log. Actually not the exception is added
    "! but the message of the exception. The message type can be submitted.
    "!
    "! @parameter exception | the exception containing the message
    "! @parameter message_type | the type of the message
    "! @parameter component_name | the name of the element for which the log entry is created
    add_exception
      IMPORTING exception      TYPE REF TO cx_root
                message_type   TYPE symsgty DEFAULT c_message_type-error
                component_name TYPE string,

    "! Returns the logged messages. The log is NOT cleared afterwards.
    "! The caller has to {@link METH.clear} it in case it should be reused.
    "!
    "! @parameter messages | the logged messages
    get_messages
      RETURNING VALUE(messages) TYPE tt_log_out,

    "! Join the messages of another log with this log. Afterwards this log contains
    "! the messages of the other log.
    "!
    "! @parameter log_to_join | the other log
    join
      IMPORTING log_to_join TYPE REF TO zif_aff_log,

    "! Clears all messages of this log.
    "!
    clear,

    "! Calculates the maximum severity of the logged messages.
    "! This is (in order):
    "! E - Error
    "! W - Warning
    "! I - Information
    "!
    "! @parameter max_severity | the maximum severity of the logged messages
    get_max_severity
      RETURNING VALUE(max_severity) TYPE symsgty,

    "! Returns true if the log contains messages, false otherwise.
    "!
    "! @parameter has_messages | true or false
    has_messages
      RETURNING VALUE(has_messages) TYPE abap_bool.

ENDINTERFACE.

INTERFACE zif_aff_writer.

  TYPES:
    BEGIN OF ENUM enum_formatting_option STRUCTURE formatting_option,
      no_formatting VALUE IS INITIAL,
      camel_case    VALUE 1,
    END OF ENUM enum_formatting_option STRUCTURE formatting_option.

  TYPES:
    BEGIN OF ENUM enum_type_info STRUCTURE type_info,
      string,
      numeric,
      boolean,
      date_time,
    END OF ENUM enum_type_info STRUCTURE type_info.

  TYPES:
    BEGIN OF ENUM enum_operation STRUCTURE operation,
      initial,
      write_element,
      open_structure,
      close_structure,
      open_table,
      close_table,
    END OF ENUM enum_operation STRUCTURE operation.

  TYPES:
    BEGIN OF ty_name_mapping,
      abap TYPE abap_compname,
      json TYPE string,
    END OF ty_name_mapping,
    ty_name_mappings TYPE HASHED TABLE OF ty_name_mapping WITH UNIQUE KEY abap.

  TYPES:
    BEGIN OF ty_value_mapping,
      abap TYPE string,
      json TYPE string,
    END OF ty_value_mapping,
    ty_value_mappings TYPE HASHED TABLE OF ty_value_mapping WITH UNIQUE KEY abap.

  TYPES:
    BEGIN OF ty_abap_value_mapping,
      abap_element   TYPE abap_compname,
      target_type    TYPE enum_type_info,
      value_mappings TYPE ty_value_mappings,
    END OF ty_abap_value_mapping,
    ty_abap_value_mappings TYPE HASHED TABLE OF ty_abap_value_mapping WITH UNIQUE KEY abap_element.

  METHODS:
    set_name_mappings
      IMPORTING
        name_mappings TYPE ty_name_mappings,

    set_abap_value_mappings
      IMPORTING
        abap_value_mappings TYPE ty_abap_value_mappings,

    set_formatting_option
      IMPORTING
        formatting_option TYPE zif_aff_writer=>enum_formatting_option,

    write_element
      IMPORTING
        element_name        TYPE string
        element_description TYPE REF TO cl_abap_elemdescr
      RAISING
        zcx_aff_tools,

    "! open node. for example table or object
    "! @parameter node_description | description for node
    open_node
      IMPORTING
        node_description TYPE REF TO cl_abap_typedescr
        node_name        TYPE string
      RAISING
        zcx_aff_tools,

    "! close node. for example table or object
    "! @parameter node_description | description for node
    close_node
      IMPORTING
        node_description TYPE REF TO cl_abap_typedescr
        node_name        TYPE string
      RAISING
        zcx_aff_tools,

    get_output
      RETURNING VALUE(result) TYPE string_table,

    get_log
      RETURNING VALUE(log) TYPE REF TO zif_aff_log,

    "! Validate the given source and writes messages into the log
    "!
    "! @parameter source | Source to validate
    "! @parameter log | log to write messages
    "! @parameter result | true, if the source is valid, false if not
    validate
      IMPORTING source        TYPE string_table
                log           TYPE REF TO zif_aff_log
      RETURNING VALUE(result) TYPE abap_bool,

    open_include
      IMPORTING
        include_description TYPE REF TO cl_abap_structdescr,

    close_include.

ENDINTERFACE.

CLASS zcl_aff_abap_doc_parser DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF abap_doc_annotation,
                 callback_class    TYPE string VALUE `$callbackClass`,
                 default           TYPE string VALUE `$default`,
                 values            TYPE string VALUE `$values`,
                 required          TYPE string VALUE `$required`,
                 show_always       TYPE string VALUE `$showAlways`,
                 minimum           TYPE string VALUE `$minimum`,
                 maximum           TYPE string VALUE `$maximum`,
                 exclusive_minimum TYPE string VALUE `$exclusiveMinimum`,
                 exclusive_maximum TYPE string VALUE `$exclusiveMaximum`,
                 max_length        TYPE string VALUE `$maxLength`,
                 min_length        TYPE string VALUE `$minLength`,
                 multiple_of       TYPE string VALUE `$multipleOf`,
                 enum_value        TYPE string VALUE `$enumValue`,
               END OF abap_doc_annotation.

    TYPES:
      BEGIN OF abap_doc,
        required          TYPE abap_bool,
        showalways        TYPE abap_bool,
        title             TYPE string,
        description       TYPE string,
        enumvalues_link   TYPE string,
        minimum           TYPE string,
        maximum           TYPE string,
        exclusive_minimum TYPE string,
        exclusive_maximum TYPE string,
        multiple_of       TYPE string,
        default           TYPE string,
        min_length        TYPE string,
        max_length        TYPE string,
        callback_class    TYPE string,
        enum_value        TYPE string,
      END OF abap_doc.

    METHODS: parse
      IMPORTING
        component_name        TYPE string
        to_parse              TYPE string
      CHANGING
        log                   TYPE REF TO zif_aff_log
      RETURNING
        VALUE(found_abap_doc) TYPE abap_doc.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_mixed_table_entry,
        offset  TYPE i,
        length  TYPE i,
        is_link TYPE abap_boolean,
      END OF ty_mixed_table_entry,
      tt_mixed_table_entry TYPE SORTED TABLE OF ty_mixed_table_entry WITH UNIQUE KEY offset.

    CONSTANTS co_shorttext_tag_open TYPE string VALUE `[\s]*<p\sclass="shorttext">` ##NO_TEXT.

    DATA abap_doc_string TYPE string.
    DATA parser_log TYPE REF TO zif_aff_log.
    DATA component_name TYPE string.
    DATA decoded_abap_doc TYPE abap_doc.
    DATA description_warning_is_needed TYPE abap_boolean.

    METHODS: parse_title,
      parse_description,
      remove_leading_trailing_spaces
        CHANGING string_to_work_on TYPE string,
      parse_annotations,
      parse_callback_class,
      get_annotation_value
        IMPORTING
          length                  TYPE i
          offset                  TYPE i
          to_decode               TYPE string
          length_of_annotation    TYPE i
          remove_whitespaces      TYPE abap_boolean
        RETURNING
          VALUE(annotation_value) TYPE string,
      parse_default,
      parse_enum_values,
      parse_required,
      parse_show_always,
      parse_number_annotations
        IMPORTING
          key_word TYPE string,
      get_number_annotation
        IMPORTING
          annotation_name TYPE string
        RETURNING
          VALUE(number)   TYPE string,
      check_next_word
        IMPORTING
          offset        TYPE i
          text_to_check TYPE string,
      write_description_message,
      workaround_remove_titles,
      check_title_positions,
      parse_enum_value,
      write_log_for_multiple_entries
        IMPORTING
          result_table TYPE match_result_tab
          annotaion    TYPE string.

ENDCLASS.
CLASS zcl_aff_abap_doc_reader DEFINITION
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS:
      create_instance
        IMPORTING
          source        TYPE string_table
        RETURNING
          VALUE(result) TYPE REF TO zcl_aff_abap_doc_reader.
    METHODS get_abap_doc_for_element
      IMPORTING
        element_name  TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        cx_static_check.
  PROTECTED SECTION.
    DATA
      source TYPE string_table.
ENDCLASS.
CLASS zcl_aff_generator DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        writer TYPE REF TO zif_aff_writer.

    METHODS generate_type
      IMPORTING
        data          TYPE data
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        zcx_aff_tools.

    METHODS get_log
      RETURNING
        VALUE(log) TYPE REF TO zif_aff_log.

  PRIVATE SECTION.
    DATA:
      writer TYPE REF TO zif_aff_writer,
      log    TYPE REF TO zif_aff_log.

    METHODS:
      check_input
        IMPORTING
          type_description TYPE REF TO cl_abap_typedescr,
      process_type_description
        IMPORTING
          type_description TYPE REF TO cl_abap_typedescr
          type_name        TYPE string OPTIONAL
        RAISING
          zcx_aff_tools,
      process_element
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          element_name        TYPE string OPTIONAL
        RAISING
          zcx_aff_tools,
      process_structure
        IMPORTING
          structure_description TYPE REF TO cl_abap_structdescr
          structure_name        TYPE string
        RAISING
          zcx_aff_tools,
      process_table
        IMPORTING
          table_description TYPE REF TO cl_abap_tabledescr
          table_name        TYPE string
        RAISING
          zcx_aff_tools,
      process_include
        IMPORTING
          structure_description TYPE REF TO cl_abap_structdescr
        RAISING
          zcx_aff_tools,
      process_components
        IMPORTING
          components TYPE cl_abap_structdescr=>component_table
        RAISING
          zcx_aff_tools,
      check_mandatory_fields
        IMPORTING
          structure_description TYPE REF TO cl_abap_structdescr.

ENDCLASS.
CLASS zcl_aff_log DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_aff_log.

    CLASS-METHODS:
      "! Writes the actual system message fields into the returned structure
      "!
      "! @parameter result | The actual system message
      get_sy_message
        RETURNING VALUE(result) TYPE symsg.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      messages     TYPE zif_aff_log=>tt_log_out,
      max_severity TYPE symsgty.

    METHODS:
      add_message
        IMPORTING
          type           TYPE symsgty
          message        TYPE symsg
          component_name TYPE string,
      set_max_severity
        IMPORTING
          type TYPE symsgty.
ENDCLASS.
CLASS zcl_aff_test_types DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      "! $hiddenabc
      unknown_annotation TYPE string.

    TYPES:
      "! <p class="shorttext">title</p>
      "! description
      "! <p class="shorttext2">Title</p>
      description_not_found TYPE string.

    TYPES:
      "! <p class="shorttext">Type With Long Description</p>
      "! This is a type with a very very long description.
      "! To ensure that the generated schema can be inserted into an include without loss of information,
      "! the length of the description should not be longer then 253 characters.
      "! So please shorten your description. Description should not replace a lexicon.
      type_with_long_description TYPE i.

    TYPES:
      "! Default type does not match constant type
      "! $default {@link zcl_aff_test_types.data:co_test.test}
      default_link TYPE i.
    CONSTANTS:
      BEGIN OF co_test,
        test TYPE string VALUE ' ',
      END OF co_test.

    TYPES:
      BEGIN OF struc_link_wrong_type,
        default_link TYPE default_link,
      END OF struc_link_wrong_type.
    TYPES:
      "! in ST val(I()) only allow integers
      "! $values {@link zcl_aff_test_types.data:co_enum}
      enum TYPE i.

    CONSTANTS:
      BEGIN OF co_enum,
        test  TYPE string VALUE ' ',
        test2 TYPE string VALUE 'A',
      END OF co_enum.

*  type to test name mapping from format_version to formatVersion
    TYPES:
      "! <p class="shorttext">Constant With Field Format Version</p>
      "! Constant with field format_version
      BEGIN OF ty_format_version,
        "! <p class="shorttext">ABAP File Format Version</p>
        "! The ABAP file format version
        format_version TYPE string,
        "! <p class="shorttext">Other Field</p>
        "! Other field
        field1         TYPE i,
      END OF ty_format_version.

*  numerical text field
    TYPES:
      "! <p   class="shorttext">Numerical Text Field</p>
      "! A numerical text field of length 4
      num_text TYPE n LENGTH 4.

    TYPES:
      "! <p   class="shorttext">Structure With Numerical Text Field</p>
      "! Structure with a numerical text field of length 4
      BEGIN OF struc_with_num_text,
        numerical_text1 TYPE num_text,
        numerical_text2 TYPE num_text,
        "! $showAlways
        numerical_text3 TYPE num_text,
        numerical_text4 TYPE num_text,
      END OF struc_with_num_text.
*  numerical text field with title and description which need to be escaped in the json schema
*    " needs to be escaped (/")
*    \ needs to be escaped (\\)
    TYPES:
      "! <p   class="shorttext" lang="en"   >Test title "\</p>
      "! Test description "\
      num_text1 TYPE n LENGTH 4.
* integer
    TYPES:
      "! <p   class="shorttext" lang="en"   >myInteger</p>
      "! A simple Integer
      integer TYPE i.
* string
    TYPES:
      "! <p class="shorttext">myStringName</p>
      "! This is a string
      "! $maxLength 3
      mystring TYPE string.
* date
    TYPES:
      "! <p   class="shorttext">Date</p>
      "! This is a date
      my_date TYPE d.
* simple structure
    TYPES:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a simple structure
      BEGIN OF my_structure,
        "! <p class="shorttext">First Element</p>
        "! This is the first element
        "! $minLength 4
        my_first_element  TYPE mystring,
        "! <p class="shorttext">Second Element</p>
        "! This is the second element
        my_second_element TYPE i,
      END OF my_structure.
* simple table:
    TYPES:
    "! <p class="shorttext">A Standard Table</p>
    "! A standard table of myString
    my_standard_table TYPE STANDARD TABLE OF mystring WITH DEFAULT KEY.

    TYPES:
    "! <p class="shorttext">A Hashed Table</p>
    "! A hashed table of my_structure
    my_hashed_table TYPE HASHED TABLE OF my_structure WITH UNIQUE KEY my_first_element.

* structure with different table types
    TYPES:
    "! <p class="shorttext">A Sorted Table</p>
    "! A sorted table of my_structure with unique key
    my_sorted_table_unique TYPE SORTED TABLE OF my_structure WITH UNIQUE KEY my_second_element.

    TYPES:
    "! <p class="shorttext">A Sorted Table</p>
    "! A sorted table of my_structure with non unique key
    my_sorted_table_n_unique TYPE SORTED TABLE OF my_structure WITH NON-UNIQUE KEY my_second_element.

    TYPES:
      "! <p class="shorttext">A Structure With Tables</p>
      "! A structure with different table types
      BEGIN OF my_structure_with_tables,
        "! <p class="shorttext">First Table</p>
        "! First table
        first_table  TYPE my_sorted_table_unique,
        "! <p class="shorttext">Second Table</p>
        "! Second table
        second_table TYPE my_sorted_table_n_unique,
      END OF my_structure_with_tables.
* simple structure
    TYPES:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a $ simple st$ructure
      BEGIN OF my_structure2,
        "! <p class="shorttext">First Element</p>
        "! This is the first element
        "! $required
        my_first_element  TYPE mystring,
        "! <p class="shorttext">Second Element</p>
        "! This is the second element
        my_second_element TYPE i,
      END OF my_structure2.
* nested Structure
    TYPES:
      "! <p class="shorttext">Nested Structure</p>
      "! This is a nested structure
      BEGIN OF my_nested_structure,
        "! <p class="shorttext synchronized" >myElementComponente </p>
        "! This is a string element
        my_element TYPE string,
      END OF my_nested_structure,
      "! <p class="shorttext">myStructure</p>
      "! This is a complex structure
      BEGIN OF my_structure3,
        "! <p class="shorttext">nestedStruc</p>
        "! This is the nested structure
        nested_struc TYPE my_nested_structure,
        "! <p class="shorttext">My Element</p>
        "! This is my element
        "! $required
        my_element   TYPE string,
      END OF my_structure3.
* type with enum values:
    TYPES:
     "! <p class="shorttext"> myCategory </p>
     "! This is an enum
     "! $values    {    @link    zcl_aff_test_types.data:enum_values    }
     category TYPE n LENGTH 2.

    CONSTANTS:
      "! <p class="shorttext">Interface Category</p>
      "! Interface category
      BEGIN OF enum_values,
        "! <p class="shorttext">generalCategory</p>
        "! General interface
        general      TYPE category VALUE '00',
        "! Interface definition of a classic BAdI
        classic_badi TYPE category VALUE '01',
      END OF enum_values ##NEEDED.

* type with enum values without initial:
    TYPES:
     "! <p class="shorttext">Enum </p>
     "! This is an enum
     "! $values    {    @link    zcl_aff_test_types.data:enum_values_no_initial    }
     category_no_initial TYPE n LENGTH 2.

    CONSTANTS:
      "! <p class="shorttext">Interface Category</p>
      "! Interface category
      BEGIN OF enum_values_no_initial,
        "! <p class="shorttext">Component 1</p>
        "! Component 1
        component_1 TYPE category_no_initial VALUE '01',
        "! <p class="shorttext">Component 2</p>
        "! Component 2
        component_2 TYPE category_no_initial VALUE '02',
      END OF enum_values_no_initial ##NEEDED.

    TYPES:
      "! <p class="shorttext">Structure with Different Enum Types</p>
      "! Structure with different enum types
      BEGIN OF structure_with_different_enum,
        enum_without_all   TYPE category_no_initial,
        "! $default {@link zcl_aff_test_types.data:enum_values_no_initial.component_2 }
        enum_with_default  TYPE category_no_initial,
        "! $required
        enum_with_required TYPE category_no_initial,
      END OF structure_with_different_enum.

    "! <p class="shorttext"> ABAP Language Version </p>
    "! ABAP language version
    "! $values {@link zcl_aff_test_types.data:co_abap_language_version}
    TYPES language_version TYPE c LENGTH 1.

    CONSTANTS:
      "! <p class="shorttext"> ABAP Language Version </p>
      "! ABAP language version
      BEGIN OF co_abap_language_version,
        "! <p class="shorttext">Standard</p>
        "! Standard
        standard          TYPE language_version VALUE ' ',
        "! <p class="shorttext">ABAP Cloud Development</p>
        "! ABAP cloud development
        cloud_development TYPE language_version VALUE '5',
      END OF co_abap_language_version.

    TYPES:
      "! <p class="shorttext"> Header </p>
      "! The header for an ABAP main object
      BEGIN OF header,
        "! <p class="shorttext"> Description</p>
        "! Description of the ABAP object
        description           TYPE string,
        "! <p class="shorttext"> Original Language</p>
        "! Original language of the ABAP object
        original_language     TYPE sy-langu,
        "! <p class="shorttext"> ABAP Language Version</p>
        "! ABAP language version
        abap_language_version TYPE language_version,
      END OF header.

* complex structure with enum_values
    TYPES:
      "! <p class="shorttext"> Class Properties </p>
      "! Class properties
      BEGIN OF ty_class_properties,
        header         TYPE header,
        "! <p class="shorttext"> Class Category </p>
        "! Class category
        "! $values {@link zcl_aff_test_types.data:co_class_category}
        class_category TYPE n LENGTH 2,
      END OF ty_class_properties.

    CONSTANTS:
      "! <p class="shorttext">Class Category</p>
      "! Class category
      BEGIN OF co_class_category,
        "! <p class="shorttext">General</p>
        "! General
        general    TYPE n LENGTH 2 VALUE '00',
        "! <p class="shorttext">Exit Class</p>
        "! Exit class
        exit_class TYPE n LENGTH 2 VALUE '01',
      END OF co_class_category.
* deep nested structure
    TYPES:
      "! <p class="shorttext">outerStructure</p>
      "! ABAP Doc Comment TYPES list first level
      BEGIN OF list,
        "! <p class="shorttext">Outer Element 1</p>
        "! ABAP Doc field1
        "! $showAlways
        field1 TYPE i,
        "! <p class="shorttext">Outer Element 2</p>
        "! ABAP Doc field2
        "! $required
        field2 TYPE c LENGTH 2,
        "! <p class="shorttext">middleStructure</p>
        "! ABAP Doc list second level
        "! $required
        BEGIN OF list1,
          "! <p class="shorttext">Middle Element</p>
          "! ABAP Doc second level
          "! $required
          element_of_list1 TYPE i,
          "! <p class="shorttext">innerStructure</p>
          "! ABAP Doc third level
          BEGIN OF list2,
            "! <p class="shorttext">Inner Element</p>
            "! ABAP Doc third level
            "! $required
            element_of_list2 TYPE string,
          END OF list2,
        END OF list1,
        "! <p class="shorttext">Outer Element 3</p>
        "! ABAP Doc field3
        "! $required
        field3 TYPE c LENGTH 2,
      END OF list.
* nested structure with table
    TYPES:
      "! <p class="shorttext">outerStructure</p>
      "! ABAP Doc Comment TYPES list first level
      BEGIN OF outer_struc,
        "! <p class="shorttext">Outer Element 1</p>
        "! ABAP Doc field1 first level
        field1 TYPE i,
        "! <p class="shorttext">Inner Structure 1</p>
        "! Inner structure
        BEGIN OF inner_struc,
          "! <p class="shorttext">Inner Element</p>
          "! ABAP Doc element second level
          "! $required
          element_of_inner_struc TYPE i,
          "! <p class="shorttext">inner Table</p>
          "! ABAP Doc element second level
          "! $required
          inner_table_var        TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY,
        END OF inner_struc,
        "! <p class="shorttext">Outer Element 2</p>
        "! ABAP Doc field2 first level
        "! $required
        field2 TYPE c LENGTH 2,
      END OF outer_struc.
* nested structure with table and enum value
    TYPES:
      "! <p class="shorttext">Title of aff_test_type</p>
      "! Description of aff_test_type
      BEGIN OF aff_test_type,
        "! <p class="shorttext">Title of Field1</p>
        "! Description of field1
        field1            TYPE i,
        "! <p class="shorttext">Title of inner_struc</p>
        "! Description of inner_struc
        "! $showAlways
        BEGIN OF inner_struc,
          "! <p class="shorttext">Title of inner_element</p>
          "! Description of inner_element
          "! $required
          inner_element TYPE i,
          "! <p class="shorttext">Title of inner_table</p>
          "! Description of inner_table
          inner_table   TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY,
        END OF inner_struc,
        "! <p class="shorttext">Title of field2</p>
        "! Description of field2
        "! $required
        field2            TYPE c LENGTH 2,
        "! <p class="shorttext">Title of field_with_values</p>
        "! Description of field_with_values
        "! $values {@link zcl_aff_test_types.data:co_class_category}
        field_with_values TYPE n LENGTH 2,
      END OF aff_test_type.
* nested table
    TYPES:
      "! <p class="shorttext">Inner Table</p>
      "! This is the inner Table
      nested_table TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      "! <p class="shorttext">Outer Table</p>
      "! This is the outer table
      first_table  TYPE STANDARD TABLE OF nested_table WITH DEFAULT KEY.
* type with enum values, but wrong link:
    TYPES:
     "! <p class="shorttext"> myCategory </p>
     "! This is an enum
     "! $values    {    @link    zcl_aff_test_types.data:enum_values_wrong    }
     category_wrong TYPE n LENGTH 2.

*  structure with component to wrong enum_values link
    TYPES:
      "! <p class="shorttext"> Structure with Wrong Link </p>
      "! This is a structure with wrong enum_values link
      BEGIN OF structure_with_wrong_link,
        "! <p class="shorttext"> First Element </p>
        "! First element
        element_one TYPE string,
        "! <p class="shorttext"> Second Element </p>
        "! Second element
        element_two TYPE category_wrong,
      END OF structure_with_wrong_link.

* structure with enum value whose link can be found outside

    TYPES:
      "! <p class="shorttext"> String Table</p>
      "! Abap Doc of the table
      string_table TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    TYPES:
      "! <p class="shorttext"> Inner Structure</p>
      "! Abap Doc of the structure
      BEGIN OF inner_struc,
        "! <p class="shorttext">Field</p>
        "! Field
        field TYPE i,
      END OF inner_struc.
    TYPES:
      "! <p class="shorttext"> Structure With ABAP Doc Outside</p>
      "! Structure with ABAP Doc outside
      BEGIN OF structure_with_doc_outside,
        "! <p class="shorttext">Table1</p>
        "! Table1
        "! $required
        table1    TYPE string_table,
        "! <p class="shorttext">Structure</p>
        "! Structure
        structure TYPE inner_struc,
        "! <p class="shorttext">Table2</p>
        "! Table2
        table2    TYPE string_table,
      END OF structure_with_doc_outside.
* Types for number annotations

    TYPES:
      "! <p class="shorttext">Integer Outside</p>
      "! Integer outside
      "! $minimum: -25
      integer_outside TYPE i.
    TYPES:
      "! <p class="shorttext">Structure With Number Types </p>
      "! This is a structure with different number types
      BEGIN OF structure_with_numbers,
        "! <p class="shorttext">Integer With Maximum </p>
        "! Integer with maximum
        "! $maximum: 10
        integer                 TYPE i,
        "! <p class="shorttext">Float With Minimum And Exclusive Maximum </p>
        "! Float with minimum and exclusive maximum
        "! $exclusiveMaximum:100.9
        "! $minimum: -0.42
        float                   TYPE decfloat16,
        "! <p class="shorttext">Packed Number With Given Multiple</p>
        "! Packed number with given multiple
        "! $multipleOf: 0.1
        "! $exclusiveMinimum: 0
        "! $maximum: 99999.90
        packed_with_multiple    TYPE p LENGTH 4 DECIMALS 2,
        "! <p class="shorttext">Packed Number With No Given Multiple</p>
        "! Packed number with no given multiple
        "! $exclusiveMinimum: 0
        packed_without_multiple TYPE p LENGTH 4 DECIMALS 1,
        "! <p class="shorttext">Integer Defined Outside</p>
        "! Integer defined outside and ABAP Doc number annotation outside
        integer_out             TYPE integer_outside,
        "! <p class="shorttext">Integer Defined Outside</p>
        "! Integer defined outside but with ABAP Doc number annotation here
        "! $maximum: 42
        integer_out_with_doc    TYPE integer_outside,
      END OF structure_with_numbers.
* Types for default annotations

    TYPES:
      "! <p class="shorttext">Structure With Default</p>
      "! Structure to test default checks in simple transformation
      BEGIN OF structure_different_default,
        "! <p class="shorttext">Four Byte Integer</p>
        "! Four byte integer
        "! $default '5'
        four_byte_int    TYPE i,
        "! <p class="shorttext">Eight Byte Integer</p>
        "! Eight byte integer
        "! $default '55'
        eight_byte_int   TYPE int8,
        "! <p class="shorttext">Binary Floating Point Number</p>
        "! Binary floating point number
        "! $default '4.3'
        bin_float        TYPE f,
        "! <p class="shorttext">Byte Like</p>
        "! Byte like
        "! $default 'FFFF'
        byte_like        TYPE x LENGTH 2,
        "! <p class="shorttext">Byte Like2</p>
        "! Byte like2
        "! $default 'FF00FF'
        byte_like2       TYPE xstring,
        "! <p class="shorttext">Decimal Floating Point Number</p>
        "! Decimal floating point number with 16 places
        "! $default '25.26'
        decimal_float_16 TYPE decfloat16,
        "! <p class="shorttext">Decimal Floating Point Number</p>
        "! Decimal floating point number with 34 places
        "! $default '123.05'
        decimal_float_34 TYPE decfloat34,
        "! <p class="shorttext">Packed Number</p>
        "! Packed number
        "! $default '123.45'
        packed_number    TYPE p LENGTH 3 DECIMALS 2,
        "! <p class="shorttext">Numeric Text Field</p>
        "! Numeric text field
        "! $default '1067'
        numeric_text     TYPE n LENGTH 4,
        "! <p class="shorttext">Character Text</p>
        "! Character text
        "! $default 'abcde'
        character_text   TYPE c LENGTH 5,
        "! <p class="shorttext">String Text</p>
        "! String text
        "! $default 'Default text'
        string_text      TYPE string,
        "! <p class="shorttext">Date</p>
        "! Date
        "! $default '19720401'
        date_field       TYPE d,
        "! <p class="shorttext">Time</p>
        "! Time
        "! $default '201500'
        time_field       TYPE t,
        "!  <p class="shorttext">Date Time</p>
        "! Date time: No support
        "! $default '9999-12-31T23:59:59.9999999'
        date_time_field  TYPE utclong,
        "! <p class="shorttext">Boolean</p>
        "! Boolean with default abap_true
        "! $default 'abap_true'
        bool_true        TYPE abap_bool,
        "! <p class="shorttext">Boolean</p>
        "! Boolean with default abap_false
        "! $default 'abap_false'
        bool_false       TYPE abap_bool,
        "! <p class="shorttext">Enum Type</p>
        "! Enum type
        "! $default {@link zcl_aff_test_types.data:co_class_category.exit_class }
        "! $values  {@link zcl_aff_test_types.data:co_class_category }
        enum_type        TYPE n LENGTH 2,
      END OF structure_different_default.

    TYPES:
      "! <p class="shorttext">Structure With Default</p>
      "! Structure with default
      BEGIN OF structure_with_default_problem,
        "! <p class="shorttext">Integer</p>
        "! Integer
        "! $default: '5'
        "! $required
        integer          TYPE i,
        "! <p class="shorttext">String Element</p>
        "! String element with default value
        "! $default: 'DefaultString'
        "! $showAlways
        string_element   TYPE string,
        "! <p class="shorttext">Enum Value</p>
        "! Enum value with default
        "! $default {@link zcl_aff_test_types.data:co_class_category.exit_class }
        "! $values  {@link zcl_aff_test_types.data:co_class_category }
        "! $required
        enum_required    TYPE n LENGTH 2,
        "! <p class="shorttext">Enum Value</p>
        "! Enum value with default
        "! $default {@link zcl_aff_test_types.data:co_class_category.exit_class }
        "! $values  {@link zcl_aff_test_types.data:co_class_category }
        "! $showAlways
        enum_show_always TYPE n LENGTH 2,
      END OF structure_with_default_problem.

    TYPES:
      "! <p class="shorttext">Inner Structure</p>
      "! Inner structure
      BEGIN OF inner_struc_with_default,
        "! <p class="shorttext">Inner Component</p>
        "! Inner component
        "! $default 'Default Value'
        inner_component TYPE string,
      END OF inner_struc_with_default,

      "! <p class="shorttext">Middle Structure</p>
      "! Middle structure
      BEGIN OF middle_struc_with_default,
        "! <p class="shorttext">Middle Component</p>
        "! Middle component
        "! $default 'abcd'
        middle_component TYPE c LENGTH 4,
        "! <p class="shorttext">Inner Structure</p>
        "! Inner structure
        inner_struc      TYPE inner_struc_with_default,
      END OF middle_struc_with_default,

      "! <p class="shorttext">Nested Structure</p>
      "! Nested structure
      BEGIN OF nested_struc_with_default,
        "! <p class="shorttext">Outer Component</p>
        "! Outer component
        "! $default '10'
        outer_component TYPE i,
        "! <p class="shorttext">Middle Structure</p>
        "! Middle structure
        middle_struc    TYPE middle_struc_with_default,
      END OF nested_struc_with_default.

*  component with wrong default links
    TYPES:
      "! <p class="shorttext">Structure With Wrong Default</p>
      "! Structure with wrong default
      BEGIN OF structure_with_wrong_default,
        "! <p class="shorttext">First Element</p>
        "! First element
        "! $default {@link zcl_aff_test_types.data:enum_values.wrong_component }
        element_one TYPE category,
        "! <p class="shorttext">Second Element</p>
        "! Second element
        "! $default {@link wrong_link }
        element_two TYPE category,
      END OF structure_with_wrong_default.
* Types for callbackClass annotation

* string with callbackClass
    TYPES:
      "! <p class="shorttext">String With Callback</p>
      "! This is a String with a CallbackClass
      "! $maxLength 3
      "! $callbackClass {     @link    zcl_aff_test_types    }
    simple_callback TYPE string.
* table with callback, components are strings
    TYPES:
      "! <p class="shorttext">my_table</p>
      "! A standard table of strings with CallbackClass
      "! $callbackClass {     @link    zcl_aff_test_types    }
      "! $required
      table_callback TYPE STANDARD TABLE OF mystring WITH DEFAULT KEY.
* table with callback, components are tables
    TYPES:
      "! <p class="shorttext">my_table_of_table</p>
      "! A standard table of my_table
      "! $callbackClass {     @link    zcl_aff_test_types   }
      table_call_of_table TYPE STANDARD TABLE OF my_standard_table WITH DEFAULT KEY.
* simple structure with callback
    TYPES:
      "! <p class="shorttext">Structure With Callback</p>
      "! Structure with callback
      "! $callbackClass {     @link    zcl_aff_test_types    }
      BEGIN OF structure_callback,
        "! <p class="shorttext">First Element</p>
        "! This is the first element
        element_name TYPE i,
      END OF structure_callback.

* table of my_structure_with_callback
    TYPES:
      "! <p class="shorttext">my_table</p>
      "! A standard table of my_structure_with_callback
      table_of_struc_with_callback TYPE STANDARD TABLE OF structure_callback WITH DEFAULT KEY.
* simple structure with component my_table_with_callback
    TYPES:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a simple structure
      BEGIN OF struc_of_table_with_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        "! $required
        element_table_callback TYPE table_callback,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        my_second_element      TYPE i,
      END OF struc_of_table_with_callback.

* simple structure with component my_structure_with_callback
    TYPES:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a simple structure
      BEGIN OF struc_in_struc_with_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        my_first_element           TYPE string,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        "! $required
        element_structure_callback TYPE structure_callback,
        "! <p class="shorttext synchronized" >Third Element</p>
        "! This is the third element
        my_third_element           TYPE i,
      END OF struc_in_struc_with_callback.

*  simple structure with component callback
    TYPES:
      "! <p class="shorttext synchronized" >Simple Structure</p>
      "! This is a  simple structure
      BEGIN OF structure_with_elem_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        "! $minLength 2
        "! $required
        "! $callbackClass {     @link    zcl_aff_test_types    }
        element_callback  TYPE mystring,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        my_second_element TYPE i,
      END OF structure_with_elem_callback.

*      simple structure with wrong callbackclass link
    TYPES:
      "! <p class="shorttext synchronized" >Structure With Wrong Callback</p>
      "! Structure with wrong callback
      BEGIN OF structure_with_wrong_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        "! $minLength 2
        "! $callbackClass {     @link    cl_aff_notest_types_for_writer    }
        "! $required
        my_first_element  TYPE mystring,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        "! $callbackClass {   wrong_link    }
        "! $maximum 4
        my_second_element TYPE i,
      END OF structure_with_wrong_callback.

*      Types with missing titles and description
    TYPES:
      element_no_title_descr TYPE string.

    TYPES:
      BEGIN OF inner_struc_no_title_descr,
        "! <p class="shorttext" >Inner Field</p>
        "! Inner field
        inner_field TYPE i,
      END OF inner_struc_no_title_descr.

    TYPES:
      table_no_title_descr TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    TYPES:
      BEGIN OF structure_no_title_descr,
        "! Only description
        field1      TYPE string,
        "! <p class="shorttext synchronized" >Only Title</p>
        inner_struc TYPE inner_struc_no_title_descr,
        inner_table TYPE table_no_title_descr,
      END OF structure_no_title_descr.
    TYPES:
      BEGIN OF ty_include_type,
        "! <p class="shorttext">First Element In Include</p>
        "! $required
        first_element  TYPE string,
        second_element TYPE my_structure,
        "! <p class="shorttext">Third Element In Include</p>
        "! Third element in include
        "! $default '10'
        third_element  TYPE i,
      END OF ty_include_type.

    TYPES:
      "! <p class="shorttext">Structure With Include</p>
      "! Structure with include
      BEGIN OF structure_with_include.
        INCLUDE TYPE ty_include_type.
    TYPES:
      "! Other element
      "! $required
        other_element   TYPE i,
        "! <p class="shorttext">Other structure</p>
        "! Other Structure
        other_structure TYPE my_structure,
      END OF structure_with_include.

    CONSTANTS:
      BEGIN OF co_overwritten_values,
        "! <p class="shorttext">Option 1</p>
        "! Option 1
        "! $enumValue 'AAAA'
        first_value  TYPE c LENGTH 2 VALUE 'AA',
        "! <p class="shorttext">Option 2</p>
        "! Option 2
        "! $enumValue 'BBBB'
        second_value TYPE c LENGTH 2 VALUE 'BB',
      END OF co_overwritten_values.

    TYPES:
      "! <p class="shorttext">Structure With Overwritten Enum Values</p>
      "! Structure with overwritten enum values
      BEGIN OF struc_with_own_enum_values,
        "! <p class="shorttext">Enum Component</p>
        "! Enum component
        "! $required
        "! $values  {@link zcl_aff_test_types.data:co_overwritten_values }
        enum_component TYPE c LENGTH 2,
      END OF struc_with_own_enum_values.

    CLASS-DATA subschema TYPE string_table.

    CLASS-DATA expected_var TYPE REF TO data.

    CLASS-METHODS get_subschema
      RETURNING VALUE(subschema) TYPE string_table.

    CLASS-METHODS serialize
      IMPORTING
        writer                     TYPE REF TO if_sxml_writer
        simple_callback            TYPE simple_callback OPTIONAL
        structure_callback         TYPE structure_callback OPTIONAL
        table_callback             TYPE table_callback OPTIONAL
        element_callback           TYPE string OPTIONAL
        element_structure_callback TYPE structure_callback OPTIONAL
        element_table_callback     TYPE table_callback OPTIONAL.

    CLASS-METHODS deserialize
      IMPORTING
        reader                     TYPE REF TO if_sxml_reader
      EXPORTING
        simple_callback            TYPE simple_callback
        structure_callback         TYPE structure_callback
        table_callback             TYPE table_callback
        element_callback           TYPE string
        element_structure_callback TYPE structure_callback
        element_table_callback     TYPE table_callback
      RAISING
        cx_sxml_error.

    CLASS-METHODS set_expected
      IMPORTING
        expected_variable TYPE any.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS jump_to_end
      IMPORTING
        reader TYPE REF TO if_sxml_reader
      RAISING
        cx_sxml_parse_error.
ENDCLASS.
CLASS zcl_aff_writer DEFINITION
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_aff_writer
      FINAL METHODS open_node close_node write_element get_output.

    METHODS constructor.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_stack_entry,
        operation TYPE zif_aff_writer=>enum_operation,
        name      TYPE string,
      END OF ty_stack_entry.

    TYPES:
      BEGIN OF ty_structure_stack,
        name          TYPE string,
        absolute_name TYPE abap_abstypename,
      END OF ty_structure_stack,
      tt_structure_stack TYPE STANDARD TABLE OF ty_structure_stack.

    DATA:
      output                     TYPE string_table,
      formatting_option          TYPE zif_aff_writer=>enum_formatting_option,
      name_mappings              TYPE zif_aff_writer=>ty_name_mappings,
      abap_value_mappings        TYPE zif_aff_writer=>ty_abap_value_mappings,
      content                    TYPE string_table,
      stack_of_structure         TYPE tt_structure_stack,
      stack                      TYPE STANDARD TABLE OF ty_stack_entry,
      indent_level               TYPE i VALUE 0,
      c_indent_number_characters TYPE i VALUE 2,
      log                        TYPE REF TO zif_aff_log,
      abap_doc_parser            TYPE REF TO zcl_aff_abap_doc_parser,
      ignore_til_indent_level    TYPE i,
      abap_doc                   TYPE zcl_aff_abap_doc_parser=>abap_doc,
      fullname_of_type           TYPE string.

    METHODS: get_value_mapping_for_element
      IMPORTING abap_element  TYPE string
      RETURNING VALUE(result) TYPE zif_aff_writer=>ty_abap_value_mapping,
      map_and_format_name
        IMPORTING name          TYPE string
        RETURNING VALUE(result) TYPE string,
      get_json_type_from_description
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(result)       TYPE zif_aff_writer=>enum_type_info
        RAISING   zcx_aff_tools,

      write_open_tag FINAL
        IMPORTING
          line TYPE string,
      write_closing_tag FINAL
        IMPORTING
          line TYPE string,
      add_to_stack FINAL
        IMPORTING
          entry TYPE ty_stack_entry,
      last_operation FINAL
        RETURNING VALUE(result) TYPE zif_aff_writer=>enum_operation,
      append_to_previous_line FINAL
        IMPORTING
          string TYPE string,
      append_before_output,
      append_after_output,

      write_tag ABSTRACT
        IMPORTING
          line TYPE string,

      write_element ABSTRACT
        IMPORTING
                  element_name        TYPE string
                  element_description TYPE REF TO cl_abap_elemdescr
        RAISING   zcx_aff_tools,

      open_structure ABSTRACT
        IMPORTING
                  structure_name        TYPE string
                  structure_description TYPE REF TO cl_abap_typedescr
        RAISING   zcx_aff_tools,

      close_structure ABSTRACT
        IMPORTING
                  structure_name        TYPE string
                  structure_description TYPE REF TO cl_abap_typedescr
        RAISING   zcx_aff_tools,

      open_table ABSTRACT
        IMPORTING
                  table_name        TYPE string
                  table_description TYPE REF TO cl_abap_typedescr
        RAISING   zcx_aff_tools ##NEEDED,

      close_table ABSTRACT
        IMPORTING
                  table_name        TYPE string
                  table_description TYPE REF TO cl_abap_typedescr
        RAISING   zcx_aff_tools ##NEEDED,

      apply_formatting
        IMPORTING name          TYPE string
        RETURNING VALUE(result) TYPE string,

      call_reader_and_decode
        IMPORTING
          name_of_source       TYPE string
          element_name         TYPE string
        RETURNING
          VALUE(read_abap_doc) TYPE zcl_aff_abap_doc_parser=>abap_doc,

      delete_first_of_struc_stack,

      get_all_path_information
        IMPORTING
          name                    TYPE string
        EXPORTING
          VALUE(source_type)      TYPE string
          VALUE(source)           TYPE string
          VALUE(fullname_of_type) TYPE string,

      get_structure_of_enum_values
        IMPORTING
          link_to_values             TYPE string
          fullname_of_type           TYPE string
        EXPORTING
          VALUE(structure_of_values) TYPE REF TO cl_abap_structdescr
          VALUE(name_of_source)      TYPE string
          VALUE(name_of_constant)    TYPE string,
      get_abap_doc_for_absolute_name
        IMPORTING
          absolute_name   TYPE abap_abstypename
        RETURNING
          VALUE(abap_doc) TYPE zcl_aff_abap_doc_parser=>abap_doc,

      compare_abap_doc
        IMPORTING
          abap_doc_additional TYPE zcl_aff_abap_doc_parser=>abap_doc
        CHANGING
          abap_doc_base       TYPE zcl_aff_abap_doc_parser=>abap_doc,

      get_splitted_absolute_name
        IMPORTING
          absolute_name TYPE abap_abstypename
        RETURNING
          VALUE(result) TYPE string_table,

      get_default_from_link
        IMPORTING
          link                 TYPE string
          fullname_of_type     TYPE string
          element_type         TYPE abap_typekind
        RETURNING
          VALUE(default_value) TYPE string,

      remove_leading_trailing_spaces
        CHANGING
          string_to_work_on TYPE string,

      is_callback_class_valid
        IMPORTING
          class_name      TYPE string
          component_name  TYPE string
        RETURNING
          VALUE(is_valid) TYPE abap_boolean,

      is_default_value_valid
        IMPORTING
                  element_description TYPE REF TO cl_abap_elemdescr
                  default_value       TYPE string
                  fullname_of_type    TYPE string
        RETURNING VALUE(is_valid)     TYPE abap_boolean
        RAISING
                  zcx_aff_tools,

      is_sy_langu
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(result)       TYPE abap_bool,

      clear_type_specifics,

      check_redundant_annotations.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_abap_types,
        boolean   TYPE string VALUE `ABAP_BOOLEAN;ABAP_BOOL;BOOLEAN;BOOLE_D;XFELD;XSDBOOLEAN;FLAG`,
        timestamp TYPE string VALUE `TIMESTAMP;TIMESTAMPL`,
      END OF c_abap_types.
    METHODS:
      get_mapped_name
        IMPORTING name          TYPE string
        RETURNING VALUE(result) TYPE string,

      is_type_timestamp
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(result)       TYPE abap_boolean,

      is_type_boolean
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(result)       TYPE abap_boolean,

      get_constant_as_struc
        IMPORTING
          name_of_source           TYPE string
          name_of_constant         TYPE string
          fullname_of_type         TYPE string
        RETURNING
          VALUE(constant_as_struc) TYPE REF TO cl_abap_structdescr,

      get_infos_of_values_link
        IMPORTING
          values_link             TYPE string
        EXPORTING
          VALUE(name_of_source)   TYPE string
          VALUE(name_of_constant) TYPE string,

      validate_default_link
        IMPORTING
          splitted_link    TYPE string_table
          fullname_of_type TYPE string
          element_type     TYPE abap_typekind
        RETURNING
          VALUE(is_valid)  TYPE abap_boolean.

ENDCLASS.
"! Writer for a JSON schema. This is just a utility class helping to create a JSON schema.
"! The generated schema must be reviewed and adapted!
CLASS zcl_aff_writer_json_schema DEFINITION
  FINAL
  CREATE PUBLIC
  INHERITING FROM zcl_aff_writer.

  PUBLIC SECTION.

    CONSTANTS:
      c_schema_specification TYPE string VALUE 'https://json-schema.org/draft/2020-12/schema' ##NO_TEXT,
      c_link_to_repository   TYPE string VALUE 'https://github.com/SAP/abap-file-formats' ##NO_TEXT.

    METHODS:
      "! Creates a JSON schema writer<br><br>
      "! Example for schema_id: http://sap.com/schema/nrob.json
      "!
      "! @parameter schema_id | The schema that should be written in the $id field of the schema
      "! @parameter format_version | The version of the ABAP file format as integer
      constructor
        IMPORTING schema_id      TYPE string
                  format_version TYPE i DEFAULT 1,

      zif_aff_writer~validate REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      write_element REDEFINITION,
      open_structure REDEFINITION,
      close_structure REDEFINITION,
      open_table REDEFINITION,
      write_tag REDEFINITION,
      close_table REDEFINITION,
      append_after_output REDEFINITION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_buffer,
        name            TYPE string,
        number_brackets TYPE i,
      END OF ty_buffer,
      tt_buffer TYPE STANDARD TABLE OF ty_buffer.

    CONSTANTS:
      c_format_version            TYPE string VALUE 'FORMAT_VERSION',
      c_max_length_of_description TYPE i VALUE 253.

    DATA:
      schema_id              TYPE string,
      structure_buffer       TYPE tt_buffer,
      table_buffer           TYPE tt_buffer,
      ignore_next_elements   TYPE abap_boolean,
      enum_values            TYPE string_table,
      enum_titles            TYPE string_table,
      enum_descriptions      TYPE string_table,
      stack_of_required_tabs TYPE STANDARD TABLE OF string_table,
      format_version         TYPE i.

    METHODS: append_comma_to_prev_line,

      get_json_schema_type
        IMPORTING element_name        TYPE string
                  element_description TYPE REF TO cl_abap_elemdescr
                  json_type           TYPE zif_aff_writer=>enum_type_info
        RETURNING VALUE(result)       TYPE string
        RAISING   zcx_aff_tools,

      open_json_schema_for_structure
        IMPORTING structure_name        TYPE string
                  structure_description TYPE REF TO cl_abap_typedescr
        RAISING   zcx_aff_tools,

      open_json_schema_for_table
        IMPORTING table_name        TYPE string
                  table_description TYPE REF TO cl_abap_tabledescr
        RAISING   zcx_aff_tools,

      open_json_schema_for_element,

      get_description
        IMPORTING type_description TYPE REF TO cl_abap_typedescr OPTIONAL
        RETURNING VALUE(result)    TYPE string,

      get_enum_values
        IMPORTING element_name        TYPE string
                  element_description TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(result)       TYPE string_table
        RAISING
                  zcx_aff_tools,

      get_enum_descriptions
        IMPORTING element_name        TYPE string
                  element_description TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(result)       TYPE string_table,

      type_is_integer
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(result)       TYPE abap_bool,

      set_enum_properties
        IMPORTING
          enum_type TYPE abap_typekind
        RAISING
          zcx_aff_tools,

      add_required_table_to_stack,

      delete_first_tab_of_req_stack,

      write_req_and_add_props,

      get_format
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(result)       TYPE string,

      date_time_from_abap_to_json
        IMPORTING
          date_time_abap        TYPE string
          element_description   TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(date_time_json) TYPE string,

      handle_default
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          json_type           TYPE zif_aff_writer=>enum_type_info
        RAISING
          zcx_aff_tools,

      handle_extrema
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          element_name        TYPE string,

      handle_string
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr,

      handle_language_field,

      handle_enums
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          element_name        TYPE string
          enums               TYPE string_table,

      write_subschema
        IMPORTING
          callback_class TYPE string,

      reset_indent_level_tag,

      write_enum_properties
        IMPORTING
          property_table TYPE string_table,

      check_title_and_description
        IMPORTING abap_doc_to_check        TYPE zcl_aff_abap_doc_parser=>abap_doc
                  fullname_of_checked_type TYPE string,
      write_title_and_description
        IMPORTING
          type_description TYPE REF TO cl_abap_typedescr
          check_not_needed TYPE abap_boolean DEFAULT abap_false,
      set_abapdoc_fullname_element
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          element_name        TYPE string
          splitted_prev_name  TYPE string_table,
      set_abapdoc_fullname_struc_tab
        IMPORTING
          type_description TYPE REF TO cl_abap_typedescr
          type_name        TYPE string,

      get_max_length
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(result)       TYPE string,
      get_extrema
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        EXPORTING VALUE(max)          TYPE string
                  VALUE(min)          TYPE string.
ENDCLASS.
CLASS zcl_aff_writer_xslt DEFINITION
  INHERITING FROM zcl_aff_writer
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          st_root_name TYPE csequence DEFAULT 'root' ##NO_TEXT,

      zif_aff_writer~validate REDEFINITION.

  PROTECTED SECTION.

    METHODS:
      write_open_structure
        IMPORTING
          structure_name        TYPE string
          structure_description TYPE REF TO cl_abap_typedescr
        RAISING
          zcx_aff_tools,
      append_after_output REDEFINITION,
      append_before_output REDEFINITION,
      write_element REDEFINITION,
      open_structure REDEFINITION,
      open_table REDEFINITION,
      close_structure REDEFINITION,
      write_tag REDEFINITION,
      close_table REDEFINITION,
      write_callback
        IMPORTING
          name_of_callback_class TYPE string
          parameter_name         TYPE string
          ref_name               TYPE string.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_variable_default_pair,
        var_name      TYPE string,
        default_value TYPE string,
      END OF ty_variable_default_pair,
      tt_variable_default_pair TYPE STANDARD TABLE OF ty_variable_default_pair WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_components_with_default,
        line_to_insert    TYPE i,
        table_of_defaults TYPE tt_variable_default_pair,
      END OF ty_components_with_default,

      tt_components_with_default TYPE STANDARD TABLE OF ty_components_with_default.
    DATA:
      st_root_name                  TYPE string,
      st_template_imports           TYPE string_table,
      next_tag_without_name_and_ref TYPE abap_boolean,
      stack_default_comp_of_struc   TYPE tt_components_with_default,
      ignore_next_elements          TYPE abap_boolean.

    METHODS: get_tag_from_type
      IMPORTING
        json_type     TYPE zif_aff_writer=>enum_type_info
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_aff_tools,

      get_option
        IMPORTING
          json_type           TYPE zif_aff_writer=>enum_type_info
          element_description TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(result)       TYPE string
        RAISING
          zcx_aff_tools,

      write_value_mappings
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          json_type           TYPE zif_aff_writer=>enum_type_info
          element_name        TYPE string
          value_mapping       TYPE zif_aff_writer=>ty_abap_value_mapping
        RAISING
          zcx_aff_tools,

      get_abap_value
        IMPORTING
          abap_value          TYPE string
          element_description TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(result)       TYPE string,
      get_name
        IMPORTING
          name          TYPE string
        RETURNING
          VALUE(result) TYPE string,

      get_ref
        IMPORTING
          name          TYPE string
        RETURNING
          VALUE(result) TYPE string,

      get_ref_for_structure
        IMPORTING
          name          TYPE string
        RETURNING
          VALUE(result) TYPE string,

      get_condition_tab_or_struc
        IMPORTING
          type_name        TYPE string
        RETURNING
          VALUE(condition) TYPE string
        RAISING
          zcx_aff_tools,

      get_condition_for_element
        IMPORTING
          element_name        TYPE string
          element_description TYPE REF TO cl_abap_elemdescr
          type                TYPE zif_aff_writer=>enum_type_info
          value_mappings      TYPE zif_aff_writer=>ty_value_mappings
        RETURNING
          VALUE(condition)    TYPE string
        RAISING
          zcx_aff_tools,

      get_value_mapping_via_enum
        IMPORTING
          enum_type             TYPE abap_typekind
        RETURNING
          VALUE(value_mappings) TYPE zif_aff_writer=>ty_abap_value_mapping
        RAISING
          zcx_aff_tools,
      get_default_value_from_default
        IMPORTING
          value_mappings      TYPE zif_aff_writer=>ty_value_mappings
          type                TYPE zif_aff_writer=>enum_type_info
          element_description TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(default)      TYPE string
        RAISING
          zcx_aff_tools,

      get_prefixed_default
        IMPORTING
          value               TYPE string
          element_description TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(result)       TYPE string
        RAISING
          zcx_aff_tools,

      write_callback_template
        IMPORTING
          element_name TYPE string
          description  TYPE REF TO cl_abap_typedescr
          tag          TYPE string OPTIONAL
        RAISING
          zcx_aff_tools,
      reset_indent_level_tag,
      write_defaults,
      write_iso_language_callback
        IMPORTING
          element_name TYPE string,
      enable_extension
        IMPORTING
          structure_description TYPE REF TO cl_abap_structdescr,
      get_default
        IMPORTING
          structure_name      TYPE string
          value_mappings      TYPE zif_aff_writer=>ty_value_mappings
          element_description TYPE REF TO cl_abap_elemdescr
          type                TYPE zif_aff_writer=>enum_type_info
        RETURNING
          VALUE(default)      TYPE string
        RAISING
          zcx_aff_tools,
      set_abapdoc_fullname_tab_struc
        IMPORTING
          type_description TYPE REF TO cl_abap_typedescr
          type_name        TYPE string,
      set_abapdoc_fullname_element
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          element_name        TYPE string.

ENDCLASS.
CLASS zcl_aff_writer_xslt IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->st_root_name = st_root_name.
    next_tag_without_name_and_ref = abap_true.
    me->formatting_option = zif_aff_writer=>formatting_option-camel_case.
    zif_aff_writer~set_name_mappings( VALUE #( ( abap = 'schema' json = '$schema' ) ) ).
  ENDMETHOD.
  METHOD open_structure.
    write_open_structure( structure_name = structure_name structure_description = structure_description ).
    INSERT VALUE #( name = structure_name absolute_name = structure_description->absolute_name ) INTO me->stack_of_structure INDEX 1.
  ENDMETHOD.
  METHOD write_open_structure.
    clear_type_specifics( ).
    set_abapdoc_fullname_tab_struc( type_description = structure_description type_name = structure_name ).

    IF abap_doc-callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = abap_doc-callback_class component_name = fullname_of_type ).
      write_callback_template( element_name = structure_name description = structure_description ).
    ENDIF.
    write_open_tag( |<tt:cond{ get_condition_tab_or_struc( structure_name ) }>| ).
    write_open_tag( |<object{ get_name( name = structure_name ) }{ get_ref_for_structure( structure_name ) }>| ).
    INSERT VALUE #( line_to_insert = lines( content ) ) INTO me->stack_default_comp_of_struc INDEX 1.
    write_open_tag( `<tt:group>` ).
    next_tag_without_name_and_ref = abap_false.
  ENDMETHOD.

  METHOD open_table.
    clear_type_specifics( ).
    set_abapdoc_fullname_tab_struc( type_description = table_description type_name = table_name ).

    IF abap_doc-callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = abap_doc-callback_class component_name = fullname_of_type ).
      write_callback_template( element_name = table_name description = table_description ).
    ENDIF.

    write_open_tag( |<tt:cond{ get_condition_tab_or_struc( table_name ) }>| ).
    write_open_tag( |<array{ get_name( name = table_name ) }>| ).
    write_open_tag( |<tt:loop{ get_ref( table_name ) }>| ).
    write_open_tag( `<tt:group>` ).
    next_tag_without_name_and_ref = abap_true.
  ENDMETHOD.
  METHOD close_structure.
    delete_first_of_struc_stack( ).
    write_defaults( ).
    enable_extension( CAST #( structure_description ) ).
    write_closing_tag( `</tt:group>` ).
    write_closing_tag( `</object>` ).
    write_closing_tag( `</tt:cond>` ).
    reset_indent_level_tag( ).
  ENDMETHOD.
  METHOD close_table.
    write_closing_tag( `</tt:group>` ).
    write_closing_tag( `</tt:loop>` ).
    write_closing_tag( `</array>` ).
    write_closing_tag( `</tt:cond>` ).
    next_tag_without_name_and_ref = abap_false.
    reset_indent_level_tag( ).
  ENDMETHOD.
  METHOD append_before_output.
    APPEND `<?sap.transform simple?>` TO output.
    APPEND `<tt:transform xmlns:tt="http://www.sap.com/transformation-templates">` TO output.
    APPEND LINES OF st_template_imports TO output.
    APPEND |<tt:root name="{ st_root_name }"/>| TO output.
    APPEND `<tt:template>` TO output.
    APPEND |<tt:ref name="{ st_root_name }">| TO output.
  ENDMETHOD.
  METHOD append_after_output.
    APPEND `</tt:ref>` TO output.
    APPEND `</tt:template>` TO output.
    APPEND `</tt:transform>` TO output.
  ENDMETHOD.
  METHOD write_element.
    CHECK ignore_next_elements = abap_false.

    clear_type_specifics( ).
    set_abapdoc_fullname_element( element_description = element_description element_name = element_name ).

    IF abap_doc-enumvalues_link IS NOT INITIAL.
      DATA(abap_value_mapping) = get_value_mapping_via_enum( element_description->type_kind ).
      IF abap_value_mapping IS NOT INITIAL.
        abap_value_mapping-target_type = zif_aff_writer=>type_info-string.
      ENDIF.
    ELSE.
      abap_value_mapping = get_value_mapping_for_element( element_name ).
    ENDIF.

    DATA(type) = COND #( WHEN abap_value_mapping IS NOT INITIAL THEN abap_value_mapping-target_type
                         ELSE get_json_type_from_description( element_description ) ).

    DATA(tag) = get_tag_from_type( type ).

    IF abap_doc-callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = abap_doc-callback_class component_name = fullname_of_type ).
      write_callback_template( element_name = element_name description = element_description tag = tag ).
    ENDIF.
    write_open_tag( |<tt:cond{ get_condition_for_element( element_name = element_name element_description = element_description value_mappings = abap_value_mapping-value_mappings type = type ) }>| ).
    write_open_tag( |<{ tag }{ get_name( name = element_name ) }>| ).
    IF ( is_sy_langu( element_description = element_description ) ).
      write_iso_language_callback( element_name = element_name ).
    ELSEIF abap_value_mapping IS INITIAL.
      write_tag( |<tt:value{ get_ref( element_name ) }{ get_option( json_type = type element_description = element_description ) }/>| ).
    ELSE.
      write_value_mappings( element_description = element_description json_type = type element_name = element_name value_mapping = abap_value_mapping ).
    ENDIF.
    write_closing_tag( |</{ tag }>| ).
    write_closing_tag( `</tt:cond>` ).
    reset_indent_level_tag( ).
  ENDMETHOD.

  METHOD set_abapdoc_fullname_element.
    IF next_tag_without_name_and_ref = abap_true.
      DATA(splitted_absolute_name) = get_splitted_absolute_name( element_description->absolute_name ).
      DATA(source_type) = splitted_absolute_name[ 1 ].
      DATA(source) = splitted_absolute_name[ 2 ].
      fullname_of_type = element_name.
      DATA(already_searched) = abap_true.
    ELSE.
      get_all_path_information(
        EXPORTING
          name             = element_name
        IMPORTING
          source_type      = source_type
          source           = source
          fullname_of_type = fullname_of_type ).
    ENDIF.
    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      abap_doc = call_reader_and_decode( name_of_source = source element_name   = fullname_of_type ).
    ENDIF.
    IF already_searched = abap_false.
      DATA(abap_doc_second) = get_abap_doc_for_absolute_name( absolute_name = element_description->absolute_name ).
      compare_abap_doc(
        EXPORTING
          abap_doc_additional = abap_doc_second
        CHANGING
          abap_doc_base       = abap_doc ).
    ENDIF.
    check_redundant_annotations( ).
  ENDMETHOD.
  METHOD set_abapdoc_fullname_tab_struc.
    IF next_tag_without_name_and_ref = abap_false.
      get_all_path_information(
        EXPORTING
          name             = type_name
        IMPORTING
          source_type      = DATA(source_type)
          source           = DATA(source)
          fullname_of_type = fullname_of_type ).
      IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
        abap_doc = call_reader_and_decode( name_of_source = source element_name = fullname_of_type ).
      ENDIF.
      DATA(abap_doc_second) = get_abap_doc_for_absolute_name( absolute_name = type_description->absolute_name ).
      compare_abap_doc(
        EXPORTING
          abap_doc_additional = abap_doc_second
        CHANGING
          abap_doc_base       = abap_doc ).
    ELSE.
      abap_doc = get_abap_doc_for_absolute_name( absolute_name = type_description->absolute_name ).
      fullname_of_type = type_name.
    ENDIF.
    check_redundant_annotations( ).
  ENDMETHOD.
  METHOD get_tag_from_type.
    CASE json_type.
      WHEN zif_aff_writer=>type_info-string OR zif_aff_writer=>type_info-date_time.
        result = `str`.
      WHEN zif_aff_writer=>type_info-boolean.
        result = `bool` ##NO_TEXT.
      WHEN zif_aff_writer=>type_info-numeric.
        result = `num`.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e102(zaff_tools) WITH json_type.
    ENDCASE.
  ENDMETHOD.
  METHOD get_option.
    IF is_sy_langu( element_description ) = abap_true.
      result = ` option="format(language)"` ##NO_TEXT.
    ELSE.
      CASE json_type.
        WHEN zif_aff_writer=>type_info-string.
          result = space.
        WHEN zif_aff_writer=>type_info-date_time.
          result = ` option="format(dateTimeOffset)"`.
        WHEN zif_aff_writer=>type_info-boolean.
          result = ` option="format(boolean)"` ##NO_TEXT.
        WHEN zif_aff_writer=>type_info-numeric.
          result = ` option="format(alpha)"` ##NO_TEXT.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e102(zaff_tools) WITH json_type.
      ENDCASE.
    ENDIF.
  ENDMETHOD.
  METHOD write_value_mappings.
    DATA(value_mappings) = value_mapping-value_mappings.
    IF lines( value_mappings ) = 0.
      RETURN.
    ENDIF.
    write_tag( |<tt:value{ get_ref( element_name ) } { get_option( json_type = json_type element_description = element_description ) }map="| ) ##NO_TEXT.

    DATA(index) = 1.
    LOOP AT value_mappings ASSIGNING FIELD-SYMBOL(<value_mapping>).
      DATA(abap_value) = get_abap_value( abap_value = <value_mapping>-abap element_description = element_description ).
      IF index < lines( value_mappings ).
        write_tag( |  val({ abap_value })=xml('{ <value_mapping>-json }'),| ) ##NO_TEXT.
      ELSE.
        write_tag( |  val({ abap_value })=xml('{ <value_mapping>-json }')| ) ##NO_TEXT.
        write_tag( `"/>` ).
      ENDIF.
      index += 1.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_abap_value.
    CASE element_description->type_kind.
      WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1 OR
           cl_abap_typedescr=>typekind_int2 OR cl_abap_typedescr=>typekind_int8.
        result = |I({ abap_value })|.
      WHEN cl_abap_typedescr=>typekind_num.
        result = |N('{ abap_value }')|.
      WHEN OTHERS.
        result = |'{ abap_value }'|.
    ENDCASE.
  ENDMETHOD.
  METHOD get_name.
    IF next_tag_without_name_and_ref = abap_false.
      result = | name="{ map_and_format_name( name ) }"| ##NO_TEXT.
    ENDIF.
  ENDMETHOD.
  METHOD get_ref.
    IF next_tag_without_name_and_ref = abap_false.
      result = | ref="{ name }"| ##NO_TEXT.
    ENDIF.
  ENDMETHOD.
  METHOD get_ref_for_structure.
    IF next_tag_without_name_and_ref = abap_false.
      result = | tt:ref="{ name }"| ##NO_TEXT.
    ENDIF.
  ENDMETHOD.
  METHOD get_condition_tab_or_struc.
    IF next_tag_without_name_and_ref = abap_true.
      RETURN.
    ENDIF.
    IF abap_doc-required = abap_false AND abap_doc-showalways = abap_false.
      condition = | s-check="not-initial({ type_name })"| ##NO_TEXT.
    ENDIF.
    condition = |{ condition } frq="?"| ##NO_TEXT.
  ENDMETHOD.

  METHOD get_condition_for_element.
    IF next_tag_without_name_and_ref = abap_true.
      RETURN.
    ENDIF.

    IF abap_doc-default IS NOT INITIAL AND abap_doc-required = abap_false.
      DATA(default) = get_default( value_mappings = value_mappings structure_name = element_name element_description = element_description type = type ).
    ENDIF.

    IF abap_doc-required = abap_false AND abap_doc-showalways = abap_false.
      IF default IS NOT INITIAL.
        condition = | s-check="{ element_name }!={ default }"| ##NO_TEXT.
      ELSE.
        condition = | s-check="not-initial({ element_name })"| ##NO_TEXT.
      ENDIF.
    ENDIF.

    condition = |{ condition } frq="?"| ##NO_TEXT.
  ENDMETHOD.

  METHOD get_default.
    default = get_default_value_from_default(
      value_mappings      = value_mappings
      element_description = element_description
      type                = type ).
    IF default IS NOT INITIAL.
      DATA(table) = stack_default_comp_of_struc[ 1 ]-table_of_defaults.
      APPEND VALUE #( var_name = structure_name default_value = default ) TO table.
      stack_default_comp_of_struc[ 1 ]-table_of_defaults = table.
    ENDIF.
  ENDMETHOD.
  METHOD get_value_mapping_via_enum.
    get_structure_of_enum_values(
      EXPORTING
        link_to_values      = abap_doc-enumvalues_link
        fullname_of_type    = fullname_of_type
      IMPORTING
        structure_of_values = DATA(structure_of_values)
        name_of_source      = DATA(name_of_source)
        name_of_constant    = DATA(name_of_constant) ).

    IF structure_of_values IS NOT INITIAL.
      FIELD-SYMBOLS:
        <attr>    TYPE data,
        <fs_data> TYPE any.
      DATA(has_initial_component) = abap_false.
      ASSIGN (name_of_source)=>(name_of_constant) TO <attr>.
      LOOP AT structure_of_values->components ASSIGNING FIELD-SYMBOL(<component>).
        DATA(fullname_of_component) = name_of_constant && '-' && <component>-name.
        DATA(abap_doc_of_component) = call_reader_and_decode( name_of_source = name_of_source element_name = fullname_of_component ).
        IF <component>-type_kind <> enum_type.
          RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e122(zaff_tools) WITH name_of_constant fullname_of_type.
        ENDIF.
        IF abap_doc_of_component-enum_value IS INITIAL.
          DATA(json_name) = map_and_format_name( CONV #( <component>-name ) ).
        ELSE.
          json_name = abap_doc_of_component-enum_value.
        ENDIF.
        ASSIGN COMPONENT <component>-name OF STRUCTURE <attr> TO <fs_data>.
        INSERT VALUE #( abap = <fs_data>  json = json_name ) INTO TABLE value_mappings-value_mappings.
        IF <fs_data> IS INITIAL.
          has_initial_component = abap_true.
        ENDIF.
      ENDLOOP.
      IF has_initial_component = abap_false AND abap_doc-required = abap_false AND abap_doc-default IS INITIAL.
        MESSAGE w127(zaff_tools) INTO DATA(message) ##NEEDED.
        log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD write_tag.
    IF ignore_til_indent_level IS INITIAL OR ignore_til_indent_level - 1 > indent_level.
      APPEND |{ repeat( val = ` `  occ = indent_level * c_indent_number_characters ) }{ line }| TO content.
    ENDIF.
  ENDMETHOD.
  METHOD get_default_value_from_default.
    IF element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
      MESSAGE w117(zaff_tools) WITH 'UTCLONG' INTO DATA(message) ##NEEDED.
      log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
      RETURN.
    ENDIF.

    default = abap_doc-default.
    REPLACE ALL OCCURRENCES OF `"` IN default WITH ``.
    IF default CS '@link'.
      DATA(default_json) = get_default_from_link( link = default fullname_of_type = fullname_of_type element_type = element_description->type_kind ).
      IF default_json IS INITIAL.
        CLEAR default.
        RETURN.
      ENDIF.
      LOOP AT value_mappings ASSIGNING FIELD-SYMBOL(<found>) WHERE json = default_json.
        default = get_prefixed_default(
          value               = <found>-abap
          element_description = element_description ).
      ENDLOOP.
    ELSE.
      IF NOT is_default_value_valid( element_description = element_description default_value = default fullname_of_type = fullname_of_type ).
        CLEAR default.
        RETURN.
      ENDIF.

      IF type <> zif_aff_writer=>type_info-boolean.
        default = get_prefixed_default(
          value               = default
          element_description = element_description ).
      ELSEIF default = `abap_true` OR default = `X`.
        default = `C('X')`.
      ELSE.
        default = `C('')`.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_prefixed_default.
    CASE element_description->type_kind.
      WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1 OR cl_abap_typedescr=>typekind_int2.
        result = |I({ value })|.
      WHEN cl_abap_typedescr=>typekind_int8.
        result = |INT8({ value })|.
      WHEN cl_abap_typedescr=>typekind_float.
        result = |F('{ value }')|.
      WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
        result = |X('{ value }')|.
      WHEN cl_abap_typedescr=>typekind_decfloat16.
        result = |DECFLOAT16('{ value }')|.
      WHEN cl_abap_typedescr=>typekind_decfloat34.
        result = |DECFLOAT34('{ value }')|.
      WHEN cl_abap_typedescr=>typekind_packed.
        result = |P({ value })|.
      WHEN cl_abap_typedescr=>typekind_num.
        result = |N('{ value }')|.
      WHEN cl_abap_typedescr=>typekind_char OR cl_abap_typedescr=>typekind_string.
        result = |C('{ value }')|.
      WHEN cl_abap_typedescr=>typekind_date.
        result = |D('{ value }')|.
      WHEN cl_abap_typedescr=>typekind_time.
        result = |T('{ value }')|.
      WHEN cl_abap_typedescr=>typekind_utclong.
        RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e117(zaff_tools) WITH `UTCLONG`.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e100(zaff_tools) WITH element_description->type_kind.
    ENDCASE.
  ENDMETHOD.
  METHOD write_defaults.
    DATA(actual_entry) = me->stack_default_comp_of_struc[ 1 ].
    DATA list_of_applies LIKE content.
    LOOP AT actual_entry-table_of_defaults ASSIGNING FIELD-SYMBOL(<default>).
      APPEND |{ repeat( val = ` `  occ = ( indent_level * c_indent_number_characters ) - c_indent_number_characters ) }<tt:assign to-ref="{ <default>-var_name }" val="{ <default>-default_value }"/>| TO list_of_applies.
    ENDLOOP.
    INSERT LINES OF list_of_applies INTO content INDEX actual_entry-line_to_insert + 1.
    DELETE me->stack_default_comp_of_struc INDEX 1.
  ENDMETHOD.
  METHOD write_callback_template.
    IF indent_level > 0.
      write_open_tag( line = '<tt:cond>' ).
      IF last_operation( ) <> zif_aff_writer=>operation-open_table.
        DATA(ref_name) = element_name.
      ELSE.
        ref_name = '$ref'.
      ENDIF.
    ELSE.
      ref_name = |.{ st_root_name  }|.
    ENDIF.
    CASE description->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        IF tag IS NOT INITIAL.
          DATA(calculated_tag) = tag.
        ELSE.
          calculated_tag = get_tag_from_type( get_json_type_from_description( CAST cl_abap_elemdescr( description ) ) ).
        ENDIF.
        DATA(component_start) = |<{ calculated_tag }>|.
        DATA(component_end) = |</{ calculated_tag }>|.
      WHEN cl_abap_typedescr=>kind_struct.
        component_start = `<object>`.
        component_end = `</object>`.
      WHEN cl_abap_typedescr=>kind_table.
        component_start = `<array>`.
        component_end = `</array>`.
    ENDCASE.

    write_open_tag( line = |{ component_start } | ).
    write_callback( name_of_callback_class = abap_doc-callback_class parameter_name = element_name ref_name = ref_name ).
    write_closing_tag( line = |  { component_end } | ).
    IF indent_level > 0.
      write_closing_tag( '</tt:cond>' ).
    ENDIF.
    ignore_til_indent_level = indent_level + 1.
  ENDMETHOD.
  METHOD write_callback.
    write_open_tag( line = |<tt:call-method class="{ name_of_callback_class }" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">| ).
    DATA(parameter_name_to_lower) = to_lower( parameter_name ).
    write_tag( line = |<tt:with-parameter name="{ parameter_name_to_lower }" ref="{ ref_name }"/>| ).
    write_closing_tag( '</tt:call-method>' ).
  ENDMETHOD.
  METHOD reset_indent_level_tag.
    IF ignore_til_indent_level - 1 = indent_level.
      CLEAR ignore_til_indent_level.
    ENDIF.
  ENDMETHOD.
  METHOD zif_aff_writer~validate.
    DATA tsource TYPE o2pageline_table.
    APPEND LINES OF source TO tsource.
    TRY.
        cl_o2_api_xsltdesc=>check_transformation_source(
          EXPORTING
            i_name       = 'GENERATED_AFF'
            i_source     = tsource
          IMPORTING
            e_error_list = DATA(errors) ).
      CATCH cx_o2_xslt_error INTO DATA(exception) ##NO_HANDLER.
    ENDTRY.
    IF lines( errors ) > 0 OR exception IS BOUND.
      LOOP AT errors ASSIGNING FIELD-SYMBOL(<error>).
        cl_message_helper=>set_msg_vars_for_clike( <error>-text ).
        log->add_error( message = VALUE #( msgid = 'ZAFF_TOOLS' msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) component_name = `` ).
      ENDLOOP.
      RETURN.
    ENDIF.
    result = abap_true.
  ENDMETHOD.

  METHOD write_iso_language_callback.
    write_callback( name_of_callback_class = 'cl_aff_xslt_callback_language' parameter_name = 'language' ref_name = element_name ).
  ENDMETHOD.

  METHOD enable_extension.

    write_open_tag( |<tt:d-cond frq="*">| ).
    write_open_tag( | <_ tt:lax="on">| ).
    write_open_tag( |<tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">| ).

    DATA(components) = structure_description->get_components( ).
    DATA str_comp TYPE string.
    LOOP AT components INTO DATA(component).
      DATA(formatted_name) = map_and_format_name( name = component-name ).
      IF component-as_include IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      IF sy-tabix = 1.
        str_comp = |{ formatted_name };|.
        CONTINUE.
      ENDIF.
      str_comp = |{ str_comp }{ formatted_name };|.
    ENDLOOP.
    DATA(tag) = |{ repeat( val = ` `  occ = indent_level * c_indent_number_characters ) }<tt:with-parameter name="MEMBERS" val="'{ str_comp }'"/>|.
    IF strlen( tag ) > 255.
      write_tag( |<tt:with-parameter name="MEMBERS"| ).
      IF ignore_til_indent_level IS INITIAL OR ignore_til_indent_level - 1 > indent_level.
        APPEND |val="'{ str_comp }'"/>| TO content.
      ENDIF.
    ELSE.
      write_tag( |<tt:with-parameter name="MEMBERS" val="'{ str_comp }'"/>| ).
    ENDIF.
    write_closing_tag( `</tt:call-method>` ).
    write_tag( |<tt:skip/>| ).
    write_closing_tag( |</_>| ).
    write_closing_tag( |</tt:d-cond>| ).
    write_open_tag( |<tt:d-cond frq="?">| ).
    write_tag( |<__/>| ).
    write_closing_tag( |</tt:d-cond>| ).

  ENDMETHOD.

ENDCLASS.

CLASS zcl_aff_writer_json_schema IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->formatting_option = zif_aff_writer=>formatting_option-camel_case.
    me->schema_id = schema_id.
    me->format_version = format_version.
    zif_aff_writer~set_name_mappings( VALUE #( ( abap = 'schema' json = '$schema' ) ) ) ##NO_TEXT.
  ENDMETHOD.
  METHOD write_element.
    IF ignore_next_elements = abap_true.
      RETURN.
    ENDIF.

    clear_type_specifics( ).
    CLEAR enum_titles.
    CLEAR enum_descriptions.
    CLEAR enum_values.

    append_comma_to_prev_line( ).
    DATA(json_type) = get_json_type_from_description( element_description ).
    DATA(mapped_and_formatted_name) = map_and_format_name( element_name ).

    DATA(splitted_prev_name) = get_splitted_absolute_name( element_description->absolute_name ).
    set_abapdoc_fullname_element( element_description = element_description element_name = element_name splitted_prev_name = splitted_prev_name ).

    IF abap_doc-required = abap_true AND lines( stack_of_required_tabs ) >= 1.
      FIELD-SYMBOLS <table1> TYPE string_table.
      ASSIGN stack_of_required_tabs[ 1 ] TO <table1>.
      APPEND mapped_and_formatted_name TO <table1>.
    ENDIF.

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class component_name = fullname_of_type ).
      IF last_operation( ) = zif_aff_writer=>operation-initial.
        open_json_schema_for_element( ).
      ENDIF.
      write_subschema( callback_class = callback_class ).
      IF last_operation( ) = zif_aff_writer=>operation-initial.
        write_closing_tag( `}` ).
      ENDIF.
      CLEAR ignore_til_indent_level.
      RETURN.
    ENDIF.
    IF last_operation( ) = zif_aff_writer=>operation-initial.
      open_json_schema_for_element( ).
    ELSEIF last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_open_tag( |"{ mapped_and_formatted_name }": \{| ).
    ENDIF.

    DATA(enums) = get_enum_values( element_name = element_name element_description = element_description ).
    IF enums IS NOT INITIAL.
      json_type = zif_aff_writer=>type_info-string.
    ENDIF.

    DATA(check_not_needed) = abap_false.

    IF last_operation( ) = zif_aff_writer=>operation-open_table AND lines( splitted_prev_name ) = 2 AND splitted_prev_name[ 2 ] = element_name.
      check_not_needed = abap_true.
    ENDIF.

    write_title_and_description( type_description = element_description check_not_needed = check_not_needed ).

    IF element_name = c_format_version.
      write_tag( `"type": "string",` ).
      write_tag( |"const": "{ format_version }",| ).
    ELSE.
      write_tag( |"type": "{ get_json_schema_type( element_name = element_name element_description = element_description json_type = json_type ) }",| ).
      DATA(format) = get_format( element_description ).
      IF format IS NOT INITIAL.
        write_tag( |"format": "{ format }",| ).
      ENDIF.

      IF enums IS NOT INITIAL.
        handle_enums( element_description = element_description element_name = element_name enums = enums ).
      ELSE. "non- enum
        IF json_type = zif_aff_writer=>type_info-numeric.
          handle_extrema( element_description = element_description element_name = element_name ).
        ELSEIF json_type = zif_aff_writer=>type_info-string AND NOT ( element_description->type_kind = cl_abap_typedescr=>typekind_date OR element_description->type_kind = cl_abap_typedescr=>typekind_time OR
             element_description->type_kind = cl_abap_typedescr=>typekind_utclong ).
          IF is_sy_langu( element_description ).
            handle_language_field( ).
          ELSE.
            handle_string( element_description = element_description ).
          ENDIF.
        ENDIF.
      ENDIF.

      IF abap_doc-default IS NOT INITIAL.
        handle_default( element_description = element_description json_type = json_type ).
      ENDIF.
    ENDIF.

*    remove "," in last line
    IF ignore_til_indent_level > indent_level OR ignore_til_indent_level IS INITIAL.
      DATA(last_line) = content[ lines( content ) ].
      content[ lines( content ) ] = substring( val = last_line off = 0 len = strlen( last_line ) - 1 ).
    ENDIF.

    IF last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_closing_tag( `}` ).
    ENDIF.
  ENDMETHOD.
  METHOD write_title_and_description.
    IF check_not_needed = abap_false.
      check_title_and_description( abap_doc_to_check = abap_doc fullname_of_checked_type = fullname_of_type ).
    ENDIF.
    DATA(title) = escape( val = abap_doc-title format = cl_abap_format=>e_json_string ).
    DATA(description) = escape( val = get_description( type_description = type_description ) format = cl_abap_format=>e_json_string ).
    IF title IS NOT INITIAL.
      write_tag( |"title": "{ title }",| ).
    ENDIF.
    IF description IS NOT INITIAL.
      write_tag( |"description": "{ description }",| ).
    ENDIF.
  ENDMETHOD.
  METHOD handle_enums.
    write_tag( `"enum": [` ).
    write_enum_properties( enums ).

    IF enum_titles IS NOT INITIAL.
      write_tag( `"enumTitles": [` ).
      write_enum_properties( enum_titles ).
    ENDIF.

    DATA(enum_descr) = get_enum_descriptions( element_name = element_name element_description = element_description ).
    write_tag( `"enumDescriptions": [` ).
    write_enum_properties( enum_descr ).
  ENDMETHOD.
  METHOD write_enum_properties.
    indent_level = indent_level + 1.
    LOOP AT property_table ASSIGNING FIELD-SYMBOL(<value>).
      IF sy-tabix < lines( property_table ).
        write_tag( |"{ <value> }",| ).
      ELSE.
        write_tag( |"{ <value> }"| ).
      ENDIF.
    ENDLOOP.
    indent_level = indent_level - 1.
    write_tag( `],` ).
  ENDMETHOD.
  METHOD handle_string.
    IF abap_doc-max_length IS NOT INITIAL.
      DATA(max_length) = abap_doc-max_length.
    ELSE.
      max_length = get_max_length( element_description ).
    ENDIF.
    IF abap_doc-min_length IS NOT INITIAL.
      write_tag( |"minLength": { abap_doc-min_length },| ).
    ENDIF.
    IF max_length IS NOT INITIAL.
      write_tag( |"maxLength": { max_length },| ).
      IF element_description->type_kind = cl_abap_typedescr=>typekind_num.
        write_tag( `"pattern": "^[0-9]+$",` ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD handle_extrema.
    IF get_value_mapping_for_element( element_name ) IS INITIAL.
      get_extrema(
        EXPORTING
          element_description = element_description
        IMPORTING
          max                 = DATA(max_value)
          min                 = DATA(min_value) ).
    ENDIF.
    DATA(multiple_of) = abap_doc-multiple_of.

    IF multiple_of IS INITIAL AND element_description->type_kind = cl_abap_typedescr=>typekind_packed.
      DATA(decimals) = element_description->decimals.
      IF decimals > 0.
        multiple_of = |0.{ repeat( val = `0`  occ = decimals - 1 ) }1|.
      ENDIF.
    ENDIF.

    DATA(exclusive_minimum) = abap_doc-exclusive_minimum.
    DATA(exclusive_maximum) = abap_doc-exclusive_maximum.

    IF exclusive_minimum IS NOT INITIAL.
      CLEAR min_value.
    ELSEIF abap_doc-minimum IS NOT INITIAL.
      min_value = abap_doc-minimum.
    ENDIF.

    IF exclusive_maximum IS NOT INITIAL.
      CLEAR max_value.
    ELSEIF abap_doc-maximum IS NOT INITIAL.
      max_value = abap_doc-maximum.
    ENDIF.

    IF min_value IS NOT INITIAL.
      write_tag( |"minimum": { min_value },| ).
    ENDIF.
    IF exclusive_minimum IS NOT INITIAL.
      write_tag( |"exclusiveMinimum": { exclusive_minimum },| ).
    ENDIF.
    IF max_value IS NOT INITIAL.
      write_tag( |"maximum": { max_value },| ).
    ENDIF.
    IF exclusive_maximum IS NOT INITIAL.
      write_tag( |"exclusiveMaximum": { exclusive_maximum },| ).
    ENDIF.

    IF multiple_of IS NOT INITIAL.
      write_tag( |"multipleOf": { multiple_of },| ).
    ENDIF.
  ENDMETHOD.
  METHOD handle_default.
    DATA(default) = abap_doc-default.
    IF abap_doc-default CS '@link'.
      default = get_default_from_link( link = abap_doc-default fullname_of_type = fullname_of_type element_type = element_description->type_kind ).
      IF default IS INITIAL.
        RETURN.
      ENDIF.
      default = |"{ default }"|.
    ELSEIF is_default_value_valid( element_description = element_description default_value = default fullname_of_type = fullname_of_type ).
      IF json_type = zif_aff_writer=>type_info-numeric OR json_type = zif_aff_writer=>type_info-boolean.
        REPLACE ALL OCCURRENCES OF `"` IN default WITH ``.
      ELSEIF json_type = zif_aff_writer=>type_info-date_time.
        default = `"` && date_time_from_abap_to_json( date_time_abap = default element_description = element_description ) && `"`.
      ENDIF.
      IF json_type = zif_aff_writer=>type_info-numeric.
        REPLACE `E` IN default WITH `e`.
      ENDIF.
      IF json_type = zif_aff_writer=>type_info-boolean.
        IF default = 'X' OR default = 'abap_true'.
          default = 'true' ##NO_TEXT.
        ELSE.
          default = 'false' ##NO_TEXT.
        ENDIF.
      ENDIF.
    ELSE.
      RETURN.
    ENDIF.

    write_tag( |"default": { default },| ).
  ENDMETHOD.
  METHOD open_structure.
    clear_type_specifics( ).
*  add a new empty required_table to the stack
    IF last_operation( ) = zif_aff_writer=>operation-initial.
      INSERT VALUE #( name = structure_name absolute_name = structure_description->absolute_name ) INTO me->stack_of_structure INDEX 1.
      add_required_table_to_stack( ).
      open_json_schema_for_structure( structure_name = structure_name structure_description = structure_description ).
      INSERT VALUE #( name = structure_name number_brackets = 2 ) INTO me->structure_buffer INDEX 1.
      RETURN.
    ENDIF.

    append_comma_to_prev_line( ).

    DATA(mapped_and_formatted_name) = map_and_format_name( structure_name ).

    set_abapdoc_fullname_struc_tab( type_description = structure_description type_name  = structure_name ).

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class component_name = fullname_of_type ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    INSERT VALUE #( name = structure_name absolute_name = structure_description->absolute_name ) INTO me->stack_of_structure INDEX 1.

    IF last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_open_tag( |"{ mapped_and_formatted_name }": \{| ).
      INSERT VALUE #( name = structure_name number_brackets = 2 ) INTO me->structure_buffer INDEX 1.
    ELSE.
      INSERT VALUE #( name = structure_name number_brackets = 1 ) INTO me->structure_buffer INDEX 1.
    ENDIF.

    write_title_and_description( structure_description ).

    IF abap_doc-required = abap_true.
      FIELD-SYMBOLS <table1> TYPE string_table.
      ASSIGN stack_of_required_tabs[ 1 ] TO <table1>.
      APPEND mapped_and_formatted_name TO <table1>.
    ENDIF.
    write_tag( `"type": "object",` ).
    write_open_tag( `"properties": {` ).
    add_required_table_to_stack( ).
  ENDMETHOD.
  METHOD close_structure.
    delete_first_of_struc_stack( ).
    DO me->structure_buffer[ 1 ]-number_brackets TIMES.
      IF me->structure_buffer[ 1 ]-number_brackets = 2 AND sy-index = 2.
        write_req_and_add_props( ).
      ENDIF.
      write_closing_tag( `}` ).
      IF me->structure_buffer[ 1 ]-number_brackets = 1.
        write_req_and_add_props( ).
      ENDIF.
    ENDDO.
    DELETE me->structure_buffer INDEX 1.
    reset_indent_level_tag( ).
  ENDMETHOD.
  METHOD open_table.
    clear_type_specifics( ).
    IF last_operation( ) = zif_aff_writer=>operation-initial.
      open_json_schema_for_table( table_name = table_name  table_description = CAST cl_abap_tabledescr( table_description ) ).
      INSERT VALUE #( name = table_name number_brackets = 2 ) INTO TABLE me->table_buffer.
      RETURN.
    ENDIF.
    append_comma_to_prev_line( ).
    DATA(mapped_and_formatted_name) = map_and_format_name( table_name ).

    set_abapdoc_fullname_struc_tab( type_description = table_description type_name = table_name ).

    IF abap_doc-required = abap_true AND lines( stack_of_required_tabs ) >= 1.
      FIELD-SYMBOLS <table1> TYPE string_table.
      ASSIGN stack_of_required_tabs[ 1 ] TO <table1>.
      APPEND mapped_and_formatted_name TO <table1>.
    ENDIF.

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class component_name = fullname_of_type ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    IF last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_open_tag( |"{ mapped_and_formatted_name }": \{| ).
      INSERT VALUE #( name = table_name number_brackets = 2 ) INTO TABLE me->table_buffer.
    ELSE.
      INSERT VALUE #( name = table_name number_brackets = 1 ) INTO TABLE me->table_buffer.
    ENDIF.

    write_title_and_description( table_description ).

    write_tag( `"type": "array",` ).
    IF CAST cl_abap_tabledescr( table_description )->has_unique_key = abap_true.
      write_tag( `"uniqueItems": true,` ).
    ENDIF.
    write_open_tag( `"items": {` ).
  ENDMETHOD.
  METHOD close_table.
    DO me->table_buffer[ name = table_name ]-number_brackets TIMES.
      write_closing_tag( `}` ).
    ENDDO.
    DELETE me->table_buffer WHERE name = table_name.
    reset_indent_level_tag( ).
  ENDMETHOD.
  METHOD append_comma_to_prev_line.
    IF ( last_operation( ) = zif_aff_writer=>operation-write_element OR
       last_operation( ) = zif_aff_writer=>operation-close_structure OR
       last_operation( ) = zif_aff_writer=>operation-close_table ) AND ( ignore_til_indent_level > indent_level OR ignore_til_indent_level IS INITIAL ).
      append_to_previous_line( `,` ).
    ENDIF.
  ENDMETHOD.

  METHOD set_abapdoc_fullname_struc_tab.
    IF last_operation( ) = zif_aff_writer=>operation-open_table.
      DATA(absolute_name) = type_description->absolute_name.
      DATA(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
      DATA(source_type) = splitted_absolute_name[ 1 ].
      DATA(source) = splitted_absolute_name[ 2 ].
      fullname_of_type = type_name.
      DATA(already_found) = abap_true.
    ELSE.
      get_all_path_information(
        EXPORTING
          name             = type_name
        IMPORTING
          source_type      = source_type
          source           = source
          fullname_of_type = fullname_of_type ).
    ENDIF.

    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      abap_doc = call_reader_and_decode( name_of_source = source element_name = fullname_of_type ).
    ENDIF.
    IF already_found = abap_false.
      DATA(abap_doc_second) = get_abap_doc_for_absolute_name( type_description->absolute_name ).
      compare_abap_doc(
        EXPORTING
          abap_doc_additional = abap_doc_second
        CHANGING
          abap_doc_base       = abap_doc ).
    ENDIF.
    check_redundant_annotations( ).
  ENDMETHOD.
  METHOD set_abapdoc_fullname_element.
* Simple Component of a structure, defined in the structure definition
    IF lines( stack_of_structure ) > 0.
      get_all_path_information(
        EXPORTING
          name             = element_name
        IMPORTING
          source_type      = DATA(source_type)
          source           = DATA(source)
          fullname_of_type = fullname_of_type ).

* Element which is in no structure
    ELSEIF lines( stack_of_structure ) = 0.
      fullname_of_type = element_name.
      source_type = splitted_prev_name[ 1 ].
      source = splitted_prev_name[ 2 ].
      DATA(already_searched) = abap_true.
    ENDIF.

    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      abap_doc = call_reader_and_decode( name_of_source = source element_name   = fullname_of_type ).
    ENDIF.

    IF already_searched = abap_false.
      DATA(abap_doc_second) = get_abap_doc_for_absolute_name( absolute_name = element_description->absolute_name ).
      compare_abap_doc(
        EXPORTING
          abap_doc_additional = abap_doc_second
        CHANGING
          abap_doc_base       = abap_doc ).
    ENDIF.
    check_redundant_annotations( ).
  ENDMETHOD.
  METHOD get_json_schema_type.
    DATA(value_mapping) = get_value_mapping_for_element( element_name ).
    IF value_mapping IS NOT INITIAL.
      DATA(type) = value_mapping-target_type.
    ELSE.
      type = json_type.
    ENDIF.
    IF type = zif_aff_writer=>type_info-numeric.
      result = 'number' ##NO_TEXT.
      IF type_is_integer( element_description ) = abap_true.
        result = 'integer'  ##NO_TEXT.
      ENDIF.
    ELSEIF type = zif_aff_writer=>type_info-date_time.
      result = 'string' ##NO_TEXT.
    ELSE.
      result = to_lower( type ).
    ENDIF.
  ENDMETHOD.
  METHOD open_json_schema_for_structure.
    DATA(absolute_name) = stack_of_structure[ 1 ]-absolute_name.
    DATA(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
    DATA(source_type) = splitted_absolute_name[ 1 ].
    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      DATA(source) = splitted_absolute_name[ 2 ].
      abap_doc = call_reader_and_decode( name_of_source = source element_name = structure_name ).
    ENDIF.
    fullname_of_type = structure_name.
    check_redundant_annotations( ).
    write_open_tag( '{' ).
    write_tag( |"$comment": "This file is autogenerated, do not edit manually, see { c_link_to_repository } for more information.",| ) ##NO_TEXT.
    write_tag( |"$schema": "{ c_schema_specification }",| ).
    write_tag( |"$id": "{ me->schema_id }",| ).
    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class component_name = structure_name ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    write_title_and_description( structure_description ).

    write_tag( '"type": "object",' ).
    write_open_tag( '"properties": {' ).
  ENDMETHOD.
  METHOD open_json_schema_for_table.
    DATA(absolute_name) = table_description->absolute_name.
    DATA(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
    DATA(source_type) = splitted_absolute_name[ 1 ].
    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      DATA(source) = splitted_absolute_name[ 2 ].
      abap_doc = call_reader_and_decode( name_of_source = source element_name   = table_name ).
    ENDIF.
    fullname_of_type = table_name.
    check_redundant_annotations( ).
    write_open_tag( '{' ).
    write_tag( |"$comment": "This file is autogenerated, do not edit manually, see { c_link_to_repository } for more information.",| ) ##NO_TEXT.
    write_tag( |"$schema": "{ c_schema_specification }",| ).
    write_tag( |"$id": "{ me->schema_id }",| ).

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class component_name = table_name ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    write_title_and_description( table_description ).

    write_tag( '"type": "array",' ).
    IF table_description->has_unique_key = abap_true.
      write_tag( '"uniqueItems": true,' ).
    ENDIF.
    write_open_tag( '"items": {' ).
  ENDMETHOD.
  METHOD write_subschema.
    TRY.
        DATA subschema TYPE string_table.
        CALL METHOD (callback_class)=>get_subschema
          RECEIVING
            subschema = subschema.
        LOOP AT subschema ASSIGNING FIELD-SYMBOL(<line>).
          write_tag( <line> ).
        ENDLOOP.
        ignore_til_indent_level = indent_level.
      CATCH cx_sy_dyn_call_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.
  METHOD open_json_schema_for_element.
    write_open_tag( '{' ).
    write_tag( |"$comment": "This file is autogenerated, do not edit manually, see { c_link_to_repository } for more information.",| ) ##NO_TEXT.
    write_tag( |"$schema": "{ c_schema_specification }",| ).
    write_tag( |"$id": "{ me->schema_id }",| ).
  ENDMETHOD.
  METHOD get_description.
    IF abap_doc-description IS NOT INITIAL.
      result = abap_doc-description.
    ELSEIF type_description IS SUPPLIED.
      DATA element_description TYPE REF TO cl_abap_elemdescr.
      TRY.
          element_description = CAST cl_abap_elemdescr( type_description ).
        CATCH cx_sy_move_cast_error.
          RETURN.
      ENDTRY.
      element_description->get_ddic_field(
        EXPORTING
          p_langu    = 'E'
        RECEIVING
          p_flddescr = DATA(ddic_field)
        EXCEPTIONS
          OTHERS     = 1 ) ##SUBRC_OK.
      IF ddic_field IS NOT INITIAL AND ddic_field-fieldtext IS NOT INITIAL.
        result = ddic_field-fieldtext.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_enum_values.
    DATA(value_mapping) = get_value_mapping_for_element( element_name ).
    IF value_mapping IS NOT INITIAL.
      LOOP AT value_mapping-value_mappings ASSIGNING FIELD-SYMBOL(<mapping>).
        APPEND <mapping>-json  TO result.
      ENDLOOP.
    ELSEIF abap_doc-enumvalues_link IS NOT INITIAL.
      set_enum_properties( element_description->type_kind ).
      result = enum_values.
    ELSE.
      IF get_json_type_from_description( element_description ) = zif_aff_writer=>type_info-boolean.
        RETURN.
      ENDIF.
      element_description->get_ddic_fixed_values(
        RECEIVING
          p_fixed_values = DATA(ddic_fixed_values)
        EXCEPTIONS
          OTHERS         = 1 ) ##SUBRC_OK.
      IF ddic_fixed_values IS INITIAL.
        RETURN.
      ENDIF.
      LOOP AT ddic_fixed_values ASSIGNING FIELD-SYMBOL(<value>).
        DATA text TYPE string.
        text = <value>-ddtext.
        REPLACE ALL OCCURRENCES OF REGEX '\s' IN text WITH '_'  ##REGEX_POSIX.
        APPEND apply_formatting( text ) TO result.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_enum_descriptions.
    DATA(value_mapping) = get_value_mapping_for_element( element_name ).
    IF value_mapping IS NOT INITIAL.
      LOOP AT value_mapping-value_mappings ASSIGNING FIELD-SYMBOL(<mapping>).
        APPEND <mapping>-json TO result.
      ENDLOOP.
    ELSEIF abap_doc-enumvalues_link IS NOT INITIAL.
      result = enum_descriptions.
    ELSE.
      element_description->get_ddic_fixed_values(
        RECEIVING
          p_fixed_values = DATA(ddic_fixed_values)
        EXCEPTIONS
          OTHERS         = 1 ) ##SUBRC_OK.
      IF ddic_fixed_values IS NOT INITIAL.
        LOOP AT ddic_fixed_values ASSIGNING FIELD-SYMBOL(<value>).
          APPEND <value>-ddtext TO result.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD type_is_integer.
    result = abap_false.
    IF element_description->type_kind = cl_abap_typedescr=>typekind_int OR
       element_description->type_kind = cl_abap_typedescr=>typekind_int1 OR
       element_description->type_kind = cl_abap_typedescr=>typekind_int2 OR
       element_description->type_kind = cl_abap_typedescr=>typekind_int8.
      result = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD set_enum_properties.
    get_structure_of_enum_values(
      EXPORTING
        link_to_values      = abap_doc-enumvalues_link
        fullname_of_type    = fullname_of_type
      IMPORTING
        structure_of_values = DATA(structure_of_values)
        name_of_source      = DATA(name_of_source)
        name_of_constant    = DATA(name_of_constant) ).

    IF structure_of_values IS NOT INITIAL.
      DATA(has_initial_component) = abap_false.
      FIELD-SYMBOLS:
        <attr>    TYPE data,
        <fs_data> TYPE any.
      ASSIGN (name_of_source)=>(name_of_constant) TO <attr>.
      LOOP AT structure_of_values->components ASSIGNING FIELD-SYMBOL(<component>).
        IF <component>-type_kind <> enum_type.
          RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e122(zaff_tools) WITH name_of_constant fullname_of_type.
        ENDIF.

        ASSIGN COMPONENT <component>-name OF STRUCTURE <attr> TO <fs_data>.
        IF <fs_data> IS INITIAL.
          has_initial_component = abap_true.
        ENDIF.
        DATA(fullname_of_value) = name_of_constant && '-' && <component>-name.
        DATA(abap_doc_of_component) = call_reader_and_decode( name_of_source = name_of_source element_name   = fullname_of_value ).
        IF abap_doc_of_component-enum_value IS INITIAL.
          DATA(enum_value) = apply_formatting( CONV #( <component>-name ) ).
        ELSE.
          enum_value = abap_doc_of_component-enum_value.
        ENDIF.

        APPEND enum_value TO enum_values.
        APPEND abap_doc_of_component-description TO enum_descriptions.
        APPEND abap_doc_of_component-title TO enum_titles.

        check_title_and_description( abap_doc_to_check = abap_doc_of_component fullname_of_checked_type = fullname_of_value ).
      ENDLOOP.
      IF has_initial_component = abap_false AND abap_doc-required = abap_false AND abap_doc-default IS INITIAL.
        MESSAGE w127(zaff_tools) INTO DATA(message) ##NEEDED.
        log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD add_required_table_to_stack.
    INSERT VALUE #( ) INTO stack_of_required_tabs INDEX 1.
  ENDMETHOD.
  METHOD delete_first_tab_of_req_stack.
    IF stack_of_required_tabs IS NOT INITIAL.
      DELETE stack_of_required_tabs INDEX 1.
    ENDIF.
  ENDMETHOD.
  METHOD write_req_and_add_props.
    IF ignore_til_indent_level > indent_level OR ignore_til_indent_level IS INITIAL.
      content[ lines( content ) ] = content[ lines( content ) ] && `,`.
      write_tag( `"additionalProperties": false` ).
      IF stack_of_required_tabs[ 1 ] IS NOT INITIAL.
        content[ lines( content ) ] = content[ lines( content ) ] && `,`.
        write_tag( `"required": [` ).
        indent_level = indent_level + 1.
        LOOP AT stack_of_required_tabs[ 1 ] ASSIGNING FIELD-SYMBOL(<required_comp>).
          IF sy-tabix < lines( stack_of_required_tabs[ 1 ] ).
            write_tag( |"{ <required_comp> }",| ).
          ELSE.
            write_tag( |"{ <required_comp> }"| ).
          ENDIF.
        ENDLOOP.
        indent_level = indent_level - 1.
        write_tag( `]` ).
      ENDIF.
    ENDIF.
    delete_first_tab_of_req_stack( ).
  ENDMETHOD.
  METHOD get_format.
    IF element_description->type_kind = cl_abap_typedescr=>typekind_date OR
    element_description->type_kind = cl_abap_typedescr=>typekind_time OR
    element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
      result = `date-time` ##NO_TEXT.
    ENDIF.
  ENDMETHOD.
  METHOD write_tag.
    IF ignore_til_indent_level IS INITIAL OR ignore_til_indent_level > indent_level.
      APPEND |{ repeat( val = ` `  occ = indent_level * c_indent_number_characters ) }{ line }| TO content.
    ENDIF.
  ENDMETHOD.
  METHOD date_time_from_abap_to_json.
    DATA(abap_date) = date_time_abap.
    REPLACE ALL OCCURRENCES OF `"` IN abap_date WITH ``.
    IF element_description->type_kind = cl_abap_typedescr=>typekind_date.
      IF strlen( abap_date ) = 8.
        date_time_json = abap_date+0(4) && `-` && abap_date+4(2) && `-` && abap_date+6(2).
      ELSEIF strlen( abap_date ) = 6.
        date_time_json = abap_date+0(4) && `-` && abap_date+4(2).
      ELSE.
        date_time_json = abap_date.
      ENDIF.
    ELSEIF element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
      date_time_json = abap_date+0(19) && `+00:00`.
    ELSEIF element_description->type_kind = cl_abap_typedescr=>typekind_time.
      DATA(difference) = 6 - strlen( abap_date ).
      IF difference > 0.
        abap_date = abap_date && repeat( val = '0' occ = difference ).
      ENDIF.
      date_time_json = abap_date+0(2) && `:` && abap_date+2(2) && `:` && abap_date+4(2).
    ENDIF.
  ENDMETHOD.
  METHOD reset_indent_level_tag.
    IF ignore_til_indent_level = indent_level.
      CLEAR ignore_til_indent_level.
    ENDIF.
  ENDMETHOD.
  METHOD append_after_output.
    APPEND `` TO output.
  ENDMETHOD.
  METHOD check_title_and_description.
    IF ignore_til_indent_level IS INITIAL OR ignore_til_indent_level > indent_level. "Only write message if no callback class provided
      IF abap_doc_to_check-title IS INITIAL.
        MESSAGE i119(zaff_tools) WITH 'Title' INTO DATA(message) ##NEEDED ##NO_TEXT.
        log->add_info( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_checked_type ).
      ENDIF.

      IF abap_doc_to_check-description IS INITIAL.
        MESSAGE i119(zaff_tools) WITH 'Description' INTO message ##NO_TEXT.
        log->add_info( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_checked_type ).
      ELSEIF strlen( abap_doc_to_check-description ) > c_max_length_of_description.
        MESSAGE w125(zaff_tools) WITH c_max_length_of_description INTO message.
        log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_checked_type ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD zif_aff_writer~validate.
    TRY.
        DATA(string) = concat_lines_of( table = source sep = cl_abap_char_utilities=>newline ).
        DATA(json_as_xstring) = cl_abap_codepage=>convert_to( string ).
        DATA(json_reader) = cl_sxml_string_reader=>create( json_as_xstring ).
        json_reader->next_node( ).
        json_reader->skip_node( ).
      CATCH cx_sxml_parse_error INTO DATA(exception).
        log->add_exception( exception = exception component_name = `` ).
        RETURN.
    ENDTRY.
    result = abap_true.
  ENDMETHOD.
  METHOD handle_language_field.
    write_tag( `"minLength": 2,` ).
    write_tag( `"maxLength": 2,` ).
    write_tag( `"pattern": "^[a-z]+$",` ).
  ENDMETHOD.
  METHOD get_max_length.
    DATA(length) = element_description->output_length.
    IF length > 0.
      DATA length_as_string TYPE string.
      length_as_string = length.
      remove_leading_trailing_spaces( CHANGING string_to_work_on = length_as_string ).
      result = length_as_string.
    ENDIF.
  ENDMETHOD.
  METHOD get_extrema.
    DATA r_field TYPE REF TO data.
    FIELD-SYMBOLS <field> TYPE any.
    CREATE DATA r_field TYPE HANDLE element_description.
    ASSIGN r_field->* TO <field>.

    DATA(max_val) = cl_abap_exceptional_values=>get_max_value( <field> ).
    ASSIGN max_val->* TO FIELD-SYMBOL(<max>).
    IF <max> IS ASSIGNED.
      max = <max>.
      REPLACE ALL OCCURRENCES OF 'E' IN max WITH 'e'.
      REPLACE ALL OCCURRENCES OF '+' IN max WITH ''.
      remove_leading_trailing_spaces( CHANGING string_to_work_on = max ).
    ENDIF.

    IF element_description->type_kind = cl_abap_typedescr=>typekind_decfloat OR
          element_description->type_kind = cl_abap_typedescr=>typekind_decfloat16 OR
          element_description->type_kind = cl_abap_typedescr=>typekind_decfloat34.
      IF <max> IS ASSIGNED.
        min = '-' && max.
      ENDIF.
    ELSE.
      DATA(min_val) = cl_abap_exceptional_values=>get_min_value( <field> ).
      ASSIGN min_val->* TO FIELD-SYMBOL(<min>).
      IF <min> IS ASSIGNED.
        DATA min_str TYPE string.
        min_str = <min>.
        DATA(length) = strlen( min_str ) - 1.
        DATA(front) = substring( val = min_str off = 0  len = length ).
        DATA(back) = substring( val = min_str off = length  len = 1 ).
        IF back = '-'.
          min = back && front.
        ELSE.
          min = min_str.
        ENDIF.
        REPLACE ALL OCCURRENCES OF 'E' IN min WITH 'e'.
        REPLACE ALL OCCURRENCES OF '+' IN min WITH ''.
        remove_leading_trailing_spaces( CHANGING string_to_work_on = min ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_aff_writer IMPLEMENTATION.

  METHOD constructor.
    log = NEW zcl_aff_log( ).
    abap_doc_parser = NEW zcl_aff_abap_doc_parser( ).
  ENDMETHOD.
  METHOD zif_aff_writer~set_abap_value_mappings.
    me->abap_value_mappings = abap_value_mappings.
  ENDMETHOD.
  METHOD zif_aff_writer~set_name_mappings.
    me->name_mappings = name_mappings.
  ENDMETHOD.
  METHOD zif_aff_writer~set_formatting_option.
    me->formatting_option = formatting_option.
  ENDMETHOD.
  METHOD get_value_mapping_for_element.
    DATA(abap_element_upper) = to_upper( abap_element ).
    DATA(abap_value_mappings_upper) = VALUE zif_aff_writer=>ty_abap_value_mappings(
      FOR abap_value_mapping IN me->abap_value_mappings (
        abap_element   = to_upper( abap_value_mapping-abap_element )
        target_type    = abap_value_mapping-target_type
        value_mappings = abap_value_mapping-value_mappings
      ) ).
    result = VALUE #( abap_value_mappings_upper[ abap_element = abap_element_upper ] OPTIONAL ) ##WARN_OK.
  ENDMETHOD.
  METHOD map_and_format_name.
    DATA(mapped_name) = me->get_mapped_name( name ).
    IF mapped_name IS NOT INITIAL.
      result = mapped_name.
    ELSE.
      result = me->apply_formatting( name ).
    ENDIF.
  ENDMETHOD.
  METHOD get_mapped_name.
    DATA(name_upper) = to_upper( name ).
    DATA(name_mappings_upper) = VALUE zif_aff_writer=>ty_name_mappings(
      FOR name_mapping IN me->name_mappings (
        abap = to_upper( name_mapping-abap )
        json = name_mapping-json
      ) ).
    result = VALUE #( name_mappings_upper[ abap = name_upper ]-json OPTIONAL ) ##WARN_OK.
  ENDMETHOD.
  METHOD apply_formatting.
    CASE me->formatting_option.
      WHEN zif_aff_writer=>formatting_option-camel_case.
        DATA(lower_name) = to_lower( name ).
        result = to_mixed( lower_name ).
      WHEN OTHERS.
        result = name.
    ENDCASE.
  ENDMETHOD.
  METHOD get_json_type_from_description.
    CASE element_description->type_kind.
      WHEN cl_abap_typedescr=>typekind_string OR cl_abap_typedescr=>typekind_csequence OR
           cl_abap_typedescr=>typekind_clike OR cl_abap_typedescr=>typekind_char OR
           cl_abap_typedescr=>typekind_w OR cl_abap_typedescr=>typekind_xstring OR
           cl_abap_typedescr=>typekind_hex OR cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_enum.
        result = COND #( WHEN is_type_boolean( element_description ) THEN zif_aff_writer=>type_info-boolean
                         ELSE zif_aff_writer=>type_info-string ).
      WHEN cl_abap_typedescr=>typekind_float OR cl_abap_typedescr=>typekind_int OR
           cl_abap_typedescr=>typekind_int1 OR cl_abap_typedescr=>typekind_int2 OR
           cl_abap_typedescr=>typekind_int8 OR cl_abap_typedescr=>typekind_decfloat OR
           cl_abap_typedescr=>typekind_decfloat16 OR cl_abap_typedescr=>typekind_decfloat34  OR cl_abap_typedescr=>typekind_numeric.
        result = zif_aff_writer=>type_info-numeric.
      WHEN cl_abap_typedescr=>typekind_packed.
        result = COND #( WHEN is_type_timestamp( element_description ) THEN zif_aff_writer=>type_info-date_time
                         ELSE zif_aff_writer=>type_info-numeric ).
      WHEN cl_abap_typedescr=>typekind_date OR cl_abap_typedescr=>typekind_time OR
           cl_abap_typedescr=>typekind_utclong.
        result = zif_aff_writer=>type_info-date_time.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e100(zaff_tools) WITH element_description->type_kind.
    ENDCASE.
  ENDMETHOD.
  METHOD is_type_boolean.
    DATA(type_name) = element_description->get_relative_name( ).
    result = xsdbool( element_description->output_length = 1 AND ( type_name IS NOT INITIAL AND c_abap_types-boolean CS type_name ) ).
  ENDMETHOD.
  METHOD is_type_timestamp.
    DATA(type_name) = element_description->get_relative_name( ).
    result = xsdbool( type_name IS NOT INITIAL AND c_abap_types-timestamp CS type_name ).
  ENDMETHOD.
  METHOD zif_aff_writer~write_element.
    write_element( element_name = element_name element_description = element_description ).
    add_to_stack( VALUE #( operation = zif_aff_writer=>operation-write_element name = element_name ) ).
  ENDMETHOD.
  METHOD zif_aff_writer~open_node.
    CASE node_description->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        open_structure( structure_name = node_name  structure_description = node_description ).
        add_to_stack( VALUE #( operation = zif_aff_writer=>operation-open_structure name = node_name ) ).

      WHEN cl_abap_typedescr=>kind_table.
        open_table( table_name = node_name  table_description = node_description ).
        add_to_stack( VALUE #( operation = zif_aff_writer=>operation-open_table name = node_name ) ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e101(zaff_tools) WITH node_description->kind.
    ENDCASE.
  ENDMETHOD.
  METHOD zif_aff_writer~close_node.
    CASE node_description->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        close_structure( structure_name = node_name  structure_description = node_description ).
        add_to_stack( VALUE #( operation = zif_aff_writer=>operation-close_structure name = node_name ) ).

      WHEN cl_abap_typedescr=>kind_table.
        close_table( table_name = node_name  table_description = node_description ).
        add_to_stack( VALUE #( operation = zif_aff_writer=>operation-close_table name = node_name ) ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e101(zaff_tools) WITH node_description->kind.
    ENDCASE.
  ENDMETHOD.
  METHOD zif_aff_writer~get_output.
    append_before_output( ).
    APPEND LINES OF content TO output.
    append_after_output( ).
    result = output.
  ENDMETHOD.
  METHOD write_open_tag.
    write_tag( line ).
    indent_level += 1.
  ENDMETHOD.
  METHOD write_closing_tag.
    indent_level -= 1.
    write_tag( line ).
  ENDMETHOD.
  METHOD add_to_stack.
    INSERT entry INTO stack INDEX 1.
  ENDMETHOD.
  METHOD last_operation.
    result = VALUE #( stack[ 1 ]-operation OPTIONAL ).
  ENDMETHOD.
  METHOD append_to_previous_line.
    DATA(index) = lines( me->content ).
    IF index > 0.
      me->content[ index ] = me->content[ index ] && string.
    ELSE.
      INSERT string INTO TABLE me->content.
    ENDIF.
  ENDMETHOD.
  METHOD append_after_output ##NEEDED.

  ENDMETHOD.
  METHOD append_before_output ##NEEDED.

  ENDMETHOD.
  METHOD call_reader_and_decode.
    DATA(reader) = NEW cl_oo_abap_doc_reader( ).
    TRY.
        DATA(result) = reader->get_abap_doc_for_element(
          clif_name    = CONV #( name_of_source )
          element_name = element_name ).

        read_abap_doc = abap_doc_parser->parse(
          EXPORTING
            component_name = element_name
            to_parse       = result
          CHANGING
            log            = log ).
      CATCH cx_root ##NO_HANDLER ##CATCH_ALL.
    ENDTRY.
  ENDMETHOD.
  METHOD remove_leading_trailing_spaces.
    SHIFT string_to_work_on RIGHT DELETING TRAILING space.
    SHIFT string_to_work_on LEFT DELETING LEADING space.
  ENDMETHOD.
  METHOD delete_first_of_struc_stack.
    IF stack_of_structure IS NOT INITIAL.
      DELETE stack_of_structure INDEX 1.
    ENDIF.
  ENDMETHOD.
  METHOD get_all_path_information.
    DATA previous_absolute_name TYPE abap_abstypename.
    DATA splitted_prev_name TYPE string_table.
    DATA(index) = 0.
    WHILE lines( splitted_prev_name ) <= 2.
      IF index >= lines( stack_of_structure ).
        RETURN.
      ENDIF.
      index = index + 1.
      previous_absolute_name = stack_of_structure[ index ]-absolute_name.
      splitted_prev_name = get_splitted_absolute_name( previous_absolute_name ).
    ENDWHILE.
    DATA(name_of_prev) = splitted_prev_name[ lines( splitted_prev_name ) ].
    source_type = splitted_prev_name[ 1 ].
    source = splitted_prev_name[ 2 ].
    fullname_of_type = name_of_prev && '-'.
    index = index - 1.
    WHILE index > 0.
      fullname_of_type = fullname_of_type  && stack_of_structure[ index ]-name && '-'.
      index = index - 1.
    ENDWHILE.
    fullname_of_type = fullname_of_type && name.
  ENDMETHOD.
  METHOD get_splitted_absolute_name.
    DATA(place_of_type) = absolute_name.
    SPLIT place_of_type AT '\' INTO TABLE DATA(splitted_in_componets).
    LOOP AT splitted_in_componets ASSIGNING FIELD-SYMBOL(<component>).
      IF <component> IS NOT INITIAL.
        SPLIT <component> AT '=' INTO TABLE DATA(splitted_in_details).
        APPEND LINES OF splitted_in_details TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_structure_of_enum_values.
    get_infos_of_values_link(
      EXPORTING
        values_link      = link_to_values
      IMPORTING
        name_of_source   = name_of_source
        name_of_constant = name_of_constant ).

    structure_of_values = get_constant_as_struc(
      name_of_source   = name_of_source
      name_of_constant = name_of_constant
      fullname_of_type = fullname_of_type ).
  ENDMETHOD.

  METHOD get_constant_as_struc.
    DATA constant TYPE REF TO cl_abap_datadescr.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = name_of_source
      RECEIVING
        p_descr_ref    = DATA(constant_descr)
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).

    IF sy-subrc <> 0.
*    class or interface doesn't exist
      MESSAGE w103(zaff_tools) WITH name_of_source INTO DATA(message) ##NEEDED.
      log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
    ELSE.
      IF constant_descr->kind = cl_abap_typedescr=>kind_intf.
        DATA(constant_descr_intf) = CAST cl_abap_intfdescr( constant_descr ).
        constant_descr_intf->get_attribute_type(
          EXPORTING
            p_name              = name_of_constant
          RECEIVING
            p_descr_ref         = constant
          EXCEPTIONS
            attribute_not_found = 1
            OTHERS              = 2 ).
        IF sy-subrc <> 0.
*      constant in interface does not exist
          MESSAGE w104(zaff_tools) WITH name_of_source && '=>' && name_of_constant INTO message.
          log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
        ENDIF.
      ELSEIF constant_descr->kind = cl_abap_typedescr=>kind_class.
        DATA(constant_descr_clas) = CAST cl_abap_classdescr( constant_descr ).
        constant_descr_clas->get_attribute_type(
          EXPORTING
            p_name              = name_of_constant
          RECEIVING
            p_descr_ref         = constant
          EXCEPTIONS
            attribute_not_found = 1
            OTHERS              = 2 ).
        IF sy-subrc <> 0.
*      constant in class does not exits
          MESSAGE w104(zaff_tools) WITH name_of_source && '=>' && name_of_constant INTO message.
          log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
        ENDIF.
      ENDIF.
      constant_as_struc = CAST cl_abap_structdescr( constant ).
    ENDIF.
  ENDMETHOD.
  METHOD get_infos_of_values_link.
    DATA(link) = values_link.
    REPLACE ALL OCCURRENCES OF REGEX `[\s]` IN link WITH `` ##REGEX_POSIX.
    REPLACE ALL OCCURRENCES OF `data:` IN link WITH ``.
    SPLIT link AT '.' INTO TABLE DATA(split_at_point).
    IF lines( split_at_point ) = 2.
      name_of_source = to_upper( split_at_point[ 1 ] ).
      name_of_constant = to_upper( split_at_point[ 2 ] ).
    ENDIF.
  ENDMETHOD.
  METHOD get_abap_doc_for_absolute_name.
    DATA(splitted_prev_name) = get_splitted_absolute_name( absolute_name ).
    IF lines( splitted_prev_name ) >= 4.
      DATA(source_type) = splitted_prev_name[ 1 ].
      DATA(source) = splitted_prev_name[ 2 ].
      DATA(fullname_of_type) = splitted_prev_name[ 4 ].
      IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
        abap_doc = call_reader_and_decode( name_of_source = source element_name   = fullname_of_type ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD compare_abap_doc.
    IF abap_doc_base-enumvalues_link IS INITIAL.
      abap_doc_base-enumvalues_link = abap_doc_additional-enumvalues_link.
    ENDIF.
    IF abap_doc_base-title IS INITIAL AND abap_doc_base-description IS INITIAL.
      abap_doc_base-title = abap_doc_additional-title.
      abap_doc_base-description = abap_doc_additional-description.
    ENDIF.
    IF abap_doc_base-minimum IS INITIAL AND abap_doc_base-maximum IS INITIAL AND abap_doc_base-exclusive_maximum IS INITIAL AND abap_doc_base-exclusive_minimum IS INITIAL.
      abap_doc_base-minimum = abap_doc_additional-minimum.
      abap_doc_base-maximum = abap_doc_additional-maximum.
      abap_doc_base-exclusive_minimum = abap_doc_additional-exclusive_minimum.
      abap_doc_base-exclusive_maximum = abap_doc_additional-exclusive_maximum.
    ENDIF.
    IF abap_doc_base-multiple_of IS INITIAL.
      abap_doc_base-multiple_of = abap_doc_additional-multiple_of.
    ENDIF.
    IF abap_doc_base-max_length IS INITIAL AND abap_doc_base-min_length IS INITIAL.
      abap_doc_base-min_length = abap_doc_additional-min_length.
      abap_doc_base-max_length = abap_doc_additional-max_length.
    ENDIF.
    IF abap_doc_base-default IS INITIAL.
      abap_doc_base-default = abap_doc_additional-default.
    ENDIF.
    IF abap_doc_base-callback_class IS INITIAL.
      abap_doc_base-callback_class = abap_doc_additional-callback_class.
    ENDIF.
  ENDMETHOD.
  METHOD get_default_from_link.
    DATA(link_to_work_on) = link.
    REPLACE ALL OCCURRENCES OF REGEX `(@link|data:)` IN link_to_work_on WITH `` ##REGEX_POSIX.
    REPLACE ALL OCCURRENCES OF REGEX `[\s]` IN link_to_work_on WITH `` ##REGEX_POSIX.
    SPLIT link_to_work_on AT '.' INTO TABLE DATA(splitted).
    IF validate_default_link( splitted_link = splitted fullname_of_type = fullname_of_type element_type = element_type ) = abap_true.
      DATA(default_abap) = splitted[ lines( splitted ) ].
      default_value = apply_formatting( default_abap ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_aff_writer~get_log.
    log = me->log.
  ENDMETHOD.

  METHOD is_callback_class_valid.
    DATA(name_of_callback_class) = to_upper( class_name ).
    cl_oo_classname_service=>get_all_method_includes(
      EXPORTING
        clsname            = CONV #( name_of_callback_class )
      RECEIVING
        result             = DATA(result)
      EXCEPTIONS
        class_not_existing = 1 ).
    IF sy-subrc = 0.
      DATA(has_method_get_subschema) = xsdbool( line_exists( result[ cpdkey = VALUE #( clsname = name_of_callback_class cpdname = 'GET_SUBSCHEMA' ) ] ) ).
      DATA(has_method_serialize) = xsdbool( line_exists( result[ cpdkey = VALUE #( clsname = name_of_callback_class cpdname = 'SERIALIZE' ) ] ) ).
      DATA(has_method_deserialize) = xsdbool( line_exists( result[ cpdkey = VALUE #( clsname = name_of_callback_class cpdname = 'DESERIALIZE' ) ] ) ).
      is_valid = xsdbool( has_method_get_subschema = abap_true AND has_method_serialize = abap_true AND has_method_deserialize = abap_true ).
    ENDIF.
    IF is_valid = abap_false.
      MESSAGE w106(zaff_tools) INTO DATA(message) ##NEEDED.
      log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
    ENDIF.
  ENDMETHOD.

  METHOD validate_default_link.
    IF lines( splitted_link ) = 3.
      DATA(source_name) = to_upper( splitted_link[ 1 ] ).
      DATA(constant_name) = to_upper( splitted_link[ 2 ] ).
      DATA(component_name) = to_upper( splitted_link[ 3 ] ).
      DATA(constant_description) = get_constant_as_struc(
        name_of_source   = source_name
        name_of_constant = constant_name
        fullname_of_type = fullname_of_type ).
      IF constant_description IS NOT INITIAL.
        DATA(components) = constant_description->get_components( ).
        DATA(row) = VALUE #( components[ name = component_name ] OPTIONAL ).
        IF row IS NOT INITIAL.
          IF row-type->type_kind = element_type.
            is_valid = abap_true.
          ELSE.
            MESSAGE w122(zaff_tools) WITH constant_name fullname_of_type INTO DATA(message) ##NEEDED.
            log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
          ENDIF.
        ELSE.
          MESSAGE w105(zaff_tools) WITH component_name constant_name INTO message.
          log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD is_default_value_valid.
    DATA(default) = default_value.
    REPLACE ALL OCCURRENCES OF `"` IN default WITH ``.
    DATA(type) = get_json_type_from_description( element_description ).
    DATA r_field TYPE REF TO data.
    FIELD-SYMBOLS <field> TYPE any.
    CREATE DATA r_field TYPE HANDLE element_description.
    ASSIGN r_field->* TO <field>.
    IF element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
*      No support for default with utclong
      MESSAGE w117(zaff_tools) WITH 'UTCLONG' INTO DATA(message) ##NEEDED.
      log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
      is_valid = abap_false.
      RETURN.
    ELSEIF type = zif_aff_writer=>type_info-boolean.
      default = to_lower( default ).
      IF default = 'abap_true' OR default = 'x' OR default = 'abap_false' OR default = ''.
        is_valid = abap_true.
      ENDIF.
    ELSEIF type = zif_aff_writer=>type_info-string OR type = zif_aff_writer=>type_info-date_time.
      DATA string TYPE string.
      TRY.
          <field> = default.
          string = <field>.
          IF element_description->type_kind = cl_abap_typedescr=>typekind_num OR element_description->type_kind = cl_abap_typedescr=>typekind_numeric.
            SHIFT string LEFT DELETING LEADING '0'.
          ENDIF.
          IF element_description->type_kind = cl_abap_typedescr=>typekind_time.
            default = default && repeat( val = '0' occ = 6 - strlen( default ) ).
          ENDIF.
          IF element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
            REPLACE REGEX `T|t` IN default WITH ` ` ##REGEX_POSIX.
          ENDIF.
          remove_leading_trailing_spaces( CHANGING string_to_work_on = string ).
          remove_leading_trailing_spaces( CHANGING string_to_work_on = default ).
          IF string = default.
            is_valid = abap_true.
          ELSE.
            is_valid = abap_false.
          ENDIF.
        CATCH cx_root.
          is_valid = abap_false.
      ENDTRY.
    ELSEIF type = zif_aff_writer=>type_info-numeric.
      TRY.
          <field> = default.
          IF <field> - default = 0.
            is_valid = abap_true.
          ELSE.
            is_valid = abap_false.
          ENDIF.
        CATCH cx_root.
          is_valid = abap_false.
      ENDTRY.
    ENDIF.
    IF is_valid = abap_false.
      MESSAGE w114(zaff_tools) INTO message.
      log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
    ENDIF.
  ENDMETHOD.
  METHOD zif_aff_writer~validate.
    result = abap_true.
  ENDMETHOD.
  METHOD zif_aff_writer~close_include.
    delete_first_of_struc_stack( ).
  ENDMETHOD.
  METHOD zif_aff_writer~open_include.
    INSERT VALUE #( absolute_name = include_description->absolute_name ) INTO me->stack_of_structure INDEX 1.
  ENDMETHOD.
  METHOD is_sy_langu.
    DATA(sy_langu_description) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( VALUE sy-langu( ) ) ).
    result = xsdbool( sy_langu_description->edit_mask = element_description->edit_mask ).
  ENDMETHOD.

  METHOD clear_type_specifics.
    CLEAR abap_doc.
    CLEAR fullname_of_type.
  ENDMETHOD.

  METHOD check_redundant_annotations.
    IF abap_doc-showalways = abap_true AND abap_doc-required = abap_true.
      MESSAGE i112(zaff_tools) INTO DATA(message) ##NEEDED.
      log->add_info( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
    ENDIF.

    IF abap_doc-required = abap_true AND abap_doc-default IS NOT INITIAL.
      MESSAGE w126(zaff_tools) INTO message.
      log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = fullname_of_type ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS zcl_aff_test_types IMPLEMENTATION.
  METHOD get_subschema.
    subschema = zcl_aff_test_types=>subschema.
  ENDMETHOD.
  METHOD serialize.
    IF ( simple_callback IS SUPPLIED ).
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
    ELSEIF ( structure_callback IS SUPPLIED ).
      writer->open_element( name   = 'str' ).
      writer->write_attribute( name = 'name' value = 'elementName' ) ##NO_TEXT.
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
      writer->close_element( ).
    ELSEIF ( table_callback IS SUPPLIED ).
      writer->open_element( name   = 'str' ).
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
      writer->close_element( ).
    ELSEIF ( element_callback IS SUPPLIED ).
      writer->write_attribute( name = 'name' value = 'elementCallback' ) ##NO_TEXT.
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
    ELSEIF ( element_structure_callback IS SUPPLIED ).
      writer->write_attribute( name = 'name' value = 'elementStructureCallback' ) ##NO_TEXT.
      writer->open_element( name = 'str' ).
      writer->write_attribute( name = 'name' value = 'elementName' ).
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
      writer->close_element( ).
    ELSEIF ( element_table_callback IS SUPPLIED ).
      writer->write_attribute( name = 'name' value = 'elementTableCallback' ) ##NO_TEXT.
      writer->open_element( name   = 'str' ).
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
      writer->close_element( ).
    ENDIF.
  ENDMETHOD.
  METHOD deserialize.
    FIELD-SYMBOLS:
        <attr>    TYPE data.

    reader->next_node( ).

    ASSIGN expected_var->* TO <attr>.
    IF ( simple_callback IS SUPPLIED ).
      simple_callback = <attr>.
    ELSEIF ( structure_callback IS SUPPLIED ).
      structure_callback = <attr>.
      jump_to_end( reader ).
    ELSEIF ( table_callback IS SUPPLIED ).
      table_callback = <attr>.
      jump_to_end( reader ).
    ELSEIF ( element_callback IS SUPPLIED ).
      element_callback = <attr>.
    ELSEIF ( element_structure_callback IS SUPPLIED ).
      element_structure_callback = <attr>.
      jump_to_end( reader ).
    ELSEIF ( element_table_callback IS SUPPLIED ).
      element_table_callback = <attr>.
      jump_to_end( reader ).
    ENDIF.
  ENDMETHOD.
  METHOD jump_to_end.
    DATA(type_start) = reader->name.
    DATA exit TYPE abap_boolean VALUE abap_false.
    IF reader->node_type = if_sxml_node=>co_nt_element_close.
      exit = abap_true.
    ENDIF.
    WHILE exit = abap_false.
      reader->next_node( ).
      IF reader->node_type = if_sxml_node=>co_nt_element_close AND reader->name = type_start.
        exit = abap_true.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.
  METHOD set_expected.
    GET REFERENCE OF expected_variable INTO expected_var.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_aff_log IMPLEMENTATION.
  METHOD zif_aff_log~get_messages.
    messages = me->messages.
  ENDMETHOD.
  METHOD zif_aff_log~add_info.
    set_max_severity( zif_aff_log=>c_message_type-info ).
    add_message( type = zif_aff_log=>c_message_type-info message = message component_name = component_name ).
  ENDMETHOD.
  METHOD zif_aff_log~add_warning.
    set_max_severity( zif_aff_log=>c_message_type-warning ).
    add_message( type = zif_aff_log=>c_message_type-warning message = message component_name = component_name ).
  ENDMETHOD.
  METHOD zif_aff_log~add_error.
    set_max_severity( zif_aff_log=>c_message_type-error ).
    add_message( type = zif_aff_log=>c_message_type-error message = message component_name = component_name ).
  ENDMETHOD.
  METHOD zif_aff_log~add_exception.
    set_max_severity( message_type ).

    IF exception->get_text( ) IS NOT INITIAL.
      cl_message_helper=>set_msg_vars_for_if_msg( exception ).
      add_message( type = message_type message = get_sy_message( ) component_name = component_name ).
    ENDIF.

    IF exception->previous IS BOUND.
      zif_aff_log~add_exception( exception = exception->previous message_type = message_type component_name = component_name ).
    ENDIF.
  ENDMETHOD.
  METHOD add_message.

    MESSAGE
      ID message-msgid
      TYPE type
      NUMBER message-msgno
      WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4
      INTO DATA(text).

    APPEND VALUE #( component_name = component_name
                    type         = type
                    text         = text
                    message      = get_sy_message( ) ) TO me->messages.
  ENDMETHOD.
  METHOD zif_aff_log~join.
    APPEND LINES OF log_to_join->get_messages( ) TO me->messages.
    set_max_severity( log_to_join->get_max_severity( ) ).
  ENDMETHOD.
  METHOD zif_aff_log~clear.
    CLEAR me->messages.
  ENDMETHOD.
  METHOD zif_aff_log~get_max_severity.
    max_severity = me->max_severity.
  ENDMETHOD.
  METHOD zif_aff_log~has_messages.
    has_messages = xsdbool( me->messages IS NOT INITIAL ).
  ENDMETHOD.
  METHOD get_sy_message.
    result = VALUE #(
      msgid = sy-msgid
      msgno = sy-msgno
      msgty = sy-msgty
      msgv1 = sy-msgv1
      msgv2 = sy-msgv2
      msgv3 = sy-msgv3
      msgv4 = sy-msgv4 ).
  ENDMETHOD.
  METHOD set_max_severity.
    IF type = zif_aff_log=>c_message_type-error.
      max_severity = zif_aff_log=>c_message_type-error.
    ELSEIF type = zif_aff_log=>c_message_type-warning.
      IF max_severity <> zif_aff_log=>c_message_type-error.
        max_severity = zif_aff_log=>c_message_type-warning.
      ENDIF.
    ELSEIF type = zif_aff_log=>c_message_type-info.
      IF max_severity IS INITIAL.
        max_severity = zif_aff_log=>c_message_type-info.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS zcl_aff_generator IMPLEMENTATION.

  METHOD constructor.
    me->writer = writer.
    log = NEW zcl_aff_log( ).
  ENDMETHOD.

  METHOD generate_type.
    DATA(type_description) = cl_abap_typedescr=>describe_by_data( data ).
    check_input( type_description ).
    process_type_description( type_description ).
    result = writer->get_output( ).
    log->join( log_to_join = writer->get_log( ) ).
  ENDMETHOD.

  METHOD check_input.
    TRY.
        DATA(structure_description) = CAST cl_abap_structdescr( type_description ).
        check_mandatory_fields( structure_description ).
      CATCH cx_sy_move_cast_error.
        MESSAGE w123(zaff_tools) INTO DATA(message) ##NEEDED.
        log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = type_description->get_relative_name( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD check_mandatory_fields.
    DATA(components) = structure_description->get_components( ).
    IF NOT ( line_exists( components[ name = 'HEADER' ] ) AND line_exists( components[ name = 'FORMAT_VERSION' ] ) ).
      MESSAGE w124(zaff_tools) INTO DATA(message)  ##NEEDED.
      log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = structure_description->get_relative_name( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD process_type_description.
    CASE type_description->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        process_element(
          element_name        = type_name
          element_description = CAST #( type_description ) ).
      WHEN cl_abap_typedescr=>kind_struct.
        process_structure(
          structure_name        = type_name
          structure_description = CAST #( type_description ) ).
      WHEN cl_abap_typedescr=>kind_table.
        process_table(
          table_name        = type_name
          table_description = CAST #( type_description ) ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e100(zaff_tools) WITH type_description->kind.
    ENDCASE.
  ENDMETHOD.

  METHOD process_element.
    DATA(name) = COND #( WHEN element_name IS NOT INITIAL THEN element_name
                         ELSE element_description->get_relative_name( ) ).
    writer->write_element(
      element_name        = name
      element_description = element_description ).
  ENDMETHOD.

  METHOD process_structure.
    DATA(name) = COND #( WHEN structure_name IS NOT INITIAL THEN structure_name
                         ELSE structure_description->get_relative_name( ) ).
    writer->open_node(
      node_name        = name
      node_description = structure_description ).
    DATA(components) = structure_description->get_components( ).
    process_components( components ).
    writer->close_node(
      node_name        = name
      node_description = structure_description ).
  ENDMETHOD.

  METHOD process_include.
    DATA(components) = structure_description->get_components( ).
    writer->open_include( structure_description ).
    process_components( components ).
    writer->close_include( ).
  ENDMETHOD.

  METHOD process_components.
    LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
      IF <component>-as_include = abap_true.
        process_include( CAST #( <component>-type ) ).
      ELSE.
        process_type_description(
          type_name        = <component>-name
          type_description = <component>-type ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD process_table.
    DATA(name) = COND #( WHEN table_name IS NOT INITIAL THEN table_name
                         ELSE table_description->get_relative_name( ) ).
    writer->open_node(
      node_name        = name
      node_description = table_description ).
    DATA(line_description) = table_description->get_table_line_type( ).
    process_type_description( line_description ).
    writer->close_node(
      node_name        = name
      node_description = table_description ).
  ENDMETHOD.
  METHOD get_log.
    log = me->log.
  ENDMETHOD.

ENDCLASS.

CLASS kHGwlLgOAHwRhKswYSCOzlpRPLPSjq DEFINITION DEFERRED.
* renamed: zcl_aff_abap_doc_reader :: lcl_section_source_comments
CLASS kHGwlLgOAHwRhKswYSCOzlpRPLPSjq DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_stokesx TYPE STANDARD TABLE OF stokesx .
    TYPES:
      ty_sstmnt TYPE TABLE OF sstmnt .
    TYPES:
      BEGIN OF ty_comment_block,
        tab_comments                 TYPE string_table,
        column_first_comment         TYPE i,
        hook_relevant_tok_type       TYPE stokesx,
        hook_relevant_tok_name       TYPE stokesx,
        hook_relevant_tok_name_add   TYPE stokesx,
        hook_relevant_tok_type_stmnt TYPE stokesx,
        hook_relevant_tok_name_stmnt TYPE stokesx,
      END OF ty_comment_block .
    TYPES:
      ty_comment_blocks TYPE STANDARD TABLE OF ty_comment_block WITH EMPTY KEY.

    METHODS scan_code
      IMPORTING
        !source_to_be_scanned TYPE string_table
      EXPORTING
        !tab_statements       TYPE ty_sstmnt
        !tab_tokens           TYPE ty_stokesx .
    METHODS identify_abap_doc_blocks_all
      IMPORTING
        !tab_statements     TYPE ty_sstmnt
        !tab_tokens         TYPE ty_stokesx
        !tab_source         TYPE string_table
      RETURNING
        VALUE(tab_abap_doc) TYPE ty_comment_blocks .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA clsname TYPE string.

    TYPES: BEGIN OF ty_node,
             depth          TYPE i,
             node_name      TYPE string,
             parent_node    TYPE string,
             stmnt_from_idx TYPE i,
             stmnt_to_idx   TYPE i,
           END OF ty_node.
    TYPES ty_nodes TYPE STANDARD TABLE OF ty_node.

    DATA depth TYPE i.
    DATA hierarchy_nodes TYPE ty_nodes.

    METHODS is_within_data_begin_end_of
      IMPORTING
        tab_statements TYPE ty_sstmnt
        tab_tokens     TYPE ty_stokesx
        limit          TYPE i
        limit_col      TYPE int2 OPTIONAL
      RETURNING
        VALUE(result)  TYPE abap_bool .
    METHODS is_within_types_begin_end_of
      IMPORTING
        tab_statements TYPE ty_sstmnt
        tab_tokens     TYPE ty_stokesx
        limit          TYPE i
        limit_col      TYPE int2 OPTIONAL
      RETURNING
        VALUE(result)  TYPE abap_bool .

    METHODS build_hierarchy_nodes
      IMPORTING
        tab_statements TYPE ty_sstmnt
        tab_tokens     TYPE ty_stokesx
      CHANGING
        nodes          TYPE ty_nodes.
ENDCLASS.

CLASS kHGwlLgOAHwRhKswYSCOzlpRPLPSjq IMPLEMENTATION.

  METHOD is_within_data_begin_end_of.
    FIELD-SYMBOLS: <fs_tok_prev_prev>  TYPE stokesx.
    FIELD-SYMBOLS: <fs_tok_prev_prev2> TYPE stokesx.
    FIELD-SYMBOLS: <fs_tok_prev_prev1> TYPE stokesx.
    FIELD-SYMBOLS: <fs_stmnt_tmp>      TYPE sstmnt.

    DATA counter_begin_of TYPE i.
    DATA counter_end_of TYPE i.

    result = abap_false.
    CLEAR me->depth.

    IF limit_col > 0.
      LOOP AT tab_statements ASSIGNING <fs_stmnt_tmp> WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ) AND from <= limit.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev>   INDEX <fs_stmnt_tmp>-from.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev1>  INDEX <fs_stmnt_tmp>-from + 1.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev2>  INDEX <fs_stmnt_tmp>-from + 2.
        IF ( ( <fs_tok_prev_prev>-str = 'DATA' OR <fs_tok_prev_prev>-str = 'CLASS-DATA' OR
                <fs_tok_prev_prev>-str = 'CONSTANTS' ) AND <fs_tok_prev_prev1>-str = 'BEGIN' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_begin_of = counter_begin_of + 1.
          me->depth = me->depth + 1.

        ELSEIF ( ( <fs_tok_prev_prev>-str = 'DATA' OR <fs_tok_prev_prev>-str = 'CLASS-DATA' OR
                   <fs_tok_prev_prev>-str = 'CONSTANTS' ) AND <fs_tok_prev_prev1>-str = 'END' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_end_of = counter_end_of + 1.
          me->depth = me->depth - 1.
        ENDIF.

      ENDLOOP.
    ELSE.
      LOOP AT tab_statements ASSIGNING <fs_stmnt_tmp> WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ) AND from < limit.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev>   INDEX <fs_stmnt_tmp>-from.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev1>  INDEX <fs_stmnt_tmp>-from + 1.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev2>  INDEX <fs_stmnt_tmp>-from + 2.
        IF ( ( <fs_tok_prev_prev>-str = 'DATA' OR <fs_tok_prev_prev>-str = 'CLASS-DATA' OR
                <fs_tok_prev_prev>-str = 'CONSTANTS' ) AND <fs_tok_prev_prev1>-str = 'BEGIN' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_begin_of = counter_begin_of + 1.
          me->depth = me->depth + 1.

        ELSEIF ( ( <fs_tok_prev_prev>-str = 'DATA' OR <fs_tok_prev_prev>-str = 'CLASS-DATA' OR
                   <fs_tok_prev_prev>-str = 'CONSTANTS' ) AND <fs_tok_prev_prev1>-str = 'END' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_end_of = counter_end_of + 1.
          me->depth = me->depth - 1.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF counter_begin_of > counter_end_of.
      result = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD is_within_types_begin_end_of.
    FIELD-SYMBOLS: <fs_tok_prev_prev>  TYPE stokesx.
    FIELD-SYMBOLS: <fs_tok_prev_prev2> TYPE stokesx.
    FIELD-SYMBOLS: <fs_tok_prev_prev1> TYPE stokesx.
    FIELD-SYMBOLS: <fs_stmnt_tmp>      TYPE sstmnt.

    DATA counter_begin_of TYPE i.
    DATA counter_end_of TYPE i.

    result = abap_false.
    CLEAR me->depth.

    IF limit_col > 0.  " end row comment
      LOOP AT tab_statements ASSIGNING <fs_stmnt_tmp> WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ) AND from <= limit.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev>   INDEX <fs_stmnt_tmp>-from.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev1>  INDEX <fs_stmnt_tmp>-from + 1.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev2>  INDEX <fs_stmnt_tmp>-from + 2.
        IF ( <fs_tok_prev_prev>-str = 'TYPES' AND <fs_tok_prev_prev1>-str = 'BEGIN' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_begin_of = counter_begin_of + 1.
          me->depth = me->depth + 1.

        ELSEIF ( <fs_tok_prev_prev>-str = 'TYPES' AND <fs_tok_prev_prev1>-str = 'END' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_end_of = counter_end_of + 1.
          me->depth = me->depth - 1.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT tab_statements ASSIGNING <fs_stmnt_tmp> WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ) AND from < limit.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev>   INDEX <fs_stmnt_tmp>-from.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev1>  INDEX <fs_stmnt_tmp>-from + 1.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev2>  INDEX <fs_stmnt_tmp>-from + 2.
        IF ( <fs_tok_prev_prev>-str = 'TYPES' AND <fs_tok_prev_prev1>-str = 'BEGIN' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_begin_of = counter_begin_of + 1.
          me->depth = me->depth + 1.

        ELSEIF ( <fs_tok_prev_prev>-str = 'TYPES' AND <fs_tok_prev_prev1>-str = 'END' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_end_of = counter_end_of + 1.
          me->depth = me->depth - 1.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF counter_begin_of > counter_end_of.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD scan_code.
    SCAN ABAP-SOURCE source_to_be_scanned TOKENS INTO tab_tokens
                                          STATEMENTS INTO tab_statements
                                          WITH ANALYSIS
                                          WITH COMMENTS
                                          WITH PRAGMAS '*'.
  ENDMETHOD.

  METHOD identify_abap_doc_blocks_all.
    DATA l_node               TYPE ty_node.
    DATA l_node_new           TYPE ty_node.
    DATA l_name_node          TYPE string.
    DATA l_name_concatenated  TYPE string.
    DATA l_parent             TYPE string.
    DATA l_line_code          LIKE LINE OF tab_source.
    DATA line_comment_block   TYPE ty_comment_block.
    DATA tab_comments_to_save TYPE string_table.

    DATA current_statement TYPE i.
    DATA relevant_token1   TYPE stokesx.
    DATA relevant_token2   TYPE stokesx.
    DATA relevant_token3   TYPE stokesx.
    DATA l_depth           TYPE i.
    DATA l_length          TYPE i.
    DATA l_count           TYPE i.
    DATA l_from            TYPE i.
    DATA l_to              TYPE i.
*
    DATA embeded_types              TYPE abap_bool VALUE abap_false.
    DATA embeded_data_const         TYPE abap_bool VALUE abap_false.
    DATA nodes                      TYPE string_table.
    DATA hierarchy_nodes_descending TYPE ty_nodes.

    FIELD-SYMBOLS <fs_stmnt>           TYPE sstmnt.
    FIELD-SYMBOLS <fs_stmnt_prev>      TYPE sstmnt.
    FIELD-SYMBOLS <fs_stmnt_next>      TYPE sstmnt.
    FIELD-SYMBOLS <fs_tok>             TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_prev>        TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_prev_plus_1> TYPE stokesx.

    CLEAR tab_abap_doc.

    me->build_hierarchy_nodes(
      EXPORTING
        tab_statements = tab_statements
        tab_tokens     = tab_tokens
      CHANGING
        nodes          = me->hierarchy_nodes ).

    LOOP AT tab_statements ASSIGNING <fs_stmnt> WHERE type = 'P' OR type = 'S'.
      CLEAR: tab_comments_to_save, relevant_token1, relevant_token2, relevant_token3, line_comment_block, l_name_concatenated.

      current_statement  = sy-tabix.

*     get the comment block in sections
      LOOP AT tab_tokens ASSIGNING <fs_tok> FROM <fs_stmnt>-from TO <fs_stmnt>-to.
        READ TABLE tab_source INTO l_line_code INDEX <fs_tok>-row.
        IF <fs_tok>-col > 0.
          l_length = <fs_tok>-col - 1.
          IF l_length > 0.
            CHECK ( l_line_code(l_length) CO space ).
          ENDIF.
        ENDIF.

        DATA(l_line_code_condensed) = l_line_code.
        CONDENSE l_line_code_condensed.
        IF l_line_code_condensed(2) = '"!'.           " filter only ABAP Doc comments
          APPEND l_line_code TO tab_comments_to_save.
        ENDIF.
      ENDLOOP.

      CHECK tab_comments_to_save IS NOT INITIAL.

      embeded_types      = abap_false.
      embeded_data_const = abap_false.

*     consider data and types typtype 4
      LOOP AT tab_statements ASSIGNING <fs_stmnt_prev> TO current_statement WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ).
      ENDLOOP.

      " is_within_.._begin_of sets the required me->depth value
      IF <fs_stmnt_prev> IS ASSIGNED.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev>  INDEX <fs_stmnt_prev>-from.  " fist token in statement
        IF <fs_tok_prev>-str = 'INCLUDE'.
          READ TABLE tab_tokens ASSIGNING <fs_tok_prev_plus_1> INDEX <fs_stmnt_prev>-from + 1.  " second token in statement
        ENDIF.

        IF ( <fs_tok_prev>-str = 'ENDCLASS' ).
          EXIT.  " get ABAP Docs only in definition part
        ENDIF.

        IF <fs_tok_prev>-str = 'TYPES' OR ( <fs_tok_prev>-str = 'INCLUDE' AND
                                             <fs_tok_prev_plus_1> IS ASSIGNED AND <fs_tok_prev_plus_1>-str = 'TYPE' ).
          IF me->is_within_types_begin_end_of(
           tab_statements = tab_statements
           tab_tokens     = tab_tokens
           limit          = <fs_stmnt>-from )  = abap_true.
            embeded_types = abap_true.
          ENDIF.
        ENDIF.

        IF ( <fs_tok_prev>-str = 'DATA' OR <fs_tok_prev>-str = 'CLASS-DATA' OR <fs_tok_prev>-str = 'CONSTANTS' ).
          IF me->is_within_data_begin_end_of(
             tab_statements = tab_statements
             tab_tokens     = tab_tokens
             limit          = <fs_stmnt>-from )  = abap_true.
            embeded_data_const = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.

*     identify the next statement in section and consider the first 2 tokens as hook
*     where the comment has to be attached in the new section
      LOOP AT tab_statements ASSIGNING <fs_stmnt_next> FROM current_statement + 1  WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ).
        EXIT.
      ENDLOOP.

      IF <fs_stmnt_next> IS ASSIGNED.
        LOOP AT tab_tokens ASSIGNING <fs_tok> FROM <fs_stmnt_next>-from WHERE type <> 'C'.
          IF relevant_token2 IS INITIAL AND relevant_token1 IS NOT INITIAL.
            relevant_token2 = <fs_tok>.
            IF relevant_token1-str = 'CLASS'.
              READ TABLE tab_tokens INTO relevant_token3 INDEX sy-tabix + 1.
              IF relevant_token3-str = 'DEFINITION' AND relevant_token2-str <> clsname.
                relevant_token2-str = clsname. " it happens at COPY: you have old clsname there
              ENDIF.
              CLEAR relevant_token3.
            ENDIF.

            IF ( relevant_token1-str = 'TYPES' AND relevant_token2-str = 'BEGIN' ) OR
               ( relevant_token1-str = 'DATA' AND relevant_token2-str = 'BEGIN' ) OR
               ( relevant_token1-str = 'CLASS-DATA' AND relevant_token2-str = 'BEGIN' ) OR
               ( relevant_token1-str = 'CONSTANTS' AND relevant_token2-str = 'BEGIN' ).
*             we need the third token in this special case ...
              READ TABLE tab_tokens INTO relevant_token3 INDEX sy-tabix + 2.
*             Check whether this is a TYPES BEGIN OF MESH my_mesh statement.
              IF (  relevant_token1-str = 'TYPES' AND relevant_token3-str = 'MESH' AND <fs_stmnt_next>-to - <fs_stmnt_next>-from = 4 ).
                READ TABLE tab_tokens INTO relevant_token3 INDEX sy-tabix + 1.
              ELSEIF (  relevant_token1-str = 'TYPES' AND relevant_token3-str = 'ENUM' AND <fs_stmnt_next>-to - <fs_stmnt_next>-from > 5 ).
                READ TABLE tab_tokens INTO relevant_token3 INDEX sy-tabix + 1.
              ENDIF.
            ENDIF.
            EXIT.
          ENDIF.
          IF relevant_token1 IS INITIAL.
            relevant_token1 = <fs_tok>.
          ENDIF.
        ENDLOOP.

        IF relevant_token3-str IS NOT INITIAL.
          l_name_node = relevant_token3-str.
        ELSE.
          l_name_node = relevant_token2-str.
        ENDIF.
        IF relevant_token1-str = 'TYPES' OR relevant_token1-str = 'DATA' OR relevant_token1-str = 'CLASS-DATA' OR relevant_token1-str = 'CONSTANTS'.
          APPEND l_name_node TO nodes.
          CLEAR: l_count, l_from, l_to.

          " get the relevant node-interval hierarchy interval
          LOOP AT me->hierarchy_nodes INTO DATA(l_hier).
            CHECK l_hier-depth = 0 AND l_hier-stmnt_from_idx > 0 AND l_hier-stmnt_from_idx <= <fs_stmnt_next>-from.
            l_from = sy-tabix.
          ENDLOOP.

          IF l_from > 0.
            LOOP AT me->hierarchy_nodes FROM l_from + 1 INTO l_hier WHERE depth = 0.
              l_to = sy-tabix.
              EXIT.
            ENDLOOP.
            IF sy-subrc <> 0 OR l_to = 0.
              l_to = lines( me->hierarchy_nodes ).
            ELSEIF l_to > l_from.
              l_to = l_to - 1.
            ENDIF.
            " operate only within relavent node_interval
            LOOP AT me->hierarchy_nodes FROM l_from TO l_to INTO l_node WHERE node_name = l_name_node AND depth = me->depth.
              l_depth = l_node-depth.
              l_parent = l_node-parent_node.
              WHILE l_depth <> 0.

                CLEAR hierarchy_nodes_descending.
                LOOP AT me->hierarchy_nodes INTO l_node_new FROM l_from TO l_to
                                                            WHERE node_name = l_parent AND depth < l_depth.
                  APPEND l_node_new TO hierarchy_nodes_descending.
                ENDLOOP.
                " eed to insert nodes from bottom up
                SORT hierarchy_nodes_descending BY depth DESCENDING. "#EC CI_SORTLOOP
                LOOP AT hierarchy_nodes_descending INTO l_node_new WHERE node_name = l_parent AND depth < l_depth.
                  INSERT l_node_new-node_name INTO nodes INDEX 1.
                ENDLOOP.
                IF sy-subrc <> 0.
                  EXIT.  " while
                ENDIF.
                l_depth  = l_node_new-depth.
                l_parent = l_node_new-parent_node.
              ENDWHILE.
            ENDLOOP.
          ENDIF.
        ENDIF.

        IF lines( nodes ) <= 1.
          CLEAR l_name_concatenated.
        ELSE.
          LOOP AT nodes INTO l_name_node.
            IF l_name_concatenated IS INITIAL.
              l_name_concatenated = l_name_node.
            ELSE.
              l_name_concatenated = l_name_concatenated && '-' && l_name_node.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      line_comment_block-tab_comments               = tab_comments_to_save.
      line_comment_block-hook_relevant_tok_type     = relevant_token1.
      line_comment_block-hook_relevant_tok_name     = relevant_token2.
      line_comment_block-hook_relevant_tok_name_add = relevant_token3.

      IF l_name_concatenated CS '-' AND ( embeded_types = abap_true OR embeded_data_const = abap_true ).
        IF line_comment_block-hook_relevant_tok_name_add IS NOT INITIAL.
          line_comment_block-hook_relevant_tok_name_add-str = l_name_concatenated.
        ELSEIF line_comment_block-hook_relevant_tok_name IS NOT INITIAL.
          line_comment_block-hook_relevant_tok_name-str = l_name_concatenated.
        ENDIF.
      ENDIF.

      APPEND line_comment_block TO tab_abap_doc.
      CLEAR: nodes, l_name_concatenated.

    ENDLOOP.

  ENDMETHOD.
  METHOD build_hierarchy_nodes.

    DATA l_count_begin TYPE i.
    DATA l_count_end   TYPE i.
    DATA l_node_root   TYPE string.
    DATA l_node        TYPE ty_node.
    DATA l_depth       TYPE i.
    DATA parents       TYPE string_table.

    FIELD-SYMBOLS <fs_stmnt>     TYPE sstmnt.
    FIELD-SYMBOLS <fs_tok>       TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_next2> TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_next1> TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_next3> TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_next4> TYPE stokesx.

    CLEAR nodes.

    LOOP AT tab_statements ASSIGNING <fs_stmnt>  WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ). " no comments

      " check first token in statement
      IF NOT ( tab_tokens[ <fs_stmnt>-from ]-str = 'TYPES' OR tab_tokens[ <fs_stmnt>-from ]-str = 'DATA' OR
               tab_tokens[ <fs_stmnt>-from ]-str = 'CLASS-DATA' OR tab_tokens[ <fs_stmnt>-from ]-str = 'CONSTANTS' ).
        CONTINUE.
      ENDIF.

      LOOP AT tab_tokens ASSIGNING <fs_tok> FROM <fs_stmnt>-from TO <fs_stmnt>-to.
        CHECK ( <fs_tok>-str = 'TYPES' OR <fs_tok>-str = 'DATA' OR
                <fs_tok>-str = 'CLASS-DATA' OR <fs_tok>-str = 'CONSTANTS' ).

        READ TABLE tab_tokens ASSIGNING <fs_tok_next1>  INDEX <fs_stmnt>-from  + 1.
        READ TABLE tab_tokens ASSIGNING <fs_tok_next2>  INDEX <fs_stmnt>-from  + 2.
        READ TABLE tab_tokens ASSIGNING <fs_tok_next3>  INDEX <fs_stmnt>-from  + 3.

        IF <fs_tok_next1> IS ASSIGNED AND <fs_tok_next2> IS ASSIGNED AND <fs_tok_next3> IS ASSIGNED.
          IF ( <fs_tok_next1>-str = 'BEGIN' AND <fs_tok_next2>-str = 'OF' ).

            l_node-node_name      = <fs_tok_next3>-str.
            l_node-stmnt_from_idx = <fs_stmnt>-from.
            l_node-stmnt_to_idx   = <fs_stmnt>-to.

            IF <fs_tok>-str = 'TYPES' AND ( l_node-node_name = 'MESH' OR l_node-node_name = 'ENUM' ).
              READ TABLE tab_tokens ASSIGNING <fs_tok_next4>  INDEX <fs_stmnt>-from  + 4.
              l_node-node_name = <fs_tok_next4>-str.
            ENDIF.

            IF l_depth = 0.
              l_node_root         = <fs_tok_next3>-str.
              l_node-depth        = 0.
              APPEND l_node TO nodes.
              l_node-parent_node = l_node_root.
            ELSE.
              l_node-depth = l_depth.
              APPEND l_node TO nodes.
            ENDIF.

            APPEND l_node-node_name TO parents.
            l_node-parent_node = l_node-node_name.  " for the child nodes

            l_depth       = l_depth + 1.
            l_count_begin = l_count_begin + 1.
          ELSEIF ( <fs_tok_next1>-str = 'END' AND <fs_tok_next2>-str = 'OF' ).
            l_depth = l_depth - 1.
            l_count_end = l_count_end + 1.

            IF l_depth = 0.
              CLEAR: l_count_begin, l_count_end, l_node, l_node_root, parents.
              CONTINUE.
            ENDIF.

            DATA(l_lines) = lines( parents ).
            DELETE parents INDEX l_lines.
            l_node-parent_node = parents[ l_depth ].

          ELSE.  " TYPES /DATA/CLASS-DATA...     embeded in BEGIN or stand-alone
            l_node-stmnt_from_idx = <fs_stmnt>-from.
            l_node-stmnt_to_idx   = <fs_stmnt>-to.
            l_node-node_name = <fs_tok_next1>-str.
            l_node-depth     = l_depth.
            IF NOT line_exists( nodes[ table_line = l_node ] ).
              APPEND l_node TO nodes.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS zcl_aff_abap_doc_reader IMPLEMENTATION.
  METHOD create_instance.
    result = NEW #( ).
    result->source = source.
  ENDMETHOD.
  METHOD get_abap_doc_for_element.
    DATA: l_element_name      TYPE string,
          l_scanned_elem_name TYPE string.
    DATA section_source       TYPE string_table.
    DATA scan_abap_doc_blocks TYPE STANDARD TABLE OF kHGwlLgOAHwRhKswYSCOzlpRPLPSjq=>ty_comment_block.
    DATA element_was_found    TYPE abap_bool.

    CLEAR: result, element_was_found.

    l_element_name  = element_name.

    TRANSLATE l_element_name  TO UPPER CASE.
    CONDENSE l_element_name.
    DATA(scan_util) = NEW kHGwlLgOAHwRhKswYSCOzlpRPLPSjq( ).

    section_source[] = me->source[].

    scan_util->scan_code( EXPORTING source_to_be_scanned = section_source
                          IMPORTING tab_statements       = DATA(scan_statements)
                                    tab_tokens           = DATA(scan_tokens) ).

    scan_abap_doc_blocks = scan_util->identify_abap_doc_blocks_all(
      tab_statements = scan_statements
      tab_tokens     = scan_tokens
      tab_source     = section_source ).

    LOOP AT scan_abap_doc_blocks ASSIGNING FIELD-SYMBOL(<fs_abap_doc_block>).

      IF <fs_abap_doc_block>-hook_relevant_tok_name-str = 'BEGIN'.
        l_scanned_elem_name = <fs_abap_doc_block>-hook_relevant_tok_name_add-str.
      ELSE.
        l_scanned_elem_name = <fs_abap_doc_block>-hook_relevant_tok_name-str.
      ENDIF.

      IF l_scanned_elem_name = l_element_name.

        " prepare the result for required element
        LOOP AT <fs_abap_doc_block>-tab_comments ASSIGNING FIELD-SYMBOL(<adoc_line>).
          CONDENSE <adoc_line>.         " remove leading spaces
          <adoc_line> = <adoc_line>+2.  " remove "!
          CONDENSE <adoc_line>.         " remove again leading spaces
          IF sy-tabix = 1.
            result = <adoc_line>.
          ELSE.
            CONCATENATE result <adoc_line> INTO result SEPARATED BY space.
          ENDIF.
        ENDLOOP.
        element_was_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF element_was_found = abap_false.
      RAISE EXCEPTION NEW zcx_aff_tools( message = l_element_name ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_aff_abap_doc_parser IMPLEMENTATION.
  METHOD parse.
    CLEAR description_warning_is_needed.
    CLEAR decoded_abap_doc.
    abap_doc_string = to_parse.
    me->component_name = component_name.
    parser_log = log.
    parse_title( ).
    parse_annotations( ).
    parse_description( ).
    found_abap_doc = decoded_abap_doc.
    write_description_message( ).
  ENDMETHOD.
  METHOD parse_title.
    REPLACE ALL OCCURRENCES OF REGEX `[\s]*(<p[\s]+class="shorttext([\s]+synchronized)?"([\s]+lang="[a-zA-Z]{2}")?[\s]*>)[\s]*`
        IN abap_doc_string WITH `<p class="shorttext">` ##NO_TEXT ##REGEX_POSIX.
    decoded_abap_doc-title = substring_after( val = abap_doc_string regex = co_shorttext_tag_open ) ##REGEX_POSIX.
    IF ( decoded_abap_doc-title IS NOT INITIAL ).
      decoded_abap_doc-title = substring_before( val = decoded_abap_doc-title sub = '</p>' ).
      remove_leading_trailing_spaces( CHANGING string_to_work_on = decoded_abap_doc-title ).
    ENDIF.
    check_title_positions( ).
    workaround_remove_titles( ).
  ENDMETHOD.
  METHOD check_title_positions.
    IF ( count( val = abap_doc_string regex = co_shorttext_tag_open ) > 1 ) ##REGEX_POSIX.
      MESSAGE i107(zaff_tools) WITH `'Title'` INTO DATA(message)  ##NEEDED.
      parser_log->add_info( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
    ENDIF.
    IF ( find( val = abap_doc_string regex = co_shorttext_tag_open ) > 0 ) ##REGEX_POSIX.
      MESSAGE i113(zaff_tools) INTO message.
      parser_log->add_info( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
    ENDIF.
  ENDMETHOD.
  METHOD workaround_remove_titles.
    WHILE ( matches( val = abap_doc_string regex = `.*[\s]*<p\sclass="shorttext">.*` ) ) ##REGEX_POSIX.
      DATA(start_offset) = find( val = abap_doc_string regex = co_shorttext_tag_open occ = 1 ) ##REGEX_POSIX.
      abap_doc_string = abap_doc_string(start_offset) && substring_after( val = abap_doc_string+start_offset sub = `</p>` ).
    ENDWHILE.
  ENDMETHOD.

  METHOD parse_description.
    FIND FIRST OCCURRENCE OF REGEX `(\$callbackClass|\$default|\$values|\$required|\$showAlways|\$minimum|\$maximum|\$exclusiveMinimum|\$exclusiveMaximum|\$multipleOf|\$maxLength|\$minLength|\$enumValue)`
      IN abap_doc_string MATCH OFFSET DATA(offset) ##REGEX_POSIX.
    IF sy-subrc = 0.
      DATA(description) = abap_doc_string+0(offset).
      remove_leading_trailing_spaces( CHANGING string_to_work_on = description ).
      decoded_abap_doc-description = description.
    ELSE.
      remove_leading_trailing_spaces( CHANGING string_to_work_on = abap_doc_string ).
      decoded_abap_doc-description = abap_doc_string.
    ENDIF.
  ENDMETHOD.
  METHOD parse_annotations.
    FIND ALL OCCURRENCES OF REGEX `\$[a-zA-Z]+` IN abap_doc_string RESULTS DATA(result_table) ##REGEX_POSIX.
    DATA(modified_abap_doc_string) = abap_doc_string.
    LOOP AT result_table ASSIGNING FIELD-SYMBOL(<entry>).
      DATA(offset) = <entry>-offset.
      DATA(length) = <entry>-length.
      DATA(key_word) = abap_doc_string+offset(length).
      CASE key_word.
        WHEN abap_doc_annotation-callback_class.
          parse_callback_class( ).
        WHEN abap_doc_annotation-default.
          parse_default( ).
        WHEN abap_doc_annotation-values.
          parse_enum_values( ).
        WHEN abap_doc_annotation-required.
          parse_required( ).
        WHEN abap_doc_annotation-show_always.
          parse_show_always( ).
        WHEN abap_doc_annotation-minimum OR abap_doc_annotation-maximum OR abap_doc_annotation-exclusive_minimum OR abap_doc_annotation-exclusive_maximum
             OR abap_doc_annotation-max_length OR abap_doc_annotation-multiple_of OR abap_doc_annotation-min_length.
          parse_number_annotations( key_word = key_word ).
        WHEN abap_doc_annotation-enum_value.
          parse_enum_value( ).
        WHEN OTHERS.
          REPLACE key_word IN modified_abap_doc_string WITH ''.
          MESSAGE w108(zaff_tools) WITH key_word INTO DATA(message) ##NEEDED.
          parser_log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
      ENDCASE.
    ENDLOOP.
    abap_doc_string = modified_abap_doc_string.
  ENDMETHOD.

  METHOD parse_callback_class.
    IF decoded_abap_doc-callback_class IS NOT INITIAL.
      RETURN.
    ENDIF.
    DATA(string_to_parse) = abap_doc_string.
    REPLACE ALL OCCURRENCES OF REGEX `\$callbackClass[\s]*(:[\s]*)?\{[\s]*@link` IN string_to_parse WITH `\$callbackClass\{@link` ##REGEX_POSIX.
    FIND ALL OCCURRENCES OF REGEX `\$callbackClass\{@link[^\}]+\}` IN string_to_parse RESULTS DATA(result_table) ##REGEX_POSIX.
    IF lines( result_table ) = 0.
      MESSAGE w109(zaff_tools) WITH abap_doc_annotation-callback_class INTO DATA(message) ##NEEDED.
      parser_log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
      RETURN.
    ENDIF.
    write_log_for_multiple_entries( result_table = result_table annotaion = abap_doc_annotation-callback_class ).
    DATA(offset_found) = result_table[ 1 ]-offset.
    DATA(length_found) = result_table[ 1 ]-length.
    decoded_abap_doc-callback_class = get_annotation_value( length = length_found - 1 offset = offset_found to_decode = string_to_parse length_of_annotation = 20 remove_whitespaces = abap_true ).
    LOOP AT result_table ASSIGNING FIELD-SYMBOL(<entry>).
      check_next_word( offset = <entry>-offset + <entry>-length text_to_check = string_to_parse ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_annotation_value.
    DATA(step) = offset + length_of_annotation.
    DATA(length_of_annotation_value) = length - length_of_annotation.
    DATA(value) = to_decode+step(length_of_annotation_value).
    IF remove_whitespaces = abap_true.
      remove_leading_trailing_spaces( CHANGING string_to_work_on = value ).
    ENDIF.
    annotation_value = value.
  ENDMETHOD.
  METHOD parse_default.
    IF decoded_abap_doc-default IS NOT INITIAL.
      RETURN.
    ENDIF.
    DATA(string_to_parse) = abap_doc_string.
    REPLACE ALL OCCURRENCES OF REGEX `\$default[\s]*(:[\s]*)?'` IN string_to_parse WITH `\$default'` ##REGEX_POSIX.
    REPLACE ALL OCCURRENCES OF REGEX `\$default[\s]*(:[\s]*)?\{[\s]*@link` IN string_to_parse WITH `\$default\{@link` ##REGEX_POSIX.

    FIND ALL OCCURRENCES OF REGEX `\$default'[^']*'` IN string_to_parse RESULTS DATA(result_table_value) ##REGEX_POSIX.
    FIND ALL OCCURRENCES OF REGEX `\$default\{@link[^\}]+\}` IN string_to_parse RESULTS DATA(result_table_link) ##REGEX_POSIX.

    DATA mixed_result_table TYPE tt_mixed_table_entry.
    LOOP AT result_table_value ASSIGNING FIELD-SYMBOL(<default_value>).
      INSERT VALUE #( offset = <default_value>-offset length = <default_value>-length is_link = abap_false ) INTO TABLE mixed_result_table.
    ENDLOOP.
    LOOP AT result_table_link ASSIGNING FIELD-SYMBOL(<default_link>).
      INSERT VALUE #( offset = <default_link>-offset length = <default_link>-length is_link = abap_true ) INTO TABLE mixed_result_table.
    ENDLOOP.

    IF lines( mixed_result_table ) = 0.
      MESSAGE w109(zaff_tools) WITH abap_doc_annotation-default INTO DATA(message) ##NEEDED.
      parser_log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
      RETURN.
    ENDIF.
    IF lines( mixed_result_table ) > 1.
      MESSAGE i107(zaff_tools) WITH abap_doc_annotation-default INTO message.
      parser_log->add_info( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
    ENDIF.
    DATA(warning_set) = abap_false.
    LOOP AT mixed_result_table ASSIGNING FIELD-SYMBOL(<entry>).
      check_next_word( offset = <entry>-offset + <entry>-length text_to_check = string_to_parse ).
      IF <entry>-is_link = abap_false AND decoded_abap_doc-default IS INITIAL.
        decoded_abap_doc-default = `"` && get_annotation_value( length = <entry>-length - 1 offset = <entry>-offset to_decode = string_to_parse length_of_annotation = 9 remove_whitespaces = abap_false ) && `"`.
      ELSEIF <entry>-is_link = abap_true AND decoded_abap_doc-default IS INITIAL.
        DATA(link) = get_annotation_value( length = <entry>-length - 1 offset = <entry>-offset to_decode = string_to_parse length_of_annotation = 9 remove_whitespaces = abap_true ).
        DATA(link_for_testing) = link.
        REPLACE ALL OCCURRENCES OF REGEX `\s` IN link_for_testing WITH `` ##REGEX_POSIX.
        REPLACE ALL OCCURRENCES OF REGEX `(@link|data:)` IN link_for_testing WITH `` ##REGEX_POSIX.
        SPLIT link_for_testing AT '.' INTO TABLE DATA(splitted).
        IF lines( splitted ) = 3.
          decoded_abap_doc-default = link.
        ELSEIF warning_set = abap_false.
          MESSAGE w111(zaff_tools) WITH abap_doc_annotation-default INTO message.
          parser_log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
          warning_set = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD parse_enum_values.
    IF decoded_abap_doc-enumvalues_link IS NOT INITIAL.
      RETURN.
    ENDIF.
    DATA(string_to_parse) = abap_doc_string.
    REPLACE ALL OCCURRENCES OF REGEX `\$values[\s]*(:[\s]*)?\{[\s]*@link` IN string_to_parse WITH `\$values\{@link` ##REGEX_POSIX.
    FIND ALL OCCURRENCES OF REGEX `\$values\{@link([^\}]+)\}` IN string_to_parse RESULTS DATA(result_table) ##REGEX_POSIX.
    IF lines( result_table ) = 0.
      MESSAGE w109(zaff_tools) WITH abap_doc_annotation-values INTO DATA(message) ##NEEDED.
      parser_log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
      RETURN.
    ENDIF.
    write_log_for_multiple_entries( result_table = result_table annotaion = abap_doc_annotation-values ).
    DATA(warning_written) = abap_false.
    LOOP AT result_table ASSIGNING FIELD-SYMBOL(<entry>).
      DATA(offset_found) = <entry>-offset.
      DATA(length_found) = <entry>-length.
      DATA(link) = get_annotation_value( length = length_found - 1  offset = offset_found to_decode = string_to_parse length_of_annotation = 13 remove_whitespaces = abap_true ).
      check_next_word( offset = offset_found + length_found text_to_check = string_to_parse ).
      DATA(link_for_testing) = link.
      REPLACE ALL OCCURRENCES OF REGEX `\s` IN link_for_testing WITH `` ##REGEX_POSIX.
      REPLACE ALL OCCURRENCES OF REGEX `data:` IN link_for_testing WITH `` ##REGEX_POSIX.
      SPLIT link_for_testing AT '.' INTO TABLE DATA(splitted).
      IF lines( splitted ) = 2 AND decoded_abap_doc-enumvalues_link IS INITIAL.
        decoded_abap_doc-enumvalues_link = link.
      ELSEIF lines( splitted ) <> 2 AND warning_written = abap_false.
        MESSAGE w111(zaff_tools) WITH abap_doc_annotation-values INTO message.
        parser_log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
        warning_written = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD parse_required.
    IF decoded_abap_doc-required IS NOT INITIAL.
      RETURN.
    ENDIF.
    FIND ALL OCCURRENCES OF abap_doc_annotation-required IN abap_doc_string RESULTS DATA(result_table).
    write_log_for_multiple_entries( result_table = result_table annotaion = abap_doc_annotation-required ).
    decoded_abap_doc-required = abap_true.
    LOOP AT result_table ASSIGNING FIELD-SYMBOL(<entry>).
      check_next_word( offset = <entry>-offset + <entry>-length text_to_check = abap_doc_string ).
    ENDLOOP.
  ENDMETHOD.
  METHOD parse_show_always.
    IF decoded_abap_doc-showalways IS NOT INITIAL.
      RETURN.
    ENDIF.
    FIND ALL OCCURRENCES OF abap_doc_annotation-show_always IN abap_doc_string RESULTS DATA(result_table).
    write_log_for_multiple_entries( result_table = result_table annotaion = abap_doc_annotation-show_always ).
    decoded_abap_doc-showalways = abap_true.
    LOOP AT result_table ASSIGNING FIELD-SYMBOL(<entry>).
      check_next_word( offset = <entry>-offset + <entry>-length text_to_check = abap_doc_string ).
    ENDLOOP.
  ENDMETHOD.
  METHOD parse_number_annotations.
    CASE key_word.
      WHEN abap_doc_annotation-minimum.
        IF decoded_abap_doc-minimum IS INITIAL.
          decoded_abap_doc-minimum = get_number_annotation( annotation_name = key_word ).
        ENDIF.
      WHEN abap_doc_annotation-maximum.
        IF decoded_abap_doc-maximum IS INITIAL.
          decoded_abap_doc-maximum = get_number_annotation( annotation_name = key_word ).
        ENDIF.
      WHEN abap_doc_annotation-exclusive_minimum.
        IF decoded_abap_doc-exclusive_minimum IS INITIAL.
          decoded_abap_doc-exclusive_minimum = get_number_annotation( annotation_name = key_word ).
        ENDIF.
      WHEN abap_doc_annotation-exclusive_maximum.
        IF decoded_abap_doc-exclusive_maximum IS INITIAL.
          decoded_abap_doc-exclusive_maximum = get_number_annotation( annotation_name = key_word ).
        ENDIF.
      WHEN abap_doc_annotation-multiple_of.
        IF decoded_abap_doc-multiple_of IS INITIAL.
          decoded_abap_doc-multiple_of = get_number_annotation( annotation_name = key_word ).
        ENDIF.
      WHEN abap_doc_annotation-min_length.
        IF decoded_abap_doc-min_length IS INITIAL.
          decoded_abap_doc-min_length = get_number_annotation( annotation_name = key_word ).
        ENDIF.
      WHEN abap_doc_annotation-max_length.
        IF decoded_abap_doc-max_length IS INITIAL.
          decoded_abap_doc-max_length = get_number_annotation( annotation_name = key_word ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD get_number_annotation.
    DATA(abap_doc) = abap_doc_string.
    DATA(dummy_annotation) = `$dummyannotation`.
    REPLACE ALL OCCURRENCES OF annotation_name IN abap_doc WITH dummy_annotation.
    REPLACE ALL OCCURRENCES OF REGEX  `\$dummyannotation[\s]*(:[\s]*)?` IN abap_doc WITH `\$dummyannotation` ##REGEX_POSIX.
    FIND ALL OCCURRENCES OF REGEX `\$dummyannotation[^\s]+` IN abap_doc RESULTS DATA(result_table) ##REGEX_POSIX.
    IF lines( result_table ) = 0.
      MESSAGE w109(zaff_tools) WITH abap_doc_annotation-values INTO DATA(message) ##NEEDED.
      parser_log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
      RETURN.
    ENDIF.
    write_log_for_multiple_entries( result_table = result_table annotaion = annotation_name ).
    DATA(annotation_length) = strlen( dummy_annotation ).
    DATA(regex_of_number_expressions) = cl_abap_regex=>create_pcre( pattern     = `(\+|-)?[0-9]+(.[0-9]+)?(e(\+|-)?[0-9]+)?`
                                                                    ignore_case = abap_true ).
    DATA(warning_written) = abap_false.
    LOOP AT result_table ASSIGNING FIELD-SYMBOL(<entry>).
      DATA(offset_found) = <entry>-offset.
      DATA(length_found) = <entry>-length.
      DATA(begin_of_number) = offset_found + annotation_length.
      DATA(length_of_number) = length_found - annotation_length.
      DATA(number_candidate) = abap_doc+begin_of_number(length_of_number).
      remove_leading_trailing_spaces( CHANGING string_to_work_on = number_candidate ).
      DATA(matcher) = regex_of_number_expressions->create_matcher( text = number_candidate ).
      DATA(match) = matcher->match( ).
      check_next_word( offset = offset_found + length_found text_to_check = abap_doc ).
      IF match = abap_true AND number IS INITIAL.
        number = number_candidate.
      ELSEIF match = abap_false AND warning_written = abap_false.
        MESSAGE w110(zaff_tools) WITH annotation_name INTO message.
        parser_log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
        warning_written = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_enum_value.
    IF decoded_abap_doc-enum_value IS NOT INITIAL.
      RETURN.
    ENDIF.
    DATA(string_to_parse) = abap_doc_string.
    REPLACE ALL OCCURRENCES OF REGEX `\$enumValue[\s]*(:[\s]*)?'` IN string_to_parse WITH `\$enumValue'` ##REGEX_POSIX.
    FIND ALL OCCURRENCES OF REGEX `\$enumValue'[^']*'` IN string_to_parse RESULTS DATA(result_table) ##REGEX_POSIX.
    IF lines( result_table ) = 0.
      MESSAGE w109(zaff_tools) WITH abap_doc_annotation-enum_value INTO DATA(message) ##NEEDED.
      parser_log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
      RETURN.
    ENDIF.
    write_log_for_multiple_entries( result_table = result_table annotaion = abap_doc_annotation-enum_value ).
    DATA(offset_found) = result_table[ 1 ]-offset.
    DATA(length_found) = result_table[ 1 ]-length.
    decoded_abap_doc-enum_value = get_annotation_value( length = length_found - 1 offset = offset_found to_decode = string_to_parse length_of_annotation = 11 remove_whitespaces = abap_true ).
    LOOP AT result_table ASSIGNING FIELD-SYMBOL(<entry>).
      check_next_word( offset = <entry>-offset + <entry>-length text_to_check = string_to_parse ).
    ENDLOOP.
  ENDMETHOD.

  METHOD remove_leading_trailing_spaces.
    SHIFT string_to_work_on RIGHT DELETING TRAILING space.
    SHIFT string_to_work_on LEFT DELETING LEADING space.
  ENDMETHOD.
  METHOD check_next_word.
    IF description_warning_is_needed = abap_true.
      RETURN.
    ENDIF.
    DATA(current_offset) = offset.
    DATA next_word TYPE string.
    DATA next_char TYPE c.

    WHILE next_char = space AND current_offset < strlen( text_to_check ).
      next_char = text_to_check+current_offset(1).
      current_offset += 1.
    ENDWHILE.
    next_word = next_char.
    IF current_offset >= strlen( text_to_check ).
      RETURN.
    ENDIF.
    DATA(regex_of_letter) = cl_abap_regex=>create_pcre( pattern = `[a-zA-Z]` ) ##NO_TEXT.
    DO.
      next_char = text_to_check+current_offset(1).
      current_offset += 1.
      next_word = next_word && next_char.
      IF regex_of_letter->create_matcher( text = next_char )->match( ) = abap_false OR current_offset >= strlen( text_to_check ).
        EXIT.
      ENDIF.
    ENDDO.
    remove_leading_trailing_spaces( CHANGING string_to_work_on = next_word ).
    IF strlen( next_word ) = 1 OR next_word+0(1) <> `$`.
      description_warning_is_needed = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD write_description_message.
    IF description_warning_is_needed = abap_true AND decoded_abap_doc-description IS INITIAL.
      MESSAGE w115(zaff_tools) INTO DATA(message) ##NEEDED.
      parser_log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
    ELSEIF description_warning_is_needed = abap_true AND decoded_abap_doc-description IS NOT INITIAL.
      MESSAGE i116(zaff_tools) INTO message.
      parser_log->add_info( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
    ENDIF.
  ENDMETHOD.

  METHOD write_log_for_multiple_entries.
    IF lines( result_table ) > 1.
      MESSAGE i107(zaff_tools) WITH annotaion INTO DATA(message) ##NEEDED.
      parser_log->add_info( message = zcl_aff_log=>get_sy_message( ) component_name = component_name ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_generator_helper DEFINITION
 FINAL
 CREATE PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS: generate
      IMPORTING generate_schema TYPE abap_bool
                interface_name  TYPE tadir-obj_name
                type_name       TYPE tadir-obj_name
      RETURNING VALUE(result)   TYPE string
      RAISING   zcx_aff_tools.
  PRIVATE SECTION.
    CLASS-METHODS get_format_version
      IMPORTING interface_name TYPE tadir-obj_name
      RETURNING VALUE(result)  TYPE i.
    CLASS-METHODS get_object_type_path
      IMPORTING interface_name TYPE tadir-obj_name
      RETURNING VALUE(path)    TYPE string.

ENDCLASS.

CLASS lcl_generator_helper IMPLEMENTATION.

  METHOD generate.
    DATA(absolute_name) = |\\INTERFACE={ interface_name }\\TYPE={ type_name }|.

    cl_abap_typedescr=>describe_by_name( EXPORTING p_name = absolute_name RECEIVING p_descr_ref = DATA(type_description) EXCEPTIONS type_not_found = 1 ).
    IF sy-subrc = 1.
      RAISE EXCEPTION NEW zcx_aff_tools( ).
    ENDIF.
    DATA element_description TYPE REF TO cl_abap_structdescr.
    element_description ?= type_description.
    DATA field TYPE REF TO data.
    FIELD-SYMBOLS <field> TYPE any.
    CREATE DATA field TYPE HANDLE element_description.
    ASSIGN field->* TO <field>.
    DATA my_type TYPE REF TO data.
    GET REFERENCE OF <field> INTO my_type.

    ASSIGN my_type->* TO <field>.

    DATA(format_version) = get_format_version( interface_name ).
    DATA(object_type_path) = get_object_type_path( interface_name ).
    DATA(schema_id) = |https://github.com/SAP/abap-file-formats/blob/main/file-formats/{ object_type_path }-v{ format_version }.json| ##NO_TEXT.
    DATA writer TYPE REF TO zcl_aff_writer.
    " set up the writer
    IF generate_schema = abap_true.
      writer = NEW zcl_aff_writer_json_schema( schema_id = schema_id format_version = format_version ).
    ELSE.
      writer = NEW zcl_aff_writer_xslt( ).
    ENDIF.

    DATA(generator) = NEW zcl_aff_generator( writer ).
    DATA(result_table) = generator->generate_type( <field> ).
    CONCATENATE LINES OF result_table INTO result SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.

  METHOD get_format_version.
    DATA format_version TYPE string.
    SPLIT interface_name  AT '_' INTO TABLE DATA(splitted_intfname).
    DATA(last) = splitted_intfname[ lines( splitted_intfname ) ].
    REPLACE ALL OCCURRENCES OF 'V' IN last WITH ''.
    TRY.
        DATA(regx) = '[[:alpha:]]+'.
*      check if the token only contains digits
        DATA(contains_chars) = xsdbool( count( val = last regex = regx ) > 0 ) ##REGEX_POSIX.
        DATA(default_format_version) = 1.
        IF contains_chars = abap_false.
          format_version  = last.
        ELSE.
          format_version = default_format_version.
        ENDIF.
      CATCH cx_sy_conversion_no_number.

        format_version = default_format_version.
    ENDTRY.

    result = CONV #( format_version ).
  ENDMETHOD.

  METHOD get_object_type_path.
    SPLIT interface_name  AT '_' INTO TABLE DATA(splitted_intfname).
    DATA(object_type) = splitted_intfname[ lines( splitted_intfname ) - 1 ].
    DATA(main_object_type) = object_type.
    IF object_type = 'REPS' OR object_type = 'FUNC'.
      main_object_type = 'FUGR'.
    ELSEIF object_type = 'INDX'.
      main_object_type = 'TABL'.
    ENDIF.
    path = |{ to_lower( main_object_type ) }/{ to_lower( object_type ) }|.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  PARAMETERS:
    p_schema TYPE c RADIOBUTTON GROUP sel USER-COMMAND upd DEFAULT 'X',
    p_xslt   TYPE c RADIOBUTTON GROUP sel ##NEEDED,
    p_intf   TYPE tadir-obj_name,
    p_type   TYPE tadir-obj_name DEFAULT 'ty_main'.
  TRY.
      DATA(xslt_content) = lcl_generator_helper=>generate( generate_schema = p_schema interface_name = to_upper( p_intf ) type_name = to_upper( p_type ) ).
      cl_demo_output=>display( xslt_content ).
    CATCH zcx_aff_tools INTO DATA(exception).
      cl_demo_output=>display( exception->get_text( ) ).
  ENDTRY.

****************************************************
INTERFACE lif_abapmerge_marker.
* abapmerge 0.14.3 - 2022-05-16T15:51:47.896Z
ENDINTERFACE.
****************************************************