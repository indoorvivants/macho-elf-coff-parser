import java.io.DataInputStream
import java.io.RandomAccessFile
import java.nio.channels.Channels

case class DWARF(header: DWARF.Header, abbrevs: Array[DWARF.Abbrev])

object DWARF {
  implicit val endi = Endianness.LITTLE
  import CommonParsers._
  case class Header(
      version: Int,
      is64: Boolean,
      unit_length: Long,
      unit_type: Byte,
      debug_abbrev_offset: Long
  )
  object Header {
    def parse(implicit ds: DataInputStream) = {

      val unit_length_s = uint32()

      val (dwarf64, unit_length) = if (unit_length_s == 0xffffffff) {
        (true, uint64())
      } else (false, unit_length_s.toLong)

      val version = uint16()
      assert(
        version == 3,
        s"Expected DWARF version 3 (for Scala Native) , got $version instead"
      )

      def read_ulong =
        if (dwarf64) uint64() else uint32()

      val (unit_type: Byte, address_size, debug_abbrev_offset) =
        if (version >= 5) {
          (
            uint8(),
            uint8(),
            uint64()
          )
        } else {
          val dao = read_ulong
          (
            0.toByte,
            uint8(),
            dao
          )
        }
      Header(
        version = version,
        is64 = dwarf64,
        unit_length = unit_length,
        unit_type = unit_type,
        debug_abbrev_offset = debug_abbrev_offset
      )

    }
  }

  case class Abbrev(
      code: Int,
      tag: Int,
      children: Boolean,
      attributes: Array[Attr]
  )
  case class Attr(at: Int, form: Int, value: Int)

  object Abbrev {
    def parse(implicit ds: DataInputStream) = {
      def readAttribute: Option[Attr] = {
        val at = read_unsigned_leb128()
        val form = read_unsigned_leb128()
        if (at == 0 && form == 0) None
        else Some(Attr(at, form, value = 0))
      }
      def readAbbrev: Option[Abbrev] = {
        val code = read_unsigned_leb128()
        if (code == 0) None
        else {
          val tag = read_unsigned_leb128()
          val children = uint8() == 1

          val attrs = Array.newBuilder[Attr]

          var stop = false

          while (!stop) {
            val attr = readAttribute

            attr.foreach(attrs += _)

            stop = attr.isEmpty
          }

          Some(Abbrev(code, tag, children, attrs.result()))
        }
      }

      val abbrevs = Array.newBuilder[Abbrev]

      var stop = false
      while (!stop) {
        val abbrev = readAbbrev
        abbrev.foreach(abbrevs += _)
        stop = abbrev.isEmpty
      }

      abbrevs.result()
    }
  }

  def parse(
      raf: RandomAccessFile,
      debug_info_offset: Int,
      debug_abbrev_offset: Int
  ) = {
    val ds =
      new DataInputStream(Channels.newInputStream(raf.getChannel()))

    raf.seek(debug_abbrev_offset)

    val abbrev = Abbrev.parse(ds)

    raf.seek(debug_info_offset)
    val header = Header.parse(ds)

    DWARF(header, abbrev)
  }

  def read_unsigned_leb128()(implicit ds: DataInputStream) = {
    var result = 0
    var shift = 0
    var stop = false
    while (!stop) {
      val byte = ds.readByte().toInt
      result |= (byte & 0x7f) << shift
      stop = (byte & 64) == 0
      shift += 7
    }

    result
  }

  object Attribute {
    val DW_AT_sibling = 0x01 // reference
    val DW_AT_location = 0x02 // exprloc, loclistptr
    val DW_AT_name = 0x03 // string
    val DW_AT_ordering = 0x09 // constant
    val DW_AT_byte_size = 0x0b // constant, exprloc, reference
    val DW_AT_bit_offset = 0x0c // constant, exprloc, reference
    val DW_AT_bit_size = 0x0d // constant, exprloc, reference
    val DW_AT_stmt_list = 0x10 // lineptr
    val DW_AT_low_pc = 0x11 // address
    val DW_AT_high_pc = 0x12 // address, constant
    val DW_AT_language = 0x13 // constant
    val DW_AT_discr = 0x15 // reference
    val DW_AT_discr_value = 0x16 // constant
    val DW_AT_visibility = 0x17 // constant
    val DW_AT_import = 0x18 // reference
    val DW_AT_string_length = 0x19 // exprloc, loclistptr
    val DW_AT_common_reference = 0x1a // reference
    val DW_AT_comp_dir = 0x1b // string
    val DW_AT_const_value = 0x1c // block, constant, string
    val DW_AT_containing_type = 0x1d // reference
    val DW_AT_default_value = 0x1e // reference
    val DW_AT_inline = 0x20 // constant
    val DW_AT_is_optional = 0x21 // flag
    val DW_AT_lower_bound = 0x22 // constant, exprloc, reference
    val DW_AT_producer = 0x25 // string
    val DW_AT_prototyped = 0x27 // flag
    val DW_AT_return_addr = 0x2a // exprloc, loclistptr
    val DW_AT_start_scope = 0x2c // constant, rangelistptr
    val DW_AT_bit_stride = 0x2e // constant, exprloc, reference
    val DW_AT_upper_bound = 0x2f // constant, exprloc, reference
    val DW_AT_abstract_origin = 0x31 // reference
    val DW_AT_accessibility = 0x32 // constant
    val DW_AT_address_class = 0x33 // constant
    val DW_AT_artificial = 0x34 // flag
    val DW_AT_base_types = 0x35 // reference
    val DW_AT_calling_convention = 0x36 // constant
    val DW_AT_count = 0x37 // constant, exprloc, reference
    val DW_AT_data_member_location = 0x38 // constant, exprloc, loclistptr
    val DW_AT_decl_column = 0x39 // constant
    val DW_AT_decl_file = 0x3a // constant
    val DW_AT_decl_line = 0x3b // constant
    val DW_AT_declaration = 0x3c // flag
    val DW_AT_discr_list = 0x3d // block
    val DW_AT_encoding = 0x3e // constant
    val DW_AT_external = 0x3f // flag
    val DW_AT_frame_base = 0x40 // exprloc, loclistptr
    val DW_AT_friend = 0x41 // reference
    val DW_AT_identifier_case = 0x42 // constant
    val DW_AT_macro_info = 0x43 // macptr
    val DW_AT_namelist_item = 0x44 // reference
    val DW_AT_priority = 0x45 // reference
    val DW_AT_segment = 0x46 // exprloc, loclistptr
    val DW_AT_specification = 0x47 // reference
    val DW_AT_static_link = 0x48 // exprloc, loclistptr
    val DW_AT_type = 0x49 // reference
    val DW_AT_use_location = 0x4a // exprloc, loclistptr
    val DW_AT_variable_parameter = 0x4b // flag
    val DW_AT_virtuality = 0x4c // constant
    val DW_AT_vtable_elem_location = 0x4d // exprloc, loclistptr
    val DW_AT_allocated = 0x4e // constant, exprloc, reference
    val DW_AT_associated = 0x4f // constant, exprloc, reference
    val DW_AT_data_location = 0x50 // exprloc
    val DW_AT_byte_stride = 0x51 // constant, exprloc, reference
    val DW_AT_entry_pc = 0x52 // address
    val DW_AT_use_UTF8 = 0x53 // flag
    val DW_AT_extension = 0x54 // reference
    val DW_AT_ranges = 0x55 // rangelistptr
    val DW_AT_trampoline = 0x56 // address, flag, reference, string
    val DW_AT_call_column = 0x57 // constant
    val DW_AT_call_file = 0x58 // constant
    val DW_AT_call_line = 0x59 // constant
    val DW_AT_description = 0x5a // string
    val DW_AT_binary_scale = 0x5b // constant
    val DW_AT_decimal_scale = 0x5c // constant
    val DW_AT_small = 0x5d // reference
    val DW_AT_decimal_sign = 0x5e // constant
    val DW_AT_digit_count = 0x5f // constant
    val DW_AT_picture_string = 0x60 // string
    val DW_AT_mutable = 0x61 // flag
    val DW_AT_threads_scaled = 0x62 // flag
    val DW_AT_explicit = 0x63 // flag
    val DW_AT_object_pointer = 0x64 // reference
    val DW_AT_endianity = 0x65 // constant
    val DW_AT_elemental = 0x66 // flag
    val DW_AT_pure = 0x67 // flag
    val DW_AT_recursive = 0x68 // flag
    val DW_AT_signature = 0x69 // reference
    val DW_AT_main_subprogram = 0x6a // flag
    val DW_AT_data_bit_offset = 0x6b // constant
    val DW_AT_const_expr = 0x6c // flag
    val DW_AT_enum_class = 0x6d // flag
    val DW_AT_linkage_name = 0x6e // string

  }

  object Forms {

    val DW_FORM_addr = 0x01
    val DW_FORM_block2 = 0x03
    val DW_FORM_block4 = 0x04
    val DW_FORM_data2 = 0x05
    val DW_FORM_data4 = 0x06
    val DW_FORM_data8 = 0x07
    val DW_FORM_string = 0x08
    val DW_FORM_block = 0x09
    val DW_FORM_block1 = 0x0a
    val DW_FORM_data1 = 0x0b
    val DW_FORM_flag = 0x0c
    val DW_FORM_sdata = 0x0d
    val DW_FORM_strp = 0x0e
    val DW_FORM_udata = 0x0f
    val DW_FORM_ref_addr = 0x10
    val DW_FORM_ref1 = 0x11
    val DW_FORM_ref2 = 0x12
    val DW_FORM_ref4 = 0x13
    val DW_FORM_ref8 = 0x14
    val DW_FORM_ref_udata = 0x15
    val DW_FORM_indirect = 0x16
    val DW_FORM_sec_offset = 0x17
    val DW_FORM_exprloc = 0x18
    val DW_FORM_flag_present = 0x19
    val DW_FORM_ref_sig8 = 0x20
  }

  object Tags {
    val DW_TAG_array_type = 0x01
    val DW_TAG_class_type = 0x02
    val DW_TAG_entry_point = 0x03
    val DW_TAG_enumeration_type = 0x04
    val DW_TAG_formal_parameter = 0x05
    val DW_TAG_imported_declaration = 0x08
    val DW_TAG_label = 0x0a
    val DW_TAG_lexical_block = 0x0b
    val DW_TAG_member = 0x0d
    val DW_TAG_pointer_type = 0x0f
    val DW_TAG_reference_type = 0x10
    val DW_TAG_compile_unit = 0x11
    val DW_TAG_string_type = 0x12
    val DW_TAG_structure_type = 0x13
    val DW_TAG_subroutine_type = 0x15
    val DW_TAG_typedef = 0x16
    val DW_TAG_union_type = 0x17
    val DW_TAG_unspecified_parameters = 0x18
    val DW_TAG_variant = 0x19
    val DW_TAG_common_block = 0x1a
    val DW_TAG_common_inclusion = 0x1b
    val DW_TAG_inheritance = 0x1c
    val DW_TAG_inlined_subroutine = 0x1d
    val DW_TAG_module = 0x1e
    val DW_TAG_ptr_to_member_type = 0x1f
    val DW_TAG_set_type = 0x20
    val DW_TAG_subrange_type = 0x21
    val DW_TAG_with_stmt = 0x22
    val DW_TAG_access_declaration = 0x23
    val DW_TAG_base_type = 0x24
    val DW_TAG_catch_block = 0x25
    val DW_TAG_const_type = 0x26
    val DW_TAG_constant = 0x27
    val DW_TAG_enumerator = 0x28
    val DW_TAG_file_type = 0x29
    val DW_TAG_friend = 0x2a
    val DW_TAG_namelist = 0x2b
    val DW_TAG_namelist_item = 0x2c
    val DW_TAG_packed_type = 0x2d
    val DW_TAG_subprogram = 0x2e
    val DW_TAG_template_type_parameter = 0x2f
    val DW_TAG_template_value_parameter = 0x30
    val DW_TAG_thrown_type = 0x31
    val DW_TAG_try_block = 0x32
    val DW_TAG_variant_part = 0x33
    val DW_TAG_variable = 0x34
    val DW_TAG_volatile_type = 0x35
    val DW_TAG_dwarf_procedure = 0x36
    val DW_TAG_restrict_type = 0x37
    val DW_TAG_interface_type = 0x38
    val DW_TAG_namespace = 0x39
    val DW_TAG_imported_module = 0x3a
    val DW_TAG_unspecified_type = 0x3b
    val DW_TAG_partial_unit = 0x3c
    val DW_TAG_imported_unit = 0x3d
    val DW_TAG_mutable_type = 0x3e
    val DW_TAG_condition = 0x3f
    val DW_TAG_shared_type = 0x40
  }
}
