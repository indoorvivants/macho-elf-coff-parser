import java.io.DataInputStream
import java.io.RandomAccessFile
import java.nio.channels.Channels
import scala.collection.immutable.IntMap
import DWARF.Form.DW_FORM_strp
import java.nio.channels.FileChannel

object DWARF {
  implicit val endi: Endianness = Endianness.LITTLE
  import CommonParsers._

  case class DIE(
      header: DWARF.Header,
      abbrevs: Vector[DWARF.Abbrev],
      units: Vector[DWARF.CompileUnit]
  )

  case class Header(
      version: Int,
      is64: Boolean,
      unit_length: Long,
      unit_type: Byte,
      debug_abbrev_offset: Long,
      address_size: Long,
      unit_offset: Long
  )
  object Header {
    def parse(raf: RandomAccessFile)(implicit ds: DataInputStream): Header = {

      val unit_length_s = uint32()

      val (dwarf64, unit_length) = if (unit_length_s == 0xffffffff) {
        (true, uint64())
      } else (false, unit_length_s.toLong)

      val unit_offset = raf.getChannel().position()

      val version = uint16()
      assert(
        version == 3,
        s"Expected DWARF version 3 (for Scala Native) , got $version instead"
      )

      def read_ulong: Long =
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
        debug_abbrev_offset = debug_abbrev_offset,
        address_size = address_size.toInt,
        unit_offset = unit_offset
      )

    }
  }

  case class Abbrev(
      code: Int,
      tag: Tag,
      children: Boolean,
      attributes: Vector[Attr]
  )
  case class Attr(at: Attribute, form: Form, value: Int)

  object Abbrev {
    def parse(implicit ds: DataInputStream): Vector[Abbrev] = {
      def readAttribute: Option[Attr] = {
        val at = read_unsigned_leb128()
        val form = read_unsigned_leb128()
        if (at == 0 && form == 0) None
        else
          Some(
            Attr(
              Attribute.fromCode(at),
              Form.fromCodeUnsafe(form),
              value = 0
            )
          )
      }
      def readAbbrev: Option[Abbrev] = {
        val code = read_unsigned_leb128()
        if (code == 0) None
        else {
          val tag = read_unsigned_leb128()
          val children = uint8() == 1

          val attrs = Vector.newBuilder[Attr]

          var stop = false

          while (!stop) {
            val attr = readAttribute

            attr.foreach(attrs += _)

            stop = attr.isEmpty
          }

          Some(Abbrev(code, Tag.fromCodeUnsafe(tag), children, attrs.result()))
        }
      }

      val abbrevs = Vector.newBuilder[Abbrev]

      var stop = false
      while (!stop) {
        val abbrev = readAbbrev
        abbrev.foreach(abbrevs += _)
        stop = abbrev.isEmpty
      }

      abbrevs.result()
    }
  }

  case class CompileUnit(abbrev: Option[Abbrev], values: Map[Attr, Any])
  case class Section(offset: Int, size: Long)
  case class Strings(buf: Array[Byte]) {
    def read(at: Int): String = {
      assert(at < buf.length)
      val until = buf.indexWhere(_ == 0, at)

      new String(buf.slice(at, until))
    }
  }
  object Strings {
    def parse(raf: RandomAccessFile, debug_str: Section): Strings = {
      val pos = raf.getChannel().position()
      raf.seek(debug_str.offset)

      val buf = Array.ofDim[Byte](debug_str.size.toInt)
      raf.readFully(buf)
      raf.seek(pos)

      Strings(buf)
    }
  }

  def parse(
      raf: RandomAccessFile,
      debug_info: Section,
      debug_abbrev: Section
  ): Vector[DIE] = {
    raf.seek(debug_info.offset)
    val end_offset = debug_info.offset + debug_info.size
    def stop = raf.getChannel().position() >= end_offset
    val dies = Vector.newBuilder[DIE]
    while (!stop) {
      val die = DIE.parse(raf, debug_info, debug_abbrev)

      dies += die

    }

    dies.result()
  }

  object DIE {
    def parse(
        raf: RandomAccessFile,
        debug_info: Section,
        debug_abbrev: Section
    ) = {

      val channel = raf.getChannel()
      implicit val ds =
        new DataInputStream(Channels.newInputStream(channel))
      val header = Header.parse(raf)
      val pos = channel.position()

      raf.seek(debug_abbrev.offset + header.debug_abbrev_offset)
      val abbrev = Abbrev.parse(ds)
      val idx = IntMap(abbrev.map(a => a.code -> a): _*)

      raf.seek(pos)

      val units = readUnits(channel, header.unit_offset, header, idx)

      DIE(header, abbrev, units)
    }
  }

  def readUnits(
      channel: FileChannel,
      offset: Long,
      header: Header,
      idx: IntMap[Abbrev]
  )(implicit ds: DataInputStream): Vector[CompileUnit] = {

    val end_offset = offset + header.unit_length

    def stop = channel.position() >= end_offset
    val units = Vector.newBuilder[CompileUnit]

    while (!stop) {
      val attrs = Map.newBuilder[Attr, Any]

      val code = read_unsigned_leb128()
      idx.get(code) match {
        case None =>
          units += CompileUnit(None, Map.empty)
        case s @ Some(abbrev) =>
          abbrev.attributes.foreach { attr =>
            val value = AttributeValue.parse(header, attr.form)
            val pos = channel.position()
            attrs += (attr -> value)
          }

          units += CompileUnit(s, attrs.result())
      }

    }
    units.result()
  }

  object AttributeValue {
    def parse(header: Header, form: Form)(implicit ds: DataInputStream): Any = {
      import Form._
      form match {
        case DW_FORM_strp =>
          if (header.is64) uint64()
          else uint32()
        case DW_FORM_data1 =>
          uint8()
        case DW_FORM_data2 =>
          uint16()
        case DW_FORM_data4 =>
          uint32()
        case DW_FORM_addr =>
          if (header.address_size == 4)
            uint32()
          else if (header.address_size == 8)
            uint64()
          else throw new RuntimeException(s"Uknown header size: ${header.address_size}")
        case DW_FORM_flag =>
          uint8() == 1
        case DW_FORM_ref_addr =>
          if (header.is64) uint64()
          else uint32()
        case DW_FORM_block1 =>
          val len = uint8()
          ds.readNBytes(len)

      }

    }
  }

  def read_unsigned_leb128()(implicit ds: DataInputStream): Int = {
    var result = 0
    var shift = 0
    var stop = false
    while (!stop) {
      val byte = ds.readByte().toInt
      result |= (byte & 0x7f) << shift
      stop = (byte & 0x80) == 0
      shift += 7
    }

    result
  }

  sealed abstract class Attribute(val code: Int)
      extends Product
      with Serializable {
    override def toString(): String =
      s"[${getClass().getSimpleName().dropRight(1)}:0x${code.toHexString.reverse.padTo(2, '0').reverse}]"
  }

  object Attribute {
    case object DW_AT_sibling extends Attribute(0x01)
    case object DW_AT_location extends Attribute(0x02)
    case object DW_AT_name extends Attribute(0x03)
    case object DW_AT_ordering extends Attribute(0x09)
    case object DW_AT_byte_size extends Attribute(0x0b)
    case object DW_AT_bit_offset extends Attribute(0x0c)
    case object DW_AT_bit_size extends Attribute(0x0d)
    case object DW_AT_stmt_list extends Attribute(0x10)
    case object DW_AT_low_pc extends Attribute(0x11)
    case object DW_AT_high_pc extends Attribute(0x12)
    case object DW_AT_language extends Attribute(0x13)
    case object DW_AT_discr_value extends Attribute(0x15)
    case object DW_AT_visibility extends Attribute(0x16)
    case object DW_AT_import extends Attribute(0x17)
    case object DW_AT_string_length extends Attribute(0x19)
    case object DW_AT_common_reference extends Attribute(0x1a)
    case object DW_AT_comp_dir extends Attribute(0x1b)
    case object DW_AT_const_value extends Attribute(0x1c)
    case object DW_AT_containing_type extends Attribute(0x1d)
    case object DW_AT_default_value extends Attribute(0x1e)
    case object DW_AT_inline extends Attribute(0x20)
    case object DW_AT_is_optional extends Attribute(0x21)
    case object DW_AT_lower_bound extends Attribute(0x22)
    case object DW_AT_producer extends Attribute(0x25)
    case object DW_AT_prototyped extends Attribute(0x27)
    case object DW_AT_return_addr extends Attribute(0x2a)
    case object DW_AT_start_scope extends Attribute(0x2c)
    case object DW_AT_stride_size extends Attribute(0x2e)
    case object DW_AT_upper_bound extends Attribute(0x2f)
    case object DW_AT_abstract_origin extends Attribute(0x31)
    case object DW_AT_accessibility extends Attribute(0x32)
    case object DW_AT_address_class extends Attribute(0x33)
    case object DW_AT_artificial extends Attribute(0x34)
    case object DW_AT_base_types extends Attribute(0x35)
    case object DW_AT_calling_convention extends Attribute(0x36)
    case object DW_AT_count extends Attribute(0x37)
    case object DW_AT_data_member_location extends Attribute(0x38)
    case object DW_AT_decl_column extends Attribute(0x39)
    case object DW_AT_decl_file extends Attribute(0x3a)
    case object DW_AT_decl_line extends Attribute(0x3b)
    case object DW_AT_declaration extends Attribute(0x3c)
    case object DW_AT_ranges extends Attribute(0x55)
    case class Unknown(value: Int) extends Attribute(value)

    final private val codeMap = Seq(
      DW_AT_sibling,
      DW_AT_location,
      DW_AT_name,
      DW_AT_ordering,
      DW_AT_byte_size,
      DW_AT_bit_offset,
      DW_AT_bit_size,
      DW_AT_stmt_list,
      DW_AT_low_pc,
      DW_AT_high_pc,
      DW_AT_language,
      DW_AT_discr_value,
      DW_AT_visibility,
      DW_AT_import,
      DW_AT_string_length,
      DW_AT_common_reference,
      DW_AT_comp_dir,
      DW_AT_const_value,
      DW_AT_containing_type,
      DW_AT_default_value,
      DW_AT_inline,
      DW_AT_is_optional,
      DW_AT_lower_bound,
      DW_AT_producer,
      DW_AT_prototyped,
      DW_AT_return_addr,
      DW_AT_start_scope,
      DW_AT_stride_size,
      DW_AT_upper_bound,
      DW_AT_abstract_origin,
      DW_AT_accessibility,
      DW_AT_address_class,
      DW_AT_artificial,
      DW_AT_base_types,
      DW_AT_calling_convention,
      DW_AT_count,
      DW_AT_data_member_location,
      DW_AT_decl_column,
      DW_AT_decl_file,
      DW_AT_decl_line,
      DW_AT_declaration,
      DW_AT_ranges
    ).map(t => t.code -> t).toMap

    def fromCode(code: Int): Attribute =
      codeMap.getOrElse(code, Unknown(code))
    def fromCodeUnsafe(code: Int): Attribute = codeMap.getOrElse(
      code,
      throw new RuntimeException(s"Unknown DWARF attribute code: $code")
    )
  }

  sealed abstract class Form(val code: Int) extends Product with Serializable {
    override def toString(): String =
      s"[${getClass().getSimpleName().dropRight(1)}:0x${code.toHexString.reverse.padTo(2, '0').reverse}]"

  }

  object Form {
    case object DW_FORM_addr extends Form(0x01)
    case object DW_FORM_block2 extends Form(0x03)
    case object DW_FORM_block4 extends Form(0x04)
    case object DW_FORM_data2 extends Form(0x05)
    case object DW_FORM_data4 extends Form(0x06)
    case object DW_FORM_data8 extends Form(0x07)
    case object DW_FORM_string extends Form(0x08)
    case object DW_FORM_block extends Form(0x09)
    case object DW_FORM_block1 extends Form(0x0a)
    case object DW_FORM_data1 extends Form(0x0b)
    case object DW_FORM_flag extends Form(0x0c)
    case object DW_FORM_sdata extends Form(0x0d)
    case object DW_FORM_strp extends Form(0x0e)
    case object DW_FORM_udata extends Form(0x0f)
    case object DW_FORM_ref_addr extends Form(0x10)
    case object DW_FORM_ref1 extends Form(0x11)
    case object DW_FORM_ref2 extends Form(0x12)
    case object DW_FORM_ref4 extends Form(0x13)
    case object DW_FORM_ref8 extends Form(0x14)
    case object DW_FORM_ref_udata extends Form(0x15)
    case object DW_FORM_indirect extends Form(0x16)
    case object DW_FORM_sec_offset extends Form(0x17)
    case object DW_FORM_exprloc extends Form(0x18)
    case object DW_FORM_flag_present extends Form(0x19)
    case object DW_FORM_ref_sig8 extends Form(0x20)

    private final val codeMap: Map[Int, Form] = Seq(
      DW_FORM_addr,
      DW_FORM_block2,
      DW_FORM_block4,
      DW_FORM_data2,
      DW_FORM_data4,
      DW_FORM_data8,
      DW_FORM_string,
      DW_FORM_block,
      DW_FORM_block1,
      DW_FORM_data1,
      DW_FORM_flag,
      DW_FORM_sdata,
      DW_FORM_strp,
      DW_FORM_udata,
      DW_FORM_ref_addr,
      DW_FORM_ref1,
      DW_FORM_ref2,
      DW_FORM_ref4,
      DW_FORM_ref8,
      DW_FORM_ref_udata,
      DW_FORM_indirect,
      DW_FORM_sec_offset,
      DW_FORM_exprloc,
      DW_FORM_flag_present,
      DW_FORM_ref_sig8
    ).map(form => form.code -> form).toMap

    def fromCode(code: Int): Option[Form] = codeMap.get(code)
    def fromCodeUnsafe(code: Int): Form = codeMap.getOrElse(
      code,
      throw new RuntimeException(s"Unknown DWARF abbrev code: $code")
    )
  }

  sealed abstract class Tag(val code: Int)

  object Tag {
    case object DW_TAG_array_type extends Tag(0x01)
    case object DW_TAG_class_type extends Tag(0x02)
    case object DW_TAG_entry_point extends Tag(0x03)
    case object DW_TAG_enumeration_type extends Tag(0x04)
    case object DW_TAG_formal_parameter extends Tag(0x05)
    case object DW_TAG_imported_declaration extends Tag(0x08)
    case object DW_TAG_label extends Tag(0x0a)
    case object DW_TAG_lexical_block extends Tag(0x0b)
    case object DW_TAG_member extends Tag(0x0d)
    case object DW_TAG_pointer_type extends Tag(0x0f)
    case object DW_TAG_reference_type extends Tag(0x10)
    case object DW_TAG_compile_unit extends Tag(0x11)
    case object DW_TAG_string_type extends Tag(0x12)
    case object DW_TAG_structure_type extends Tag(0x13)
    case object DW_TAG_subroutine_type extends Tag(0x15)
    case object DW_TAG_typedef extends Tag(0x16)
    case object DW_TAG_union_type extends Tag(0x17)
    case object DW_TAG_unspecified_parameters extends Tag(0x18)
    case object DW_TAG_variant extends Tag(0x19)
    case object DW_TAG_common_block extends Tag(0x1a)
    case object DW_TAG_common_inclusion extends Tag(0x1b)
    case object DW_TAG_inheritance extends Tag(0x1c)
    case object DW_TAG_inlined_subroutine extends Tag(0x1d)
    case object DW_TAG_module extends Tag(0x1e)
    case object DW_TAG_ptr_to_member_type extends Tag(0x1f)
    case object DW_TAG_set_type extends Tag(0x20)
    case object DW_TAG_subrange_type extends Tag(0x21)
    case object DW_TAG_with_stmt extends Tag(0x22)
    case object DW_TAG_access_declaration extends Tag(0x23)
    case object DW_TAG_base_type extends Tag(0x24)
    case object DW_TAG_catch_block extends Tag(0x25)
    case object DW_TAG_const_type extends Tag(0x26)
    case object DW_TAG_constant extends Tag(0x27)
    case object DW_TAG_enumerator extends Tag(0x28)
    case object DW_TAG_file_type extends Tag(0x29)
    case object DW_TAG_friend extends Tag(0x2a)
    case object DW_TAG_namelist extends Tag(0x2b)
    case object DW_TAG_namelist_item extends Tag(0x2c)
    case object DW_TAG_packed_type extends Tag(0x2d)
    case object DW_TAG_subprogram extends Tag(0x2e)
    case object DW_TAG_template_type_param extends Tag(0x2f)

    private final val codeMap = Seq(
      DW_TAG_array_type,
      DW_TAG_class_type,
      DW_TAG_entry_point,
      DW_TAG_enumeration_type,
      DW_TAG_formal_parameter,
      DW_TAG_imported_declaration,
      DW_TAG_label,
      DW_TAG_lexical_block,
      DW_TAG_member,
      DW_TAG_pointer_type,
      DW_TAG_reference_type,
      DW_TAG_compile_unit,
      DW_TAG_string_type,
      DW_TAG_structure_type,
      DW_TAG_subroutine_type,
      DW_TAG_typedef,
      DW_TAG_union_type,
      DW_TAG_unspecified_parameters,
      DW_TAG_variant,
      DW_TAG_common_block,
      DW_TAG_common_inclusion,
      DW_TAG_inheritance,
      DW_TAG_inlined_subroutine,
      DW_TAG_module,
      DW_TAG_ptr_to_member_type,
      DW_TAG_set_type,
      DW_TAG_subrange_type,
      DW_TAG_with_stmt,
      DW_TAG_access_declaration,
      DW_TAG_base_type,
      DW_TAG_catch_block,
      DW_TAG_const_type,
      DW_TAG_constant,
      DW_TAG_enumerator,
      DW_TAG_file_type,
      DW_TAG_friend,
      DW_TAG_namelist,
      DW_TAG_namelist_item,
      DW_TAG_packed_type,
      DW_TAG_subprogram,
      DW_TAG_template_type_param
    ).map(t => t.code -> t).toMap

    def fromCode(code: Int): Option[Tag] = codeMap.get(code)
    def fromCodeUnsafe(code: Int): Tag = codeMap.getOrElse(
      code,
      throw new RuntimeException(s"Unknown DWARF tag code: $code")
    )
  }

}
