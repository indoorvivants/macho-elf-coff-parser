import java.io.DataInputStream
case class DWARF()

object DWARF {
  def parse(ds: DataInputStream) = {
    implicit val endi = Endianness.LITTLE
    implicit val stream = ds
    import CommonParsers._
    val unit_length = uint32()
    println(unit_length)
    val version = uint16()

    println(version)

    val debug_abbrev_offset = uint64()
    println(debug_abbrev_offset)

  }
}
