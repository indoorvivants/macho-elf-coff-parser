import java.io.FileInputStream
import java.io.DataInputStream
import java.io.File
import java.io.RandomAccessFile
import pprint.pprintln
import java.nio.channels.Channels

case class Location(low_pc: Long, high_pc: Long, filename: String)

object Main {
  def main(args: Array[String]): Unit = {
    val filename = args.head
    val file = new File(filename)
    val is = new FileInputStream(file)
    val ds = new DataInputStream(is)
    val raf = new RandomAccessFile(filename, "r")

    val macho = MachO.parse(ds)
    val sections = macho.segments.flatMap(_.sections)
    pprintln(sections)

    val dwarf = for {
      debug_info <- sections.find(_.sectname == "__debug_info")
      debug_abbrev <- sections.find(_.sectname == "__debug_abbrev")
      debug_str <- sections.find(_.sectname == "__debug_str")
      strings = DWARF.Strings.parse(
        raf,
        DWARF.Section(debug_str.offset, debug_str.size)
      )
      dwarf = DWARF.parse(
        raf,
        debug_info = DWARF.Section(debug_info.offset, debug_info.size),
        debug_abbrev = DWARF.Section(debug_abbrev.offset, debug_abbrev.size)
      )

    } yield {

      val locs = Vector.newBuilder[Location]

      dwarf.foreach { die =>
        die.units
          .collectFirst {
            case cu
                if cu.abbrev.exists(_.tag == DWARF.Tag.DW_TAG_compile_unit) =>
              cu
          }
          .foreach { cu =>
            val n = for {
              name <- cu.values
                .find(_._1.at == DWARF.Attribute.DW_AT_name)
                .map(_._2.asInstanceOf[Int])
                .map(strings.read)
            } yield name

            n.foreach { fileName =>
              die.units.collect {
                case cu
                    if cu.abbrev.exists(_.tag == DWARF.Tag.DW_TAG_subprogram) =>
                  val loc = for {
                    low_pc <- cu.values
                      .find(_._1.at == DWARF.Attribute.DW_AT_low_pc)
                      .map(_._2.asInstanceOf[Long])
                    high_pc <- cu.values
                      .find(_._1.at == DWARF.Attribute.DW_AT_high_pc)
                      .map(_._2.asInstanceOf[Long])
                  } yield Location(low_pc, high_pc, fileName)

                  loc.foreach(locs += _)
              }
            }
          }

      }

      pprintln(locs.result())

    }

  }
}
