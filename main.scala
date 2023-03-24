//> using scala "2.12"
//> using lib "com.lihaoyi::pprint::0.8.1"
//> using lib "com.indoorvivants.detective::platform::0.0.2"

package scala.scalanative.runtime.dwarf

import java.io.FileInputStream
import java.io.DataInputStream
import java.io.File
import java.io.RandomAccessFile
import pprint.pprintln
import java.nio.channels.Channels
import com.indoorvivants.detective.Platform
import scala.scalanative.unsigned.UInt

case class Location(low_pc: Long, high_pc: Long, filename: String)

object Main {
  def main(args: Array[String]): Unit = {
    val filename = args.head
    val file = new File(filename)
    implicit val bf: BinaryFile = new BinaryFile(
      new RandomAccessFile(filename, "r")
    )

    if (Platform.os == Platform.OS.MacOS) {

      val macho = MachO.parse(bf)
      val sections = macho.segments.flatMap(_.sections)

      val dwarf = for {
        debug_info <- sections.find(_.sectname == "__debug_info")
        debug_abbrev <- sections.find(_.sectname == "__debug_abbrev")
        debug_str <- sections.find(_.sectname == "__debug_str")
        debug_line <- sections.find(_.sectname == "__debug_line")

      } yield {

        val (dies, strings) = readDWARF(
          debug_info = DWARF.Section(debug_info.offset, debug_info.size),
          debug_abbrev = DWARF.Section(debug_abbrev.offset, debug_abbrev.size),
          debug_str = DWARF.Section(debug_str.offset, debug_str.size)
        )

        val lines = DWARF.Lines
          .parse(DWARF.Section(debug_line.offset, debug_line.size))

        pprintln(lines.files, width = 300, height = Int.MaxValue)

        pprintln(
          lines.find(0x10008a398L)
        )
      }
    } else if (Platform.os == Platform.OS.Linux) {
      sys.error(
        "Linux is not supported yet, will someone please write an ELF parser"
      )
    } else if (Platform.os == Platform.OS.Windows) {
      sys.error(
        "Windows is not supported yet, will someone please write a COFF parser, or whatever windows uses"
      )
    }

  }

  def readDWARF(
      debug_info: DWARF.Section,
      debug_abbrev: DWARF.Section,
      debug_str: DWARF.Section
  )(implicit bf: BinaryFile) = {
    DWARF.parse(
      debug_info = DWARF.Section(debug_info.offset, debug_info.size),
      debug_abbrev = DWARF.Section(debug_abbrev.offset, debug_abbrev.size)
    ) ->
      DWARF.Strings.parse(
        DWARF.Section(debug_str.offset, debug_str.size)
      )

  }

  def readLocations(dwarf: Vector[DWARF.DIE], strings: DWARF.Strings) = {

    val locs = Vector.newBuilder[Location]

    dwarf.foreach { die =>
      die.units
        .collectFirst {
          case cu if cu.abbrev.exists(_.tag == DWARF.Tag.DW_TAG_compile_unit) =>
            cu
        }
        .foreach { cu =>
          val n = for {
            name <- cu.values
              .find(_._1.at == DWARF.Attribute.DW_AT_name)
              .map(_._2.asInstanceOf[UInt])
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

    locs.result()
  }
}
