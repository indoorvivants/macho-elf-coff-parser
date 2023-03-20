import java.io.FileInputStream
import java.io.DataInputStream
import java.io.File
import java.io.RandomAccessFile
import pprint.pprintln
import java.nio.channels.Channels

object Main {
  def main(args: Array[String]): Unit = {
    val filename = args.head
    val file = new File(filename)
    val is = new FileInputStream(file)
    val ds = new DataInputStream(is)
    val raf = new RandomAccessFile(filename, "r")

    val macho = MachO.parse(ds)
    val sections = macho.segments.flatMap(_.sections)

    val dwarf = for {
      debug_info <- sections.find(_.sectname == "__debug_info")
      debug_abbrev <- sections.find(_.sectname == "__debug_abbrev")
      debug_str <- sections.find(_.sectname == "__debug_str")
      dwarf = DWARF.parse(
        raf,
        debug_info = DWARF.Section(debug_info.offset, debug_info.size),
        debug_abbrev = DWARF.Section(debug_abbrev.offset, debug_abbrev.size),
        debug_str = DWARF.Section(debug_str.offset, debug_str.size)
      )

    } yield {

      val strps =
        dwarf.units.flatMap(
          _.values
            .filter(_._1.form == DWARF.Form.DW_FORM_strp)
            .map(_._2.asInstanceOf[Int])
        )

      strps.foreach { strp => 
        println(dwarf.strings.read(strp))
      }
    }
  }
}
