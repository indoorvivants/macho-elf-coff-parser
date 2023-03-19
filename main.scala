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
    pprintln(sections)

    // sections.find(_.sectname == "__debug_info").map { debugSection =>
    //   val offset = debugSection.offset
    //   pprint.pprintln(debugSection, showFieldNames = true)

    //   raf.seek(offset)

    //   pprintln(DWARF.parse(raf))
    // }

    val dwarf = for {
      debug_info <- sections.find(_.sectname == "__debug_info")
      debug_abbrev <- sections.find(_.sectname == "__debug_abbrev")

    } yield DWARF.parse(
      raf,
      debug_info_offset = debug_info.offset,
      debug_abbrev_offset = debug_abbrev.offset
    )

    pprintln(dwarf)
  }
}
