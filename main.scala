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

    sections.find(_.sectname == "__debug_info").map { debugSection =>
      val offset = debugSection.offset
      pprint.pprintln(debugSection, showFieldNames = true)

      raf.seek(offset)
      val channel = Channels.newInputStream(raf.getChannel())

      DWARF.parse(new DataInputStream(channel))
    }
  }
}
