package scala.scalanative.runtime.dwarf

import ELF._
import scala.scalanative.runtime.dwarf.Bits.X32
import scala.scalanative.runtime.dwarf.Bits.X64
import scala.scalanative.unsigned.UShort
import scala.scalanative.unsigned.UInt

case class ELF(header: Header, sections: List[Section])

sealed trait Bits extends Product with Serializable
object Bits {
  case object X32 extends Bits
  case object X64 extends Bits
}

object ELF {
  import CommonParsers._
  def parse(ds: BinaryFile): ELF = {
    implicit val stream: BinaryFile = ds

    val magic = uint32()(Endianness.BIG, stream)
    assert(magic == MAGIC)

    val cls = uint8()(Endianness.BIG, stream)

    val endianness = uint8()(Endianness.BIG, stream)

    val version = uint8()(Endianness.BIG, stream)
    val abi = uint8()(Endianness.BIG, stream)
    val abi_version = uint8()(Endianness.BIG, stream)
    val padding = stream.skipNBytes(7L)

    implicit val endi: Endianness =
      if (endianness == 1) Endianness.LITTLE else Endianness.BIG
    implicit val bits: Bits = if (cls == 1) Bits.X32 else Bits.X64

    val fileType = uint16()
    val machine = uint16()
    val versionAgain = uint32()

    val entryPointAddress = readVariableSize()
    val programHeaderStart = readVariableSize()
    val sectionsHeaderStart = readVariableSize()

    val flags = uint32()
    val headerSize = uint16()
    val programHeaderSize = uint16()
    val programHeaderEntries = uint16()
    val sectionsHeaderSize = uint16()
    val sectionsHeaderEntries = uint16()

    val sectionNamesEntryIndex = uint16()
    pprint.log(sectionNamesEntryIndex)

    bits match {
      case X32 => assert(ds.position() == 0x34)
      case X64 => assert(ds.position() == 0x40)

    }
    ds.seek(programHeaderStart)
    readProgramHeaders(programHeaderEntries)

    ds.seek(sectionsHeaderStart)
    val sections =
      readSectionHeaders(sectionsHeaderEntries, sectionNamesEntryIndex)

    ELF(Header(), sections.toList)

  }

  def readSectionHeaders(
      entries: UShort,
      nameSectionIndex: Int
  )(implicit ds: BinaryFile, bits: Bits, endi: Endianness) = {
    pprint.log(s"$entries")

    val infos = collection.mutable.ArrayBuffer.empty[SectionInfo]

    for (i <- 0 until entries) {
      val sectionNameAddress = uint32()
      val sectionType = uint32()
      val flags = readVariableSize()
      val virtualAddressInMemory = readVariableSize()
      val offsetInFileImage = readVariableSize()
      val sectionSizeInFileImage = readVariableSize()
      val sectionIndex = uint32()
      val sectionInfo = uint32()
      val sectionAlignment = readVariableSize()
      val entrySize = readVariableSize()

      infos += SectionInfo(
        size = sectionSizeInFileImage,
        nameOffset = sectionNameAddress.toInt,
        offsetInFile = offsetInFileImage
      )

      pprint.log(s"$i ${sectionNameAddress} ${offsetInFileImage.toHexString}")
    }

    val nameSection = infos(nameSectionIndex)
    val allData = {
      ds.seek(nameSection.offsetInFile); ds.readNBytes(nameSection.size.toInt)
    }

    def readName(at: Int) = {
      var i = at
      var stop = false
      val builder = Array.newBuilder[Byte]
      while (!stop) {
        val byte = allData(i)
        if (byte == 0) {
          stop = true
        } else {
          builder += byte
        }

        i += 1
      }

      new String(builder.result())
    }

    infos.map { info =>
      val name = readName(info.nameOffset)

      Section(name, info)
    }.toVector

  }

  def readProgramHeaders(
      entries: UShort
  )(implicit ds: BinaryFile, bits: Bits, endi: Endianness) = {
    // pprint.log(s"$entries")
    for (i <- 0 until entries) {
      var flags: UInt = 0
      val tpe = uint32()

      if (bits == Bits.X64) flags = uint32()

      val offsetInFileImage = readVariableSize()
      val virtualAddressInMemory = readVariableSize()
      val physicalAddress = readVariableSize()
      val sizeInFileImage = readVariableSize()
      val sizeInMemory = readVariableSize()

      if (bits == Bits.X32) flags = uint32()

      val align = readVariableSize()

      // pprint.log(
      //   s"${offsetInFileImage.toHexString} ${virtualAddressInMemory.toHexString} ${physicalAddress.toHexString} ${sizeInFileImage.toHexString} ${sizeInMemory.toHexString} $align"
      // )

    }

  }

  val MAGIC = 0x7f454c46

  def readVariableSize()(implicit
      ds: BinaryFile,
      bits: Bits,
      endi: Endianness
  ) =
    bits match {
      case Bits.X32 => uint32().toLong
      case Bits.X64 => uint64()
    }

  case class Header()
  case class ProgramHeader(
  )
  case class Section(name: String, info: SectionInfo) {
    def offset = info.offsetInFile
    def size = info.size
  }

  case class SectionInfo(size: Long, offsetInFile: Long, nameOffset: Int)

}
