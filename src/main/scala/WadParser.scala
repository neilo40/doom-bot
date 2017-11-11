import java.io.{File, FileInputStream}
import java.nio.{ByteBuffer, ByteOrder, MappedByteBuffer}
import java.nio.channels.FileChannel.MapMode._
import math.abs

object WadParser {
  val HEADER_SIZE = 12
  val DOOR_LINEDEF_TYPES = List(1, 117, 63, 114, 29, 111, 90, 105, 4, 108, 31, 118, 61, 115, 103, 112, 86, 106,
    2, 109, 46, 42, 116, 50, 113, 75, 107, 3, 110, 196, 175, 76, 16)
  val LIFT_LINEDEF_TYPES = List(66, 15, 148, 143, 67, 14, 149, 144, 68, 20, 95, 22, 47, 181, 162, 87, 53, 182,
    163, 89, 54, 62, 21, 88, 10, 123, 122, 120, 121, 211, 212)
  val NORMAL_EXIT_TYPES = List(11, 52, 197)
  val EXIT_TYPES: List[Int] = NORMAL_EXIT_TYPES ::: List(51, 124, 198)
  val SWITCH_TYPES: List[Int] = EXIT_TYPES ::: List(103)

  private def createStream(fromFile: String): MappedByteBuffer = {
    val file = new File(fromFile)
    val fileSize = file.length
    val stream = new FileInputStream(file)
    val buffer = stream.getChannel.map(READ_ONLY, 0, fileSize)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer
  }

  private def extractWadType(byteStream: MappedByteBuffer): String = {
    val wadTypeBytes = new Array[Byte](4)
    byteStream.get(wadTypeBytes, 0, 4)
    wadTypeBytes.map(_.toChar).mkString
  }

  private def extractNumLumps(byteStream: MappedByteBuffer): Int = byteStream.getInt()

  private def extractData(byteStream: MappedByteBuffer): ByteBuffer = {
    val dataEnd = byteStream.getInt()
    val dataBytes = byteStream.slice()
    byteStream.position(dataEnd)
    dataBytes
  }

  private def extractLumps(byteStream: MappedByteBuffer, data: ByteBuffer): List[Lump] =
    byteStream.remaining() match {
      case 0 => List()
      case _ => extractLump(byteStream, data) +: extractLumps(byteStream, data)
    }

  private def extractLump(byteStream: MappedByteBuffer, data: ByteBuffer): Lump = {
    val filePos = byteStream.getInt() - HEADER_SIZE
    val size = byteStream.getInt()
    val nameBytes = new Array[Byte](8)
    byteStream.get(nameBytes, 0, 8)
    val name = nameBytes.map(_.toChar).mkString.trim()
    val dataBytes = new Array[Byte](size)
    if (filePos >= 0) {
      data.position(filePos)
      data.get(dataBytes, 0, size)
      Lump(name, dataBytes.toList)
    } else {
      Lump(name, List())
    }
  }

  private def createLumpMaps(lumps: List[Lump]): Map[String, Map[String, Lump]] = {
    var currentLevelName: Option[String] = None
    var currentLevelLumps: Map[String, Lump] = Map()
    var lumpMap: Map[String, Map[String, Lump]] = Map()
    val levelNamePattern = "(E[0-9]M[0-9])".r

    lumps foreach { lump =>
      lump.name match {
        case levelNamePattern(levelName) =>
          if (currentLevelName.isDefined) lumpMap += (currentLevelName.get -> currentLevelLumps)
          currentLevelName = Some(levelName)
          currentLevelLumps = Map(lump.name -> lump)
        case _ =>
          if (currentLevelName.isDefined) currentLevelLumps += (lump.name -> lump)
      }
    }

    lumpMap + (currentLevelName.get -> currentLevelLumps)
  }

  private def extractLevels(lumps: List[Lump]): List[Level] = {
    val lumpMaps = createLumpMaps(lumps)
    lumpMaps.keys.map(levelName => extractLevel(levelName, lumpMaps(levelName))).toList
  }

  private def extractLevel(name: String, lumps: Map[String, Lump]): Level = {
    val vertices = extractVertices(lumps("VERTEXES"))
    val sectors = extractSectors(lumps("SECTORS"))
    val sidedefs = extractSidedefs(lumps("SIDEDEFS"), sectors)
    val linedefs = extractLinedefs(lumps("LINEDEFS"), vertices, sidedefs)
    val things = extractThings(lumps("THINGS"))
    val start = extractStart(things)
    val exit = extractExit(linedefs, start)
    val quadTree = LineMXQuadTree.createQuadTree(linedefs)
    val doorSwitches = extractDoorSwitches(linedefs)

    Level(name, linedefs, quadTree, start, exit, doorSwitches)
  }

  private def extractVertices(lump: Lump): List[Vertex] = lump.data.sliding(4, 4).map(extractVertex).toList

  private def extractVertex(bytes: List[Byte]): Vertex = {
    val x = ByteBuffer.wrap(bytes.take(2).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val y = ByteBuffer.wrap(bytes.slice(2, 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    Vertex(x, y)
  }

  private def extractSectors(lump: Lump): List[Sector] = (lump.data.sliding(26, 26) map extractSector).toList

  private def extractSector(bytes: List[Byte]): Sector = {
    val floorHeight = ByteBuffer.wrap(bytes.take(2).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val ceilingHeight = ByteBuffer.wrap(bytes.slice(2, 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val sectorType = ByteBuffer.wrap(bytes.slice(22, 24).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val tag = ByteBuffer.wrap(bytes.slice(24, 26).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    Sector(sectorType, tag, floorHeight, ceilingHeight)
  }

  private def extractSidedefs(lump: Lump, sectors: List[Sector]): List[Sidedef] =
    lump.data.sliding(30, 30).map(extractSidedef(_, sectors)).toList

  private def extractSidedef(bytes: List[Byte], sectors: List[Sector]): Sidedef = {
    val sectorId = ByteBuffer.wrap(bytes.slice(28, 30).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val sector = sectors(sectorId)
    Sidedef(sectorId, Some(sector))
  }

  private def extractLinedefs(lump: Lump, vertices: List[Vertex], sidedefs: List[Sidedef]): List[Linedef] =
    lump.data.sliding(14, 14).map(extractLinedef(_, vertices, sidedefs)).toList

  private def extractLinedef(bytes: List[Byte], vertices: List[Vertex], sidedefs: List[Sidedef]): Linedef = {
    val aIndex = ByteBuffer.wrap(bytes.take(2).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val bIndex = ByteBuffer.wrap(bytes.slice(2, 4).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val flags = ByteBuffer.wrap(bytes.slice(4, 6).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val specialType = ByteBuffer.wrap(bytes.slice(6, 8).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val sectorTag = ByteBuffer.wrap(bytes.slice(8, 10).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val leftSide = ByteBuffer.wrap(bytes.slice(10, 12).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val rightSide = ByteBuffer.wrap(bytes.slice(12, 14).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt

    val leftSideDef = if (leftSide == -1) None else Some(sidedefs(leftSide))
    val rightSideDef = if (rightSide == -1) None else Some(sidedefs(rightSide))
    val leftHeight = leftSideDef.getOrElse(rightSideDef.get).sector.get.floorHeight
    val rightHeight = rightSideDef.getOrElse(leftSideDef.get).sector.get.floorHeight
    val heightDifference = abs(leftHeight - rightHeight)
    val nonTraversable =
      leftSide == -1 ||
      rightSide == -1 ||
      blocksPlayerAndMonsters(flags) ||
      (heightDifference > 20 && !isLift(specialType))

    Linedef(vertices(aIndex), vertices(bIndex), nonTraversable, Some(sectorTag), Some(specialType),
      rightSideDef, leftSideDef)
  }

  private def blocksPlayerAndMonsters(flags: Int): Boolean = (flags & 0x0001) == 1

  private def isLift(specialType: Int): Boolean = LIFT_LINEDEF_TYPES.contains(specialType)

  private def extractThings(lump: Lump): List[Thing] = (lump.data.sliding(10, 10) map extractThing).toList

  private def extractThing(bytes: List[Byte]): Thing = {
    val position = extractVertex(bytes.take(4))
    val angle = ByteBuffer.wrap(bytes.slice(4, 6).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    val doomId = ByteBuffer.wrap(bytes.slice(6, 8).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort().toInt
    Thing(position, angle, doomId)
  }

  private def extractStart(things: List[Thing]): Vertex = things.find(_.doomId == 1).get.position

  private def extractExit(linedefs: List[Linedef], start: Vertex): Vertex = {
    val normalExitLines = linedefs.find( line => NORMAL_EXIT_TYPES.contains(line.lineType.get))
    normalExitLines match {
      case Some(x) => x.midpoint
      case _ => start
    }
  }

  private def extractDoorSwitches(linedefs: List[Linedef]): List[DoorSwitch] = {
    val doorLines = linedefs.filter(line => SWITCH_TYPES.contains(line.lineType.getOrElse(-1)))
    doorLines map DoorSwitch.fromWadLine
  }

  def createWad(fromFile: String): Wad = {
    val byteStream = createStream(fromFile)
    val wadType = extractWadType(byteStream)
    val numLumps = extractNumLumps(byteStream)  // Although we don't use this, we still have to pop it off the byteStream
    val data = extractData(byteStream)
    val lumps = extractLumps(byteStream, data)
    val levels = extractLevels(lumps).sortBy(_.name)

    Wad(wadType, levels)
  }
}
