package advent.days

import advent.{Solution, Task}

import scala.annotation.tailrec

object Day9 extends Task {

  override def solve(input: List[String]): (Solution, Solution) = {
    val diskMap = input.head

    var index = -1
    val partition = diskMap.indices
      .flatMap(i => i % 2 match {
        case 0 =>
          index += 1
          (0 until diskMap(i).asDigit).map(_ => Block.File(index))
        case 1 => (0 until diskMap(i).asDigit).map(_ => Block.Free)
      })

    val calc: Seq[Block] => Long = disk => disk.zipWithIndex.foldLeft(0L)((acc, tup) => {
      val (block, index) = tup
      block match {
        case Block.File(id) => acc + (id * index)
        case _ => acc
      }
    })

    val maxId = partition.map {
      case Block.File(id) => id
      case Block.Free => 0
    }.max

    (
      () => calc(frag(partition)),
      () => calc(defrag(partition)(maxId))
    )
  }

  @tailrec
  private def frag(partition: Seq[Block]): Seq[Block] = {
    val freeIndex = partition.zipWithIndex.find((b, _) => b == Block.Free).get._2
    val blockIndex = partition.zipWithIndex.findLast((b, _) => b match {
      case Block.Free => false
      case Block.File(_) => true
    }).get._2

    if (freeIndex > blockIndex) {
      return partition
    }

    frag(partition.updated(freeIndex, partition(blockIndex)).updated(blockIndex, Block.Free))
  }

  @tailrec
  private def defrag(partition: Seq[Block])(highestId: Long): Seq[Block] = {
    if (highestId == 0) {
      return partition
    }

    val file = partition.zipWithIndex.filter((b, _) => b match {
      case Block.File(id) => id == highestId
      case _ => false
    })

    val fileStart = file.head._2
    val fileSize = file.size

    case class State(maxStart: Int, maxLength: Int, currentStart: Int, currentLength: Int, found: Boolean = false)

    val freeSpace = partition.zipWithIndex.filter((b, i) => i < fileStart).foldLeft(State(0, 0, 0, 0))((acc, tup) => {
      val (block, index) = tup
      block match {
        case Block.Free =>
          val current = acc.copy(currentLength = acc.currentLength + 1)
          if (current.currentLength == fileSize && !acc.found) {
            current.copy(maxStart = current.currentStart, maxLength = current.currentLength, found = true)
          } else {
            current
          }
        case _ =>
          acc.copy(currentStart = index + 1, currentLength = 0)
      }
    })

    if (freeSpace.maxStart > 0 && freeSpace.maxLength > 0) {
      val updated = partition.zipWithIndex.map((b, i) => {
        if (i >= freeSpace.maxStart && i < freeSpace.maxStart + fileSize) {
          Block.File(highestId)
        } else if (i >= fileStart && i < fileStart + fileSize) {
          Block.Free
        } else {
          b
        }
      })
      defrag(updated)(highestId - 1)
    } else {
      defrag(partition)(highestId - 1)
    }
  }

  private enum Block {
    case File(id: Long)
    case Free
  }
}
