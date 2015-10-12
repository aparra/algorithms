package coursera.stanford

import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object RichList {
  implicit class Patching(input: List[Int]) {

    def get(index: Int): Int =
      if (index >= input.length)
        Integer.MAX_VALUE
      else
        input(index)
  }
}

object MergeSortAndCountInversions {

  def sortAndCountInversions(input: List[Int]): (Long, List[Int]) =
    input match {
      case Nil             => (0, Nil)
      case in @ (x :: Nil) => (0, in)
      case in => {
        val half = in.size / 2
        val (leftCount, sortedLeft) = sortAndCountInversions(in take half)
        val (righCount, sortedRigth) = sortAndCountInversions(in drop half)
        val (mergeCount, mergeSort) = mergeAndCountSplitInversions(sortedLeft, sortedRigth)
        (leftCount + righCount + mergeCount, mergeSort)
      }
    }

  def mergeAndCountSplitInversions(left: List[Int], right: List[Int], count: Long = 0): (Long, List[Int]) = {
    import RichList._
    
    val merge = ArrayBuffer[Int]()
    var (i, j, inversions) = (0, 0, 0L)

    for (_ <- 0 until left.length + right.length) {
      if (left.get(i) < right.get(j)) {
        merge += left(i)
        i += 1
      } else {
        merge += right(j)
        j += 1
        inversions += left.length - i
      }
    }
    (inversions, merge.toList)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(new File("src/main/resources/integer_array.txt")).getLines.map(_.toInt).toList
    val (inversions, sortedInput) = sortAndCountInversions(input)
    
    println(s"total inversions: $inversions")
    println(sortedInput)
  }
}