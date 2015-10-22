package coursera.stanford

import java.io.File
import scala.io.Source

trait ChoosePivot {
  def index(input: Array[Int], left: Int, right: Int): Int
}

class FirstElement extends ChoosePivot {
  override def index(input: Array[Int], left: Int, right: Int): Int = left
}

class LastElement extends ChoosePivot {
  override def index(input: Array[Int], left: Int, right: Int): Int = right - 1
}

class MedianOfThree extends ChoosePivot {
  override def index(input: Array[Int], left: Int, right: Int): Int = {
    val middle =
      if (len(left, right) % 2 == 0)
        left + (len(left, right) / 2) - 1
      else
        left + (len(left, right) / 2)

    val rightIndex = right - 1
    val sortedByValue = List((left, input(left)), (middle, input(middle)), (rightIndex, input(rightIndex))).sortBy(_._2)
    sortedByValue(1)._1
  }

  def len(left: Int, right: Int) = right - left
}

case class QuickSort(choosePivot: ChoosePivot) {

  def sortAndCountComparisons(input: Array[Int], left: Int, right: Int): Int = {
    if (left >= right - 1) return 0

    val pivotIdenx = choosePivot.index(input, left, right)

    swap(input, pivotIdenx, left)
    val newPivotIndex = partition(input, left, right)

    val comparisons = countComparisons(left, right)
    val leftComparisons = sortAndCountComparisons(input, left, newPivotIndex)
    val rightComparisons = sortAndCountComparisons(input, (newPivotIndex + 1), right)

    comparisons + leftComparisons + rightComparisons
  }

  def swap(input: Array[Int], a: Int, b: Int) {
    val tmp = input(a)
    input(a) = input(b)
    input(b) = tmp
  }

  def partition(input: Array[Int], left: Int, right: Int): Int = {
    val pivot = input(left)
    var i = left + 1

    for (j <- (left + 1) until right) {
      if (input(j) < pivot) {
        swap(input, i, j)
        i += 1
      }
    }

    val newPivotIndex = i - 1
    swap(input, left, newPivotIndex)
    newPivotIndex
  }
  
  def countComparisons(left: Int, right: Int): Int = (right - left) - 1
}

object QuickSort {

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(new File("src/main/resources/quick_sort_input.txt")).getLines.map(_.toInt).toArray
    
    val comparisonsFirstElement = QuickSort(new FirstElement()).sortAndCountComparisons(input.clone, left = 0, right = input.length)
    println(s"comparisons of choose pivot using FirstElement $comparisonsFirstElement")
    
    val comparisonsLastElement = QuickSort(new LastElement()).sortAndCountComparisons(input.clone, left = 0, right = input.length)
    println(s"comparisons of choose pivot using LastElement $comparisonsLastElement")
    
    val comparisonsMedianOfThree = QuickSort(new MedianOfThree()).sortAndCountComparisons(input.clone, left = 0, right = input.length)
    println(s"comparisons of choose pivot using MedianOfThree $comparisonsMedianOfThree")
  }
}
