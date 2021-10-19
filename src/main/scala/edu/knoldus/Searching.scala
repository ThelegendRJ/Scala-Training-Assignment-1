package edu.knoldus
import scala.annotation.tailrec

class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    def recursiveSearch(start: Int, end: Int): Boolean = {
      if (start > end) false
      else {
        val mid = start + (end - start + 1) / 2
        if (array(mid) == elem)
          true
        else if (elem < array(mid)) recursiveSearch(start, mid-1)
        else recursiveSearch(mid + 1, end)
      }

    }
    recursiveSearch(0,array.length -1)
  }




  def linearSearch(array: Array[Int], elem: Int): Boolean = {

    if (array.isEmpty) false
    else if (array.head == elem) true
    else linearSearch(array.tail, elem)
  }

}
