package edu.knoldus
import scala.util.control.Breaks.{break, breakable}
class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {
    for ( i <- 1 until array.length) {
      breakable {
        for ( j <- (1 to i).reverse) {
          if (array(j-1) < array(j)) {
            break
          } else {
            val temp = array(j)
            array(j) = array(j-1)
            array(j-1) = temp
          }
        }
      }
    }
    array
  }


  def selectionSort(array: Array[Int]): Array[Int] = {

    def swap(arr: Array[Int], i1: Int, i2: Int) = {
      val temp = arr(i1)
      arr(i1) = arr(i2)
      arr(i2) = temp }

    for (i <- 0 until array.size - 1)
      swap(array, i, (i + 1 until array.size).foldLeft(i)((currMin, index) =>
        if (array(index) < array(currMin)) index
        else currMin))
    array
  }


  def bubbleSort(array: Array[Int]): Array[Int] = {
    for(i<- 1 to array.length-1){
      for(j <- (i-1) to 0 by -1){
        if(array(j)>array(j+1)){
          val temp=array(j+1)
          array(j+1)=array(j)
          array(j)=temp
        }
      }
    }
    array
  }


}
