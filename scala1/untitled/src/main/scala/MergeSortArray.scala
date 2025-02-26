
object MergeSortArray {

//  @main
  def main(): Unit = {
    println("Hello world!")
    val a1 = Array(1, 2, 3, 4, 3, 5, 6, 7).reverse

    merge(a1, 0, a1.length)

    for (i <- a1) println(i)


  }

  def merge(arr: Array[Int], x: Int, y: Int): Unit = {
    val diff = y - x
    if (diff > 1) {
      val half = x + Math.floorDiv(diff, 2)
      merge(arr, x, half)
      merge(arr, half, y)

      mergeSorted(arr, x, half, y)
    } else {
      mergeSorted(arr, x, x, y)
    }

  }

  def mergeSorted(arr: Array[Int], s: Int, m: Int, e: Int): Unit = {

    println(s"Merge {$s} {$m} {$e}")
    printArr(arr)
    val l: Array[Int] = arr.slice(s, m)
    val r: Array[Int] = arr.slice(m, e)
    var i: Int = 0;
    var j: Int = 0;

    while (i < l.length && j < r.length) {
      if (l(i) < r(j)) {
        arr(s + i + j) = l(i)
        i = i + 1
      } else {
        arr(s + i + j) = r(j)
        j = j + 1
      }
    }
    for (i <- i until l.length) {
      arr(s + i + j) = l(i)
    }
    for (j <- j until r.length) {
      arr(s + i + j) = r(j)
    }

    printArr(arr)


  }

  def printArr(arr: Array[Int]): Unit = {
    for (i <- arr) {
      print(i)
    }
    println(" ")
  }
}
