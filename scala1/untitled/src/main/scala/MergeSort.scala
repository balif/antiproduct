object MergeSort {

//  @main
  def main(): Unit = {
    println("Hello world!")
    val a1 = Array(1, 2, 3, 4, 3, 5, 6, 7).reverse



    for (i <- mergeSort(a1.toList)) println(i)


  }

  def mergeSort(seq: List[Int]): List[Int] = {
    seq match
      case Nil => Nil
      case xs::Nil => seq
      case _ => {
        val (l,r) = seq.splitAt(Math.floorDiv(seq.length,2))
        merge(mergeSort(l),mergeSort(r))
      }
  }

  def merge(seq1: List[Int], seq2: List[Int]): List[Int] = {
    (seq1, seq2) match {
      case (Nil, _) => seq2
      case (_, Nil) => seq1
      case (x :: xs, y :: ys) =>
        if (x < y) x :: merge(xs, seq2)
        else y :: merge(seq1, ys)
    }
  }

  def printArr(arr: Array[Int]): Unit = {
    for (i <- arr) {
      print(i)
    }
    println(" ")
  }
}
