import scala.annotation.tailrec

object MergeSortTail {

@main
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

  @tailrec
  def merge(seq1: List[Int], seq2: List[Int], acc: List[Int] = List()): List[Int] = {
    (seq1, seq2) match {
      case (Nil, _) => acc.reverse ++ seq2  // Musimy odwrócić akumulator, ponieważ dodawanie na początek powoduje odwrotną kolejność
      case (_, Nil) => acc.reverse ++ seq1  // Podobnie jak powyżej, odwracamy akumulator
      case (x :: xs, y :: ys) =>
        if (x < y) merge(xs, seq2, x :: acc)  // Dodajemy x na początek akumulatora
        else merge(seq1, ys, y :: acc)        // Dodajemy y na początek akumulatora
    }
  }

  def printArr(arr: Array[Int]): Unit = {
    for (i <- arr) {
      print(i)
    }
    println(" ")
  }
}
