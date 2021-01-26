import java.io.{FileOutputStream, PrintStream}
import java.nio.file.{Files, Paths}
import java.util

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.convert.ImplicitConversions.`list asScalaBuffer`
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.duration.NANOSECONDS
import scala.io.StdIn
import scala.math.Ordered.orderingToOrdered

///*
// * CS3210 - Principles of Programming Languages - Fall 2020
// * Instructor: Thyago Mota
// * Description: Prg02 - PalindromesSearch
// * Student(s) Name(s): Prashant Panth and Echglene Woy
// */

object PalindromesSearch {

  var count = 0;
  var check = {}
  val OUTPUT_FILE_NAME = "output.txt"

  //  def permutations(lst: List[Int]): List[List[Int]] = lst match{
  //
  //    case Nil => List(Nil)
  //    case List(i) => List(lst)
  //    case _ =>
  //      (for(i <- lst.indices.toList) yield{
  //        val (before,rest) = lst.splitAt(i)
  //        val elem = rest.head
  //        val subpermutes = permutations(before ++ rest.tail)
  //        subpermutes.map(elem::_)
  //      }).flatten
  //  }
  def palindrome_check(update_List: mutable.Buffer[Int]): Boolean = {
    update_List == update_List.reverse
  }
  def perms(list: List[Int]): List[List[Int]] = {
    if (list.size == 1) List(list)
    else for {
      x <- list
      y <- perms(list.filter(_ != x))
    } yield x :: y
  }
  def printPartitions(x:Int, target: Int, max: Int, suffix: util.ArrayList[Int], list: util.ArrayList[util.ArrayList[Int]] ): Unit = {
    if (target == 0) {
      list.add(suffix)
//      println(suffix)

    }
    else {
      var j=x
      if (max > 1)
        printPartitions(j,target, max-1, suffix, list);
      if (max <= target) {
        val tmp = new util.ArrayList[Int]()
        tmp.add(0, (max));
        tmp.addAll(suffix);
        printPartitions(j,target-max, max, tmp, list);
        j+=1
      }
    }
    palindrome_result(list,max)
  }

  def palindrome_result(update_List: util.ArrayList[util.ArrayList[Int]],z:Int): Unit = {
    update_List.last.permutations.foreach(palin => {
      if (palindrome_check(palin) && palin.contains(z) && check!= palin) {
             val string_palin = palin.mkString("(", ",", ")")
              check = palin.clone()
            println(string_palin)
      }else{
          check = {}
    }

    //        val string_palin = palin.mkString("(", ",", ")")
    //
    ////        if (Blank == "y") {
    ////          //          var i = 0
    ////          fileWrite.write(string_palin +"\n")
    ////        }
    //        println(string_palin)
    //        count += 1

    })
  }

  def main(args: Array[String]): Unit = {
    val list = new util.ArrayList[util.ArrayList[Int]]()
    val out = new PrintStream(new FileOutputStream(OUTPUT_FILE_NAME))
    println("Welcome to the palindromic sequence project!")
    println("Use: java PalindromesSearch n m [y]")
    println("[y]: when informed, all palindromic sequences must be saved to a file")
    println("What's your n value? ")
    val n = StdIn.readLine()
    if (n.equals("")) {
      println("Use: java PalindromesSearch n m [y]\n" +
        "[y]: when informed, all palindromic sequences must be saved to a file")
      System.exit(1)
    }
    print("What's your m value ? ")
    val m = StdIn.readLine()
    if (m.equals("")) {
      println("Use: java PalindromesSearch n m [y]\n" +
        "[y]: when informed, all palindromic sequences must be saved to a file")
      System.exit(1)
    }
    print(f"Parameter n = $n \nParameter m = $m \n")
    println("Would you like to output to a file [y]")
    val OUTPUT = StdIn.readLine()
    println("Generating palindromic sequences...")

    val start = System.nanoTime()
    printPartitions(1,n.toInt,m.toInt,new util.ArrayList[Int](),list)

    for( i <- 0 until list.size()){
      if(list.get(i).contains(m.toInt)) {
        perms(list.get(i).toList).foreach(eachPerm => {
//          palindrome_result(list,m.toInt)
          //          if (eachPerm == eachPerm.reverse) {
//          println(eachPerm.mkString("(", ",", ")"))
          count += 1
//          if (OUTPUT == "y") {
//            out.println(eachPerm.mkString("(", ",", ")"))
//          }
          //          }
        })
      }
    }
    val elapsedTime = (System.nanoTime() - start)
    println("Done!")
    println("Number of palindromic sequences found: " + count)
    println("It took me " + (NANOSECONDS.toSeconds(elapsedTime)) + "s")
    out.close()
  }
  //https://stackoverflow.com/questions/58314196/how-to-generate-all-permutations-of-a-listliststring?noredirect=1&lq=1
}
