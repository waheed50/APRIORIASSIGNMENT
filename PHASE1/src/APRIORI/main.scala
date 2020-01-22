package APRIORI
import java.io._
import scala._
import scala.util.Random
import scala.io.Source
object main {
  def main(args:Array[String])
  {
    val file = new PrintWriter("/home/Waheed/"+new File("result"+".txt" ))
  println("Enter Records:")
    var Records=readInt
    println("Enter Support:")
    var S=readDouble
    var alph=List[Char]()
    for(i<-0 to 25)
      {
      alph=alph:::List((i+65).toChar)
      }//.range(65,91).map(x=>x.toChar)
   var binary=List[List[Int]]()
    for(i<-0 to Records-1)
   {
    binary=binary:::List( List.fill(26)(0+ Random.nextInt( (1- 0) + 1 )))
   }
    //var v= List.fill(transactionsNum)(List.fill(26)(0+ scala.util.Random.nextInt( (1- 0) + 1 )))
    var Records2= binary.map(x=>(alph.zip(x)).filter(_._2!=0).map(_._1))
     var ItemCount=Records2.flatten.groupBy(identity).map(x=>(x._1,x._2.length))//.toList
     println("Item Count:")
    ItemCount.foreach(println)
    println("1 Frequent ItemSets:")
   var Freq=ItemCount.toList.sortBy(_._2).filter(_._1>=S)
   file.write("1 Frequent Itemsets:\n"+Freq.mkString)
   file.close
   
}
}
