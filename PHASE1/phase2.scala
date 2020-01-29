package APRIORI

import java.io._
import scala._
import scala.util.Random
import scala.io.Source
import sun.security.util.Length
object apriori {
  def main(args:Array[String])
  {
    println("Enter Number of Transactions:")
//Getting the no of transactions
    var transactionsNum=scala.Console.readInt
    println("Enter Support from range 0 to 1")
    var Sup=transactionsNum*scala.Console.readDouble
    var alphabetsList=List.range(65,91).map(x=>x.toChar)
   var v= List.fill(transactionsNum)(List.fill(26)(scala.util.Random.nextInt(2 )))
   var transactions= v.map(x=>(x.zip(alphabetsList)).filter(_._1==1).map(_._2))
    var count=transactions.flatten.groupBy(identity).map(x=>(x._1,x._2.size)).toList
   println("count")
    count.foreach(println)
    
    println("1 Frequent")
   var sorted=count.sortBy(_._1).filter(_._2>=Support).map(x=>x._1)//.foreach(println)
   //var v1=List[List[Char]]()
   
   println("when k is 1 the items sets are")
   sorted.foreach(println)
  
   var tt=sorted.combinations(2).toList//.toList.flatten
    
 //tt.foreach(println)
   var v1=tt.map(x=>transactions.map(y=>if(x.intersect(y)==x) 1 else 0)).map(x=>x.count(_==1)).zip(tt).filter(_._1>=Sup).map(_._2)
  println("when k is 2 the items sets are")
 // var t=v1.flatten
  v1.foreach(println)
   tt= v1.map(x=>v1.dropWhile(_==x).map(y=>if(x(x.length-1)==y(1))(x:::y).distinct else x )).map(x=>x.filter(_.length==3)).flatten
  v1=tt.map(x=>transactions.map(y=>if(x.intersect(y)==x) 1 else 0)).map(x=>x.count(_==1)).zip(tt).filter(_._1>=Sup).map(_._2)
  println("when k is 2 the items sets are")
   v1.foreach(println)
  var i=2
  var k=4
  while(v1.length>1)
  {
  tt=v1.map(x=>v1.dropWhile(_==x).map(y=>if(x.take(i)==y.take(i)) (x.take(i):::y.take(i)).distinct else x)).map(x=>x.filter(_.length==k)).flatten  
    k=k+1
    i=i+1
    v1=tt.map(x=>transactions.map(y=>if(x.intersect(y)==x) 1 else 0)).map(x=>x.count(_==1)).zip(tt).filter(_._1>=Sup).map(_._2)
  println("-------------k="+k+" itemsets")
   v1.foreach(println)
  }
  }
}