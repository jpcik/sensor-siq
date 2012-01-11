package es.upm.fi.oeg.siq.data

import java.util.TreeMap
import java.lang.Math
import scala.collection.immutable.List
import scala.collection.JavaConversions._
import scala.math._
 
class Distribution(val name:String,val gap:Double) extends Means {
  val values:TreeMap[Double,Double] = new TreeMap
  (-Pi/2 to Pi/2 by gap).foreach(angle=>values.put(tan(angle),0))
  
  def add(value:Double):Unit = add(value,1)
  
  def add(value:Double, weight:Double):Unit={
    val entry =	values.floorEntry(value)
	values.put(entry.getKey(), entry.getValue()+weight) }

  def +(d:Distribution)={
    val dd = new Distribution("added",gap)
    val vv = d.values.entrySet zip values.entrySet
    vv.foreach(a=>dd.add(a._1.getKey(),a._1.getValue()+a._2.getValue()))  
    dd
  }
  def percentages={
   val sum = values.values.sum
   values.values.map(_/sum) }
  
  def cumulative={
    (1 until values.size) map(i=>percentages.take(i).sum) }
   
  def euclidean(d:Distribution)={
    val vals = percentages
    val dvals = d.percentages
    sqrt(vals.zip(dvals) map{case (a,b)=>pow(a-b,2)} sum)  }
  
  def jeffrey(d:Distribution)={
    val vals = percentages
    val dvals = d.percentages
    vals.zip(dvals) map{
      case (0,_)=>0;case(_,0)=>0
      case (a,b)=>{val m=(a+b)/2; a*log(a/m)+b*log(b/m)}} sum }
  
  def chisquare(d:Distribution)={
    val vals = percentages
    val dvals = d.percentages
    vals.zip(dvals) map{
      case (0,0)=>0
      case (a,b)=>{val m=(a+b)/2; pow(a-m,2)/m}} sum  }
  
  def cumulEuclidean(d:Distribution)={
    cumulative.zip(d.cumulative).map{case (a,b)=>abs(a-b)} sum }
  
  def cosine(d:Distribution)={
    val vals = percentages
    val dvals = d.percentages
    vals.zip(dvals).map(a=> a._1*a._2).sum/
    		sqrt(vals.map(pow(_,2)).sum)*sqrt(dvals.map(pow(_,2)).sum) }
  
  
  override val companion=Distribution
  override def distance(item:Means):Double=euclidean(item.asInstanceOf[Distribution])
  override def mkString:String=name// percentages.mkString
}

object Distribution extends MeansUtils{
  def apply(name:String,angleFrac:Int,values:Array[Double])={
    val angle = Pi/angleFrac
    val d = new Distribution(name,angle)
    (-Pi/2 to Pi/2 by angle).zipWithIndex.
      foreach{case (a,i)=> d.add(tan(a),values(i))}
    d
  }
  def sum(l:List[List[Double]])=    
    (0 until l.head.length) map(i=>l.map(li=>li.get(i)).sum)
  
  def means(list:Iterable[Means])=list.reduce((a,b)
      =>a.asInstanceOf[Distribution]+b.asInstanceOf[Distribution])
   
}