package es.upm.fi.oeg.siq.data

import java.util.TreeMap
import java.lang.Math
import scala.collection.immutable.List
import scala.collection.JavaConversions._
import scala.math._
import scala.collection.mutable.ArrayBuffer
 
case class Distribution(val name:String,val gap:Double,
                   val period:Double,val interval:Double,val typeData:String, val error:Double) extends Means {
  val values:TreeMap[Double,Double] = new TreeMap
  (-Pi/2 to Pi/2 by gap).foreach(angle=>values.put(tan(angle),0))
  val symbols=new ArrayBuffer[Char]
  val symbolDir=(values.keySet zip (65 to 102).map(_.toChar)).toMap
  
  def add(value:Double):Unit = add(value,1)
  
  def add(value:Double, weight:Double):Unit={
    val entry =	values.floorEntry(value)
	values.put(entry.getKey(), entry.getValue()+weight) 
	symbols+=symbolDir(entry.getKey)
  }

  def +(d:Distribution)={
    val dd = new Distribution("generated",gap,period,interval,this.typeData,this.error)
    val vv = d.values.entrySet zip values.entrySet
    vv.foreach(a=>dd.add(a._1.getKey,a._1.getValue+a._2.getValue))  
    dd
  }
  def percentages={
   val sum = values.values.sum
   //values.values.map(_/sum) }
   //values.values.map(a=>a)
   distrib
  }
   
  
  def cumulative={
    (1 until values.size) map(i=>percentages.take(i).sum) }
   
  lazy val distrib={/*
    var p=collection.mutable.Map()++symbolDir.values.map(s=>symbolDir.values.map(s2=>List(s,s2).mkString)).flatten
      .map(s=>symbolDir.values.map(s2=>List(s,s2).mkString)).flatten.map(s=>s->0).toMap
    var i=0
    
    symbols.foreach{s=>
      if (i+3<symbols.size){
      val k=List(s,symbols(i+1),symbols(i+2)).mkString
      p(k)=p(k)+1}
      i+=1
    }
    p.values.map(_/period)*/
    symbols.groupBy(a=>a).map(a=>(a._1,a._2.size/period)).values
  }
  
  def euclidean(d:Distribution)={
    val vals = percentages
    val dvals = d.percentages
    sqrt(vals.zip(dvals) map{case (a,b)=>pow(a-b,2)} sum)  
    //sqrt(values.values.zip(d.values.values) map{case (a,b)=>pow(a-b,2)} sum)
    }
  
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
  
  def emd(d:Distribution)={
    cumulative.zip(d.cumulative).map{case (a,b)=>abs(a-b)} sum }
  
  def cosine(d:Distribution)={
    val vals = percentages
    val dvals = d.percentages
    vals.zip(dvals).map(a=> a._1*a._2).sum/
    		sqrt(vals.map(pow(_,2)).sum)*sqrt(dvals.map(pow(_,2)).sum) }
  
  
  override val companion=Distribution
  override def distance(item:Means):Double={item match { 
    case EmptyMeans() => Double.MaxValue
    case Distribution(n,p,i,g,t,e) => euclidean(item.asInstanceOf[Distribution])
  }
  }
  override def mkString:String=name//values.values.mkString// percentages.mkString
}

object Distribution extends MeansUtils{
  def apply(name:String,angleFrac:Int,period:Double,interval:Double,values:Array[Double],typeData:String,error:Double):Distribution={
    val angle = Pi/angleFrac
    apply(name,angle,period,interval,values,typeData,error)
  }  
  def apply(name:String,angle:Double,period:Double,interval:Double,values:Array[Double],typeData:String,error:Double)={
    val d = new Distribution(name,angle,period,interval,typeData,error)
    //d.symbols+=
    (-Pi/2 to Pi/2 by angle).zipWithIndex.
        foreach{case (a,i)=> d.add(tan(a),values(i))}
    d
  }
  def sum(l:List[List[Double]])=    
    (0 until l.head.length) map(i=>l.map(li=>li.get(i)).sum)
  
  def means(list:Iterable[Means])={
   val red=list.reduceLeft((a,b)
      =>a.asInstanceOf[Distribution]+b.asInstanceOf[Distribution]).asInstanceOf[Distribution]
   val d=list.first.asInstanceOf[Distribution]   
   val lala=red.values.values.map(_/d.values.size).toArray 
   val dd=Distribution("mean",d.gap,d.period,d.interval,red.values.values.map(_/list.size).toArray,d.typeData,d.error)
   //println("meannnn: "+dd.values.values.toString)
   dd
  }
   
}