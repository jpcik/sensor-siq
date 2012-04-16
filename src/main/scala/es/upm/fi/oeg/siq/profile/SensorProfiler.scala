package es.upm.fi.oeg.siq.profile

import es.upm.fi.oeg.siq.data._
import math._
import au.com.bytecode.opencsv.CSVWriter
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVReader
import scala.io.Source
import scala.collection.mutable.HashMap
import es.upm.fi.oeg.siq.data.compr.PwlhSummary
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.VectorIterator
import es.upm.fi.oeg.siq.data.compr.PwlhSummary$
import java.util.TreeMap
import java.io.File
import es.upm.fi.oeg.siq.data.cluster.KMeans
import com.weiglewilczek.slf4s.Logging

object SensorProfiler extends Logging{
  
  val ds:SensorDataset=AemetDataset//new MixedDataset(AemetDataset,PachubeDataset)//AemetDataset//SwissExDataset
  val testds=AemetDataset
  println (util.Random.shuffle((1 to 4).toList))
  val ranges=ds.props.map{p=>
    val div=9
    val list=util.Random.shuffle((1 to p._2).toList)    
    val gsize=if(list.size%div==0) list.size/div else (list.size/div+1)
    
    p._3 -> list.grouped(gsize).toList}.toMap // list.tak(list.take(p._2/2),list.drop(p._2/2))}.toMap
  //println("sizing "+ranges.get("humidity").get.mkString)
  val truth = new HashMap[String,Distribution]
  val alldists=new ArrayBuffer[Distribution]          
      
  def addTruth(name:String,d:Distribution){//prop:String,buckets:Int,angleFrac:Int,d:Distribution){
    val key = name//prop+buckets+"-"+angleFrac
    truth.get(key) match {
      case Some(dis)=> println(key); println(dis.percentages); truth.put(key,dis+d); //println(dis.percentages)
      case None => truth.put(key,d) }
  }
  
  def filterTruth(buckets:Int,angleFrac:Int)={
    val r = ("""(\D+)"""+buckets+"""-"""+angleFrac).r
    truth.filter(a=>a._1 match {case r(s) => true; case _ => false })
  }
  
  def compare(buckets:Int,angleFrac:Int,d:Distribution)={
    val fact=4d
    val cosito = //filterTruth(buckets,angleFrac).map(b=>{
      filter(buckets,angleFrac,d.name).map(b=>{
      val s = (b.euclidean(d),b.typeData,b.name,b.error)
      s
    }).filter(_._1<60).filter(_._4.abs<5).toList.sortBy(t=>(t._1)).take(7)
    cosito.foreach(t=>println(t+ds.getTags(t._3.drop(7).split('-')(0)).mkString))
    val gr=cosito.groupBy(s=>s._2).map{g=>
      //(g._1,g._2.size+(1/g._2.min._1)*4*g._2.size/(ds.propsMap.get(g._1).get._2/fact),g._2)
      //(g._1,g._2.size/(g._2.map(_._1).sum/g._2.size)*pow(1d/(ds.propsMap.get(g._1).get._2),1),g._2)
      (g._1,g._2.size/(g._2.map(_._1).sum/g._2.size),g._2)
      
    }.toList.sortBy(_._2).reverse
    println("sizeo"+gr.size)
    gr.foreach(println(_))
    var mx=0
    if (gr.isEmpty)
      List()
    else
      gr.first._3.take(1)
    
  }
  
  def loadDistributions(dss:SensorDataset,name:String,i:String)={
    val path=dss.distPath(name,i)
    logger.trace(path)//+i+".csv")
    val f=new File(path)//+i+".csv")
    val s=Source.fromFile(f).bufferedReader.readLine()
    if (f.exists() && s!=null)
    {
    val period=s.split(';')(3).drop(8).toDouble
    val interval=s.split(';')(2).drop(9).toDouble
    val csv = new CSVReader(Source.fromFile(path).bufferedReader,',',CSVReader.DEFAULT_QUOTE_CHARACTER)
    var dname =""
    var bck=0
    var ang=0
    var values:Array[Double]=null
    val dists=new  ArrayBuffer[(Distribution, Int,Int)] 
    Stream.continually(csv.readNext()).takeWhile(_!=null).foreach{a=>  //filter(_(0).equals("dist")).map(a=>{
      if (a(0).equals("dist")){
        dname=name+i+"-"+a(1)+"-"+a(2)
        bck=a(1).toInt
        ang=a(2).toInt
        values=a(3).dropRight(1).drop(1).split(",").map(_.toDouble/(period/1d))
      }
      else if (a(0).startsWith("interval")){
        val err=a(0).split(';').last.toDouble
        dists+=((Distribution(dname,ds.tangents,period,interval,values,name,err),bck,ang))
      }
      else if (a(0).startsWith(";maxCount")){}
      else {
        val symbs=a(0)
        dists.last._1.symbols.++=(symbs.toList)
      }
      /*val d=Distribution(name+i+"-"+a(1)+"-"+a(2), a(2).toInt,period,interval, 
          a(3).dropRight(1).drop(1).split(",").map(_.toDouble/period),name,)
      (d,a(1).toInt,a(2).toInt)  })*/
    }
    //println("sizo"+dists.length)
    Some(dists)
    }
    else None
  }
    
  def filter(b:Int,a:Int,name:String)={
    logger.trace(b+";"+a+";"+name)
    //val r = ("""(\w)-"""+b+"""-"""+a).r
    //val r = ("""(\S+)-"""+b+"""-"""+a).r
    val r = ("""(\S+)-"""+"""(\d)+"""+"""-"""+a).r
    //val exact = (name).r
    //truth.filter(a=>a._1 match {case r(s) => true; case _ => false })
    alldists.filterNot(_.name.equals(name))
    /*.filter(d=>d.name match {
    
      case r(s) => true; case _ =>  println(d.name); false
    })*/
  }
  def load(group:Int) = {
    ds.props foreach {prop=>
      val indexes=if (group == -1) (1 to prop._2) 
      else if (group<ranges(prop._3).size){
        println(ranges(prop._3))
        println(ranges(prop._3).splitAt(group))
        val splitted=ranges(prop._3).splitAt(group)
        val (start,_::end)=splitted
        (start:::end).reduceLeft(_:::_)
      }
      else {
        ranges(prop._3).reduceLeft(_:::_)
      }
      indexes.foreach(i=>{
      //(1 to prop._2).foreach(i=>{
        val ll=loadDistributions(ds,prop._3,ds.getCode(prop._3,i))
        if (ll.isDefined)
        ll.get.foreach{
          case (d,buckets,angleFrac)=> {             
          println(d.name+";int "+ d.interval)
          //if (buckets==buck(d.interval)){
          println(d.name+";"+ d.values.values)
          alldists+=d}
          //}
          }
      })
    }
    
    if (false){
   val km=new KMeans[Distribution](12,alldists)
    var c1=km.clusters
    c1.foreach(a=>println("initial cluster "+a.mkString))
    val clust=Stream.continually{km.cluster;km.clusters.foreach(a=>println(a.mkString)); km.clusters}.take(10).last
    clust.foreach(a=> println(a.set.size+"--- "+a.mkString))
    }
  }
  
  def buck(interval:Double)=
  if (interval<0.5) 500 else if ( interval>=0.5 && interval <4) 50 else if ( interval>=4 && interval <15) 5 else 2
  //if (interval<1) 500 else if ( interval>=1 && interval <5) 50 else if ( interval>=5 && interval <15) 10 else 5
      
  def findmatch(group:Int)={
    val testProps=testds.props.filter(_._3.equals("temperature"))//Array(testds.propsMap("temperature"))//humidity,lysimeter,moisture,radiation,snowheight,co2,temp,voltage, windspeed,winddir)
    val buckets = 5
    val angleFrac = 8
    var tot = 0
    var fp = 0d
    var fn = 0d
    var tp = 0d
    var tn = 0d
          
    val matched=testProps.map{prop=>
      val propMat = new HashMap[String,Int]
      ds.props.foreach(a=>propMat.put(a._3,0))
      val indexes=if (group == -1) (1 to prop._2)
      else if (ranges(prop._3).size<=group) List()
      else ranges(prop._3)(group)
      indexes.foreach{i=>
      //(1 to prop._2).foreach{i=>
        //println("loading "+resultPath("all")+prop._1+i)
        val disto=loadDistributions(testds,prop._3,testds.getCode(prop._3,i))  
        if (disto.isDefined){
          
          val distlist=disto.get.filter{
            case (d,b,af)=> (b==buck(d.interval)  && af==angleFrac)}
          if (!distlist.isEmpty){  
            //val dist=distlist.first._1

           distlist.foreach{d=>
             val dist=d._1
           println("Comparing "+dist.name)
           println("tags "+testds.getTags(testds.getCode(prop._3,i)).map(t=>t+" ").mkString)
           val cmp = compare(buckets,angleFrac,dist)
           if (!cmp.isEmpty)
             propMat.put(cmp.first._2,propMat.get(cmp.first._2).get+1)
           cmp.foreach(s=>println(s._3+"("+s._1+")"))
           println
           if (cmp.exists(a=>a._2.equals(dist.typeData))) {
             tp+=1
             fp+=cmp.length-1
           }
           else if (cmp.isEmpty)
             fn+=1
           else {
             //fn+=1
             fp+=cmp.length
           }
             
           tot+=1
            }
      //println(d.percentages)
      //println(summ.toString())
    }}
    }
    println("results "+propMat) 
    (prop._3,propMat)
    }
    println("total: "+tot)
    println("false positives: "+fp)
    println("false negatives: "+fn)
    println("true positives: "+tp)
    val pr=(tp/(tp+fp))
    val rec=tp/(tp+fn)
    println("precision: "+pr)
    println("recall: "+rec)
    
    matched.map(m=>(m._1,pr,rec,m._2))

  }
  
  def main(args: Array[String]): Unit = {
    //export
    //loadshow
    //DataGenerator.generate
    //PachubeDataset.analyzeInterval
    //PachubeDataset.analyzeInvalidData
    
    val dd=(0 to 8).map{i=>
      alldists.clear
      load(i)
      findmatch(i)      
    //truth.foreach(a=>println(a._1+" "+ a._2.percentages))
    }
    
    
    dd.foreach{p=>
      p.foreach{l=>
       println(l._1+":::"+l._2+";"+l._3+";"+l._4) 
      }      
    }
    
  }

}