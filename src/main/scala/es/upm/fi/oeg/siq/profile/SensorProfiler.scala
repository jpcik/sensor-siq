package es.upm.fi.oeg.siq.profile

import es.upm.fi.oeg.siq.data._
import math._
import au.com.bytecode.opencsv.CSVWriter
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVReader
import scala.io.Source
import scala.collection.mutable.HashMap
import es.upm.fi.oeg.siq.data.compr.PwlhSummary

object SensorProfiler {
  val dataPath = "g:/doc/semint/SensorAnnotation/benchmark/"
  val resultPath = "g:/doc/semint/SensorAnnotation/result/"
  val temp = ("Temperature/temperature_",70,"temperature")//78)
  val co2 = ("CO2/co2_",11,"co2")//11)
  val humidity = ("Humidity/humidity_",30,"humidity")
  val lysimeter = ("Lysimeter/Lysimeter",12,"lysimeter")
  val moisture = ("Moisture/Moisture",20,"moisture")
  val pressure = ("Pressure/pressure_",4,"pressure")
  val radiation = ("Radiation/radiation_",30,"radiation")
  val snowheight = ("Snow_height/snow_height_",4,"snowheight")
  val voltage = ("Voltage/voltage_",16,"voltage")
  val windspeed = ("Wind_speed/wind_speed_",40,"windspeed")
  val props = Array(temp,co2,humidity,lysimeter,moisture,radiation,snowheight,voltage,windspeed)
  
  val truth = new HashMap[String,Distribution]
  
  def generate = {
    props foreach (prop =>
	  (prop._2/2+1 to prop._2).foreach(i =>{
	  //(13 to 20).foreach(i =>{  
	    val w = new CSVWriter(new FileWriter(resultPath+prop._1+i+".csv"),',',CSVReader.DEFAULT_QUOTE_CHARACTER)
	    (0 to  2).map(pow(2,_).toInt).foreach(buckets=> {
	      val data = new CsvSeries(0,dataPath+prop._1+i+".csv",0,0)	
	      try{
	      val comp =  PwlhSummary(data,buckets);
	      if (comp.max_buckets<data.maxCount/4 ) {
	    	println("Bucket size: "+comp.max_buckets+". PerDay: "+buckets )
	    	comp.pwlh
	        (1 to 6).map(_*4) foreach (a=> {
	          val dis = comp.generateDistribution(Pi/a,20,false)
	          w.writeNext(Array(buckets.toString,a.toString,""+dis.percentages,comp.toString))
	        })
	      }
	      } catch{
	        case e:Exception=>println("Exception: "+e)
	      }
	    })
	    w.close
	  })
    )
  }
  
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
    filterTruth(buckets,angleFrac).foreach(b=>{
      //println(b._2.percentages) 
      //println(d.percentages)
      println(b._1+";"+b._2.euclidean(d)+";"+b._2.jeffrey(d))
          //b._2.cosine(d)+
          //";"+";"+b._2.chisquare(d)+";"+b._2.cumulEuclidean(d))
      //println(b._2.values.values) 
      //println(b._2.cumulative)
      })
  }
  
  def loadDistributions(name:String,path:String,i:Int)={
    val csv = new CSVReader(Source.fromFile(path+i+".csv").bufferedReader,',',CSVReader.DEFAULT_QUOTE_CHARACTER)
    Stream.continually(csv.readNext()).takeWhile(_!=null).map(a=>{
      val d=Distribution(name+i+a(0)+"-"+a(1), a(1).toInt, a(2).dropRight(1).drop(5).split(",").map(_.toDouble))
      (d,a(0).toInt,a(1).toInt) })
  }
    
  
  def load = {
    props foreach (prop=>
      (1 to prop._2/2).foreach(i=>{
      //(1 to 1).foreach(i=>{
        //val csv = new CSVReader(Source.fromFile(resultPath+prop._1+i+".csv").bufferedReader,',',CSVReader.DEFAULT_QUOTE_CHARACTER)
        //Stream.continually(csv.readNext()).takeWhile(_!=null).foreach(a=>{
         // val d=Distribution(prop._3+i, a(1).toInt, a(2).dropRight(1).drop(5).split(",").map(_.toDouble))
        loadDistributions(prop._3,resultPath+prop._1,i).foreach{
          case (d,buckets,angleFrac)=> addTruth(prop._3+buckets+"-"+angleFrac,d) }
         //println("dist"+d.values.values())  }
      })
    )
    
  }
  
  def main(args: Array[String]): Unit = {
    //generate
    load
    if (true){            
    truth.foreach(a=>println(a._1+" "+ a._2.percentages))
    val testProps=Array(humidity)//co2,humidity,lysimeter,moisture,radiation)
    val buckets = 2
    val angleFrac = 12
    (20 to 20).foreach{i=>
    testProps.foreach{prop=>
      val dist=loadDistributions(prop._3,resultPath+prop._1,i).filter{
        case (d,b,af)=> (b==buckets && af==angleFrac)}.first._1
      //val summ = PwlhSummary(new CsvSeries(0,dataPath+prop._1+i+".csv",0,0),buckets)
      //summ.pwlh
      //val summ = new LinearSummary(CsvSeries(dataPath+prop._1+i+".csv"),buckets)
      //summ.linearApprox
      //val d =summ.generateDistribution(Pi/angleFrac,20,false)
      //println(prop._3)
      println("Comparing "+dist.name)
      compare(buckets,angleFrac,dist)
      //println(d.percentages)
      //println(summ.toString())
    }}
    
    }
    
  }

}