package es.upm.fi.oeg.siq.data

import scala.io.Source
import scala.math._
import scala.collection.mutable.ListBuffer
import org.scala_tools.time.Imports._
import org.scalatest.prop.Checkers
import org.scalatest.junit.JUnitRunner
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import org.junit.Before
import org.junit.Ignore
import org.junit.runner.RunWith
import es.upm.fi.oeg.siq.data.pachube.Feed
import es.upm.fi.oeg.siq.data.pachube.Datastream
import es.upm.fi.oeg.siq.data.pachube.DataFormat
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import java.io.FileWriter
import java.io.InputStream
import es.upm.fi.oeg.siq.data._
import es.upm.fi.oeg.siq.profile.SensorProfiler._
import scala.collection.mutable.ArrayBuffer


//import org.joda.time._


//@RunWith(classOf[JUnitRunner])
class SeriesTest  extends JUnitSuite with ShouldMatchersForJUnit with Checkers {
  var sb: StringBuilder = _
  var lb: ListBuffer[String] = _

  @Before def initialize() {
    System.out.println("papas lapas")
    sb = new StringBuilder("ScalaTest is ")
    lb = new ListBuffer[String]
  }
  
  @Test@Ignore
  def testPachube(){
    val f = new Feed(6895)
    val d = new Datastream(f,3)
    val dt = new DateTime(2011,1,1,0,0,0,0,DateTimeZone.UTC)
    (1 to 90) foreach(a=>export(d,"c:/pachube"+f.feedId+"_"+d.id+".csv",dt+a.days))   
  }
  
  def export(d:Datastream,filename:String,dt:DateTime)={
    val data = d getData(dt,DataFormat.Csv)
    val it = Source.fromInputStream(data) getLines
    val out= new FileWriter(filename,true)
    it.foreach(a=>out.write(a+"\n"))
    out.close
    data.close
  }

  @Test
  def verifyEasy() { // Uses ScalaTest assertions
    sb.append("easy!")
    assert(sb.toString === "ScalaTest is easy!")
    assert(lb.isEmpty)
    lb += "sweet"
    intercept[StringIndexOutOfBoundsException] {
      "concise".charAt(-1)
    }
  }
  
  @Test
  def testSum() {
    val s:List[List[Double]] = List( List(1,2,3),List(3,3,3))
    println(Distribution.sum(s))
  }
  
  @Test
  def testCompare(){
    val strs = new ArrayBuffer[String]
    val percs = new ArrayBuffer[List[Double]]
    (2 to 2).foreach(i=>{
    val summ = PwlhSummary(new CsvSeries(0,dataPath+radiation._1+i+".csv",0,0),2)
    
    //println(summ.data.toString())
    
    summ.pwlh
    val d = summ.generateDistribution(Pi/12,20,false)
    percs+=d.percentages.toList
    strs+=(summ.toString) })
    strs.foreach(a=>println("thingy:"+a))
    percs.foreach(println)
    /*
    load
    val sp = compare(2,12,d)
    println(d.values.values)
    println(summ.toString)*/
  }

  @Test
  def testLinearAppr(){
    val summ = new LinearSummary(new CsvSeries(0,dataPath+temp._1+"14.csv",10,0),2)
    
    println(summ.data.toString())
    summ.linearApprox
    val d = summ.generateDistribution(Pi/12,20)
    println(d.values.values)
  }    


}