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
import es.upm.fi.oeg.siq.profile.SwissExDataset._
import scala.collection.mutable.ArrayBuffer
import es.upm.fi.oeg.siq.data.compr.PwlhSummary
import es.upm.fi.oeg.siq.data.compr.LinearSummary
import es.upm.fi.oeg.siq.profile.AemetDataset
import es.upm.fi.oeg.siq.profile.PachubeDataset


//@RunWith(classOf[JUnitRunner])
class SeriesTest  extends JUnitSuite with ShouldMatchersForJUnit with Checkers {
  var sb: StringBuilder = _
  var lb: ListBuffer[String] = _

  @Before def initialize() {
    System.out.println("papas lapas")
    sb = new StringBuilder("ScalaTest is ")
    lb = new ListBuffer[String]
  }
  /*
  @Test@Ignore
  def testPachube(){
    val f = new Feed(6895)
    val d = new Datastream(f,3)
    val dt = new DateTime(2011,1,1,0,0,0,0,DateTimeZone.UTC)
    (1 to 90) foreach(a=>export(d,"c:/pachube"+f.feedId+"_"+d.id+".csv",dt+a.days))   
  }*/
  
  @Test
  def loadSeries{
    val p=temp
    val i=44
    
    val t1=System.currentTimeMillis
    val s1=CsvSeries(p._1+i+".csv",p._3)
    s1.normalized.foreach(_.toString)
    val t1e=System.currentTimeMillis-t1

    val t2=System.currentTimeMillis
    val s2=CsvSeries(p._1+i+".csv",p._3)
    s2.normalized.foreach(_.toString)
    val t2e=System.currentTimeMillis-t2
    
    println(t1e+"--"+t2e)
    
    
  }
  /*
  def export(d:Datastream,filename:String,dt:DateTime)={
    val data = d getData(dt,DataFormat.Csv)
    val it = Source.fromInputStream(data) getLines
    val out= new FileWriter(filename,true)
    it.foreach(a=>out.write(a+"\n"))
    out.close
    data.close
  }*/

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
    val p = PachubeDataset.propsMap("unknown")
    val old=System.currentTimeMillis
    //val ss = new CsvSeries(0,dataPath+p._1+1+".csv",0,0,p._3)
    //ss.stream.foreach(a=>a.toString())//println("datas: "+a))
    val strs = new ArrayBuffer[String]
    val percs = new ArrayBuffer[List[Double]]
    
    //(1 to 1).foreach{j=>
      (25 to 25).foreach(i=>{
    val summ = new PwlhSummary(new CsvSeries(1,PachubeDataset.dataPath("unknown","46126_Temperature"),0,0,p._3),0.052083333333333336)
    
    //println(summ.data.toString())
    
    summ.pwlh
    val d = summ.generateDistribution(Pi/12,20,false)
    percs+=d.percentages.toList
    strs+=(summ.toString) 
    /*
    val ss = CsvSeries(dataPath+p._1+1+".csv",p._3)
    ss.stream.foreach{
      a=>println(a.left.x+";"+summ.computeValue(a.left.x))
    } */

    
    })
    //}
    println("elapsed: "+(System.currentTimeMillis-old).toString)
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
    val summ = new LinearSummary(CsvSeries(temp._1+"14.csv",humidity._3),0.2)
    
    println(summ.data.toString())
    summ.linearApprox
    val d = summ.generateDistribution(Pi/12,20)
    println(d.values.values)
  }    


}