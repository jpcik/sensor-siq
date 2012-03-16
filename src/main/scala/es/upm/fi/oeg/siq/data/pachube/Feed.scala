package es.upm.fi.oeg.siq.data.pachube

import com.sun.jersey.api.client.Client
import com.sun.jersey.core.util.MultivaluedMapImpl
import com.sun.jersey.api.client.WebResource
import org.joda.time.DateTime
import java.io.InputStream
import scala.io._
import java.io.FileWriter
import java.io.PrintStream
import com.google.gson.Gson
import org.joda.time.DateTimeZone
import org.scala_tools.time.Imports._
import com.sun.jersey.api.client.UniformInterfaceException


object DataFormat extends Enumeration("csv","json","xml","rdf"){
  type DataFormat = Value
  val Csv,Json,Xml,Rdf = Value
 }

class Feed(val id:Int, val status:String, val title:String, val tags:Array[String],
    val datastreams:Array[Datastream]) {
  def export(path:String){
    println("Feed: "+id+" tags: "+ (if (tags!=null) tags.toList else "empty"))
    datastreams.foreach{d=>
      println("Ds: "+d.id+" tags: "+(if (d.tags!=null) d.tags.toList else "empty"))
      val start=new DateTime(2011,11,1,0,0,0,0,DateTimeZone.UTC)
      val end=new DateTime(2012,2,28,0,0,0,0,DateTimeZone.UTC)
      val fw = new FileWriter(path+id+"_"+d.id+".csv",true)
      (0 to 24).map(_*5).map(i=>start+i.days).foreach{date=>
      	val s=d.getData(date,null,DataFormat.Csv,id.toString)
      	if (s.length>5){
      	fw.write(s)
      	fw.write("\r\n")}
      }
      /*
      */
      fw.close
    }
  }
}

object Feed{
  def apply(id:Int,ds:Array[String])=
    new Feed(id,"","",Array(),ds.map(did=>new Datastream(null,did,Array(),"")))
  def apply(id:Int,ds:Map[String,(String,Array[String])])=
    new Feed(id,"","",Array(),ds.map(d=>new Datastream(null,d._1,d._2._2,d._2._1)).toArray)
  def getData={
    val c = Client.create()
    val webResource = c.resource("http://api.pachube.com/v2/feeds/6895/datastreams/3")
    
	val queryParams = new MultivaluedMapImpl()
	queryParams.add("key", "c9c8f31503188d651636301d3deda6b295862803f93c2e8034750798b1257f05")
	queryParams.add("format","csv")
		   //queryParams.add("param2", "val2");
	val s:String=""	
	val res = webResource.queryParams(queryParams).get(s.getClass());
    println(res)
  }
  def search(tag:String)={
    val c = Client.create()
    val webResource = c.resource("http://api.pachube.com/v2/feeds")
    
	val queryParams = new MultivaluedMapImpl()
	queryParams.add("key", "c9c8f31503188d651636301d3deda6b295862803f93c2e8034750798b1257f05")
	queryParams.add("tag",tag)
	queryParams.add("order","retrieved_at")
    val res = webResource.queryParams(queryParams).get(classOf[String]);
    val g=new Gson()
    val col = g.fromJson(res, classOf[ResultsPage]);
    
    println("feeds:"+ col.results.length)
    col.results.take(25).foreach{f=>
      println("feed: "+f.id+":"+f.datastreams.map{d=>
        println(d.tags)
        val tags=if (d.tags!=null)
          d.tags.reduceLeft((a,b)=>a+","+b)
          else ""
        d.id+"("+tags+");"}
        .reduceLeft(_+_))
      //f.export("g:/doc/semint/SensorAnnotation/pachube/")
    }
  }
  
  def exportData(feedId:Int,datastreamIds:Array[String]){
    val feed=Feed(feedId,datastreamIds)
    feed.export("g:/doc/semint/SensorAnnotation/pachube/")
  }

  def exportData(is:InputStream){
    val feeds=Source.fromInputStream(is).getLines.map{line=>
      val l=line.split(':')
      val id=l(0)
      Feed(id.toInt,l(1).split(';').map(a=>a.split('(')(0)))
      
    }
    feeds.find(f=>f.id==14769).foreach(feed=>
      //println(feed.datastreams.map(_.id).mkString))
      feed.export("g:/doc/semint/SensorAnnotation/benchmark/pachube/"))
      

  }
}

 case class Start(name:String,atts: Map[Symbol,Any]=null)  {
    //def this()= this(null)
    //def this(atts: Map[Symbol,Any])= this (atts)
   // val content: HashMap[Symbol,Any]
   
  }
 case class GeneralEvent(name: String, atts: Map[Symbol,Any]=null)
 
  object Start{
   //def apply()=Start("papa")
   //def apply(atts:Map[Symbol,Any])=new Start(atts)
 }
 
object Print{
  def apply(s:String)=s
}

class ResultsPage(val totalResults:Int,val itemsPerPage:Int,val startIndex:Int,
    val results:Array[Feed]){
 val s=Start("dsf")
 GeneralEvent("fsdfs")
}

 


class Datastream(feed:Feed,val id:String,val tags:Array[String],val typeData:String){
  private lazy val c:Client = Client.create
  //var streamResource:WebResource = null
  
  def resource(feedId:String) = //{
    //if(streamResource==null) {      
      //streamResource = 
        c.resource("http://api.pachube.com/v2/feeds/%s/datastreams/%s" format(feedId,id))
    //}	
    //streamResource }
  
  def getData(start:DateTime,end:DateTime,format:DataFormat.Value,feedId:String)={
    val params = new MultivaluedMapImpl()
    params.add("key", "c9c8f31503188d651636301d3deda6b295862803f93c2e8034750798b1257f05")
    params.add("format",format)
    //params.add("end",end)
    params.add("start",start)
    params.add("duration","5days")
    params.add("interval","300")
    params.add("per_page","1000")    
    println("request "+id)
    println("URL "+resource(feedId).queryParams(params).getURI().toString())
    var stop=false
    var data =""
      while (!stop)
    try {data=resource(feedId).queryParams(params).get(classOf[String])
      stop=true
    }
    catch {case e:UniformInterfaceException=>println("retry ")}
    println("data got")
    data
    
  }
  
}



