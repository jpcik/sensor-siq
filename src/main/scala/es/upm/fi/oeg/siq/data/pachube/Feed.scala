package es.upm.fi.oeg.siq.data.pachube

 
import com.sun.jersey.api.client.Client
import com.sun.jersey.core.util.MultivaluedMapImpl
import com.sun.jersey.api.client.WebResource
import org.joda.time.DateTime
import java.io.InputStream
import scala.io._
import java.io.FileWriter
import java.io.PrintStream

class Feed(val feedId:Int) {
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
}

object DataFormat extends Enumeration("csv","json","xml","rdf"){
  type DataFormat = Value
  val Csv,Json,Xml,Rdf = Value
 }

class Datastream(feed:Feed,val id:Int){
  private val c:Client = Client.create
  var streamResource:WebResource = null
  
  def resource = {
    if(streamResource==null) {      
      streamResource = c.resource("http://api.pachube.com/v2/feeds/%d/datastreams/%d" format(feed.feedId,id))
    }	
    streamResource }
  
  def getData(start:DateTime,format:DataFormat.Value)={
    val params = new MultivaluedMapImpl()
    params.add("key", "c9c8f31503188d651636301d3deda6b295862803f93c2e8034750798b1257f05")
    params.add("format",format)
    params.add("start",start)
    params.add("duration","1days")
    //println(start)
    val data = resource.queryParams(params).get(classOf[InputStream])
    data
    
    //val output = Resourc
    //println(data)
  }
  
}



