package es.upm.fi.oeg.siq.profile
import scala.io.Source
import es.upm.fi.oeg.siq.data.pachube.Feed
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.joda.time.Seconds
import scala.collection.mutable.ArrayBuffer
import java.io.FileWriter

object PachubeDataset extends SensorDataset{
  val unknown = ("unknown/",11,"unknown")//78)
  val temp = ("unknown/",16,"temperature")//78)
  val humidity = ("unknown/",13,"humidity")//78)
  val pressure = ("unknown/",11,"pressure")//78)
  val winddir = ("unknown/",3,"winddir")//78)
  val windspeed = ("unknown/",6,"windspeed")//78)
  val precipitation = ("unknown/",10,"precipitation")//78)

  val rootPath="g:/doc/semint/SensorAnnotation/"
  override val props = Array(unknown,temp,humidity,pressure,winddir,windspeed,precipitation)
  
  //override def getCode(i:Int)=i.toString
   
  private val feeds=
  {
    val fs=Source.fromInputStream(getClass.getResourceAsStream("/pachube/datastreams")).getLines.map{line=>
      val l=line.split(':')
      val id=l(0)
      val map=l(1).split(';').map{a=>
        
        val sp=a.split('#')
        val sp2=sp(1).split('(')
        (sp(0),(sp2(0),sp2(1).dropRight(1).split(',')))
      }.toMap
      Feed(id.toInt,map)      
    }.toList
    println("The size is: "+fs.length)
    fs//.filter(_.id==46126)
  }
  val codesperType=props.map(c=>c._3->loadCodes(c._3).zipWithIndex).toMap
  
  override def getCode(typeData:String,i:Int)={
    println("index:" +i)
   codesperType(typeData).find(_._2==i-1).get._1 
  }    
  override def distPath(typeData:String,code:String)=
    rootPath+"result_pachube/"+typeData+"/"+code+".csv"
    
  override def dataPath(typeData:String,code:String)=
    rootPath+"benchmark/pachube/"+code+".csv"
   
  override def getTags(code:String)={
   val ff=feeds.map(f=>f.datastreams.map(d=>(f.id+"_"+d.id,d.tags)).toList)
  ff.reduceLeft(_:::_).toMap.apply(code) 
  }
  def loadCodes={
     /*val feeds=Source.fromInputStream(getClass.getResourceAsStream("/pachube/datastreams")).getLines.map{line=>
       val l=line.split(':')
       val id=l(0)
       Feed(id.toInt,l(1).split(';').map(a=>a.split('(')(0)))      
     }*/
    val dss=feeds.map(f=>f.datastreams.map(d=>(f.id+"_"+d.id,d.typeData)).toList).reduceLeft(_:::_).groupBy(_._2)
    dss.foreach(println(_))
    dss.map(l=>(l._1,l._2.map(l2=>l2._1)))
     /*List(("unknown",feeds.map{f=>
       
       println("****************************** datasets: "+f.datastreams.size)
       f.datastreams.toList.map(d=>f.id+"_"+d.id)}.reduceLeft(_:::_)
     )).toMap*/
  }
  def analyzeInvalidData{
    loadCodes("unknown").foreach{code=>
      println("File :"+code)
      //val fw=new FileWriter(dataPath("",code).replace("pachube","pachube_new"))
      //fw.write(Source.fromFile(dataPath("",code)).getLine(0)+"\r\n")
      val dates=Source.fromFile(dataPath("",code)).getLines.drop(1).map{line=>
        val sp=line.split(',')
        val value=sp(1)
        try{
         value.toDouble
         //fw.write(sp(0)+","+value+"\r\n")
        }
        catch { case e:NumberFormatException=> 
        /*  if (value.contains(".-")) {
            if (value.startsWith("-"))
              fw.write(sp(0)+","+(value.replace(".-","."))+"\r\n")
            else fw.write(sp(0)+","+"-"+value.replace(".-",".")+"\r\n")
          } else println(value)}  */
        //formatter.parseDateTime(stTime)
        println(value)}
      }.toList
      //fw.close
    }
  }
  def analyzeInterval{
    val formatter=DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSS'Z'")
    var last=0
    val counts= ArrayBuffer(0,0,0,0,0,0,0)
    //(0 to 6).foreach(counts+=0)
    loadCodes("unknown").foreach{code=>
      println("File :"+code)
      val dates=Source.fromFile(dataPath("",code)).getLines.drop(1).map{line=>
        val stTime=line.split(',')(0)
        formatter.parseDateTime(stTime)
        //println(date)
      }.toList
      val spans=dates.grouped(2).map{d =>
        val li=d.toList
        if (li.length==2)
          Seconds.secondsBetween(li(0),li(1)).getSeconds
        else 0             
      }.toList
      counts(0)=spans.count(_<4*60)
      counts(1)=spans.count(d=> d>=4*60 && d<=7*60)
      counts(2)=spans.count(d=> d>7*60 && d<=12*60)
      counts(3)=spans.count(d=> d>12*60 && d<=17*60)
      counts(4)=spans.count(d=> d>17*60 && d<=27*60)
      counts(5)=spans.count(d=> d>27*60 && d<=32*60)
      counts(6)=spans.count(d=> d>32*60)
      
      val avg=spans.sum/spans.length//(dates.length/2d)
      
      println(code + " "+avg+ " "+counts.toString)
    }
  }
}