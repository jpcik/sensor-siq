package es.upm.fi.oeg.siq.data
import java.io.File
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import com.weiglewilczek.slf4s.Logging
import java.io.FileWriter

class AemetSeries {

}

object AemetSeries extends AemetSeries with Logging{
  val codes=loadCodes
  val record=codes.map(c=>(c,new ArrayBuffer[Measurements])).toMap
  val obsProperties=Array("TA","TS","HR","PRES","VV10m","VMAX10m","DV10m","DMAX10m","PREC","BAT","NIEVE")
  type Measurements = (Long,Seq[Option[Double]])
  val types=Array("temperature","soiltemp","humidity","pressure","windspeed","windspeedmax",
      "winddir","winddirmax","precipitation","battery","snow")
  val obsTypes=(obsProperties zip types).zipWithIndex.map(p=>p._1._1->(p._1._2,p._2+1)).toMap
      
  def loadCodesPerType(name:String):Array[String]={
    logger.info("codes for: "+name)
    val s=Source.fromInputStream(getClass.getResourceAsStream("/aemet/"+name))
    s.getLines.map{line=>
      line.split(',')(0).drop(1)
    }.toArray
  }
  def loadAllCodesPerType={
    obsTypes.map(ot=>ot._1->loadCodesPerType(ot._2._1)).toMap
  } 
  
  def loadCodes={
    val s=Source.fromInputStream(getClass.getResourceAsStream("/aemet.properties"))
    s.getLines.map{line=>
      val fields=line.split(';')
      if (fields(1).size==0) fields(0)
      else fields(1)
    }.toArray    
  }
  def generateAllSeries(path:String,startDate:Long){
    val maindir=new File(path)
    maindir.list.sorted.foreach{d=>
      val date=d.substring(0,8).toLong
      if (date>=startDate)
    	generateOneDay(maindir.getPath+"/",date)
    }
  }
  
  def generateOneDay(path:String,date:Long){
    logger.info("Generating Day "+date)
    val dayDir= new File(path+date+"_diezminutales/")
    dayDir.listFiles.sorted.foreach{d=>
      generate10mins(dayDir.getPath+"/",d.getName.substring(0,12).toLong)
    }
    logger.info("Export records")
    exportRecord("g:/doc/semint/SensorAnnotation/aemet/stationstest/")
    flushRecord
  }
  def flushRecord{
    record.foreach(r=>r._2.clear)  
  }
  def generate10mins(path:String,datetime:Long){
    logger.info("Datetime: "+datetime)
    val s=Source.fromFile(path+datetime+"_datos.csv")
    s.getLines.foreach{l=>
      val (code,obs)=extract(l.split(','))
      val rec=record.get(code)
      if (rec.isDefined) rec.get+=((datetime,obs))
      else println("Missing code:"+ code)
    }
  }
  
  def exportRecord(path:String){
    record.foreach{r=>
      val fw=new FileWriter(path+r._1+".csv",true)
      r._2.foreach(data=>fw.write(data._1+
          data._2.map(o=>";"+o.getOrElse(null)).mkString+"\r\n"))
      fw.close
    }
  }
  
  def extract(s:Array[String])={
    val code=s(0)
    val observations=new ArrayBuffer[Option[Double]](obsProperties.length)
    obsProperties.foreach(p=>observations.+=(None))
    s.drop(6).foreach{field =>
      val fields=field.split('=')      
      val i=obsProperties.findIndexOf(_.equals(fields(0)))
      if (i>=0) observations(i)=
        try{Some(fields(1).toDouble)}
        catch {case e:NumberFormatException=>None}
    }
    (code,observations)
  }

  def analyze(path:String,code:String)={
    logger.info("Analyze "+code)
    //val br=Source.fromFile(path+code+".csv").bufferedReader
    val nulls=new ArrayBuffer[Int]
    val zeros=new ArrayBuffer[Int]
    val values=new ArrayBuffer[Int]
    
    obsProperties.foreach{o=>nulls.+=(0);zeros.+=(0);values.+=(0)}
    //Stream.continually(br.readLine).takeWhile(_!=null).foreach{line=>
    Source.fromFile(path+code+".csv").getLines.foreach{line=>
      val obs=line.split(';').drop(1)
      
      nulls.indices.foreach{i=>
        if (obs(i).equals("null")) nulls(i)=nulls(i)+1
        else if (obs(i).equals("0.0")) zeros(i)=zeros(i)+1
        else values(i)=values(i)+1
      }          
    }
    (code,nulls,zeros,values)    
  }
  def analyzeAll(path:String){
    val analyzed=codes.map{code=>
      analyze(path,code)}
    
    val perProperty=obsProperties.indices.map{i=>          
      analyzed.filter(a=>a._4(i)>0).map(a=>(a._1,a._4(i),a._3(i),a._2(i)))
    }.zipWithIndex.map(l=>(obsProperties(l._2),l._1))
    
    perProperty.foreach{a=>
      println(a._1)
      a._2.foreach(d=>println(d))
    }
  }
   
  def formatSeries(path:String,code:String,typeData:String){
    logger.info("Formatting "+typeData+ "::"+code)
    val s=Source.fromFile(path+"stations/"+code+".csv")
    val fw= new FileWriter(path+"dataset/"+obsTypes(typeData)._1+"/"+code+".csv")
    fw.write(600+"\r\n")
    val br=s.bufferedReader
    Stream.continually(br.readLine).takeWhile(_!=null).foreach{line=>
    //s.getLines.foreach{line=>
      try {
      val obs=line.split(';')(obsTypes(typeData)._2).toDouble
      fw.write(obs+"\r\n") 
      }
      catch {case e:NumberFormatException=>Unit}
    }
    fw.close
    s.close
  }
  
  def formatAllSeries(path:String){
    loadAllCodesPerType.filter(a=>a._1.equals("TA")).foreach{ct=>
      ct._2.foreach{code=>
        formatSeries(path,code,ct._1)
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    //generate10mins("g:/doc/semint/SensorAnnotation/aemet/aemet/20110722_diezminutales/",201107220000L)
    //generateOneDay("g:/doc/semint/SensorAnnotation/aemet/aemet/",20110722)
    //generateAllSeries("g:/doc/semint/SensorAnnotation/aemet/aemet/",20120209)
    //analyze("g:/doc/semint/SensorAnnotation/aemet/stations/","3254Y")
    //formatSeries("g:/doc/semint/SensorAnnotation/aemet/","08087","TA")
    formatAllSeries("g:/doc/semint/SensorAnnotation/aemet/")
    //analyzeAll("g:/doc/semint/SensorAnnotation/aemet/stations/")
    //exportRecord("g:/doc/semint/SensorAnnotation/aemet/stations/")
    //record.get("08087").get.foreach(println(_))
  }
}