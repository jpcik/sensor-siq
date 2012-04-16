package es.upm.fi.oeg.siq.profile
import math._
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import es.upm.fi.oeg.siq.data.CsvSeries
import es.upm.fi.oeg.siq.data.AemetSeries
import es.upm.fi.oeg.siq.data.compr.PwlhSummary
import au.com.bytecode.opencsv.CSVReader
import scala.io.Source
import java.util.TreeMap
import es.upm.fi.oeg.siq.data.compr.LinearSummary

object DataGenerator {
  val rootPath="g:/doc/semint/SensorAnnotation/"
  //def dataPath(set:Int)=rootPath+"benchmark_"+set+"/"
  //def resultPath(set:String)=rootPath+"result_"+set+"/"
  val ds:SensorDataset=AemetDataset
   
  

  def generate = {
    //val bench="aemet"
    ds.props.foreach(prop =>
	  (1 to prop._2).foreach{i =>      
        val code=ds.getCode(prop._3,i)
	  //(19 to 19).foreach(i =>{  
        //val typeData=AemetSeries.obsTypes.get(prop._1).get._1
//if (code.equals("45022_winddir")){
	    val w = new CSVWriter(new FileWriter(ds.distPath(prop._3,code)),',',CSVReader.DEFAULT_QUOTE_CHARACTER)
	    Array(500,100,50,10,5,2,1).foreach(buckets=> {
	      try{
	      val d = new CsvSeries(0,ds.dataPath(prop._3,code),0,0,prop._3)
	      println(d.toString)
	      val dataDay=60*24/d.interval
	      if (dataDay>=buckets*8 && dataDay<=buckets*100){
	        val data=new CsvSeries(0,ds.dataPath(prop._3,code),40000/dataDay,0,prop._3)
	      w.writeNext(Array(data.toString))
	      Array(8).foreach{angle=>	  
    	        	 
	        println(prop._1+code+".csv")

	        val pwl2 = new LinearSummary(data,buckets*data.period/data.maxCount)
	        	pwl2.linearApprox
	        	//pwl2.export(buckets,resultPath("data")+prop._1+code+"_"+buckets)
	        	val dis=pwl2.generateDistribution(ds.tangents,30)
	        	
	        	//println(dis.values.values)
	            w.writeNext(Array("dist",buckets.toString,angle.toString,""+dis.values.values))
	            w.writeNext(Array(pwl2.toString))
	            w.writeNext(Array(dis.symbols.toList.mkString))
	      //}
	      }
	      
	      }
	      
	      } catch{
	        case e:Exception=>println("Exception: "+e.getMessage())
	      }
	       
	    })
	    w.close
//}
	  }
    )
  }
  
  
  def loadshow{
    val interv=30
    val csv = new CSVReader(Source.fromFile(ds.distPath("","data")+"windspeed7_10_data.csv").bufferedReader,
        ';',CSVReader.DEFAULT_QUOTE_CHARACTER)
    val points= Stream.continually(csv.readNext()).takeWhile(_!=null).map(a=>(a(0).toDouble,a(1).toDouble))
    var lastPoint=(0d,0d)
    var i=0;
    points.foreach{p=>
      val slope=(p._2-lastPoint._2)/(p._1-lastPoint._1)
      val hops=((p._1-i)/interv).toInt
      (1 to hops).foreach{h=>
        println((i+h*interv)+";"+ ((i+h*interv-lastPoint._1)*slope+lastPoint._2))
      }    
      i=i+hops*interv
      lastPoint=(p._1,p._2)      
    }
    //(0 to 14400).map(i=>)

  }
      
    
  def resample(s:CsvSeries,interval:Double){
    val w = new CSVWriter(new FileWriter(s.filename+"_resampled"),',',CSVWriter.NO_QUOTE_CHARACTER)
    w.writeNext(Array("###"+(interval*60).toInt))
    /*
    s.resample(interval).stream.foreach{h=>
      val avg=h.points.map(_.y).sum/h.points.length
      w.writeNext(Array(avg.toString))
    }*/
    w.close
  } 
  
  def split(s:CsvSeries,newPeriod:Int){
    if (newPeriod<s.period){
    //val w = new CSVWriter(new FileWriter(s.filename+"_trimmed"),',',CSVWriter.NO_QUOTE_CHARACTER)
    //w.writeNext(Array("###"+s.datainterval.toInt*60))
    val size=(24*60*newPeriod/s.datainterval).toInt
    s.stream.grouped(size).zipWithIndex.foreach{h=>      
      val w = new CSVWriter(new FileWriter(s.filename+"_trimmed"+h._2),',',CSVWriter.NO_QUOTE_CHARACTER)
      w.writeNext(Array("###"+(s.datainterval*60).toInt))
      h._1.foreach(d=>w.writeNext(Array(d.y.toString)))
      w.close
    }
    }
  }
  
  //1b p d
  //1d m c
  def export={
    val values:TreeMap[Double,Int] = new TreeMap
    (-Pi/2 to Pi/2 by Pi/12).zipWithIndex.foreach{angle => values.put(tan(angle._1),angle._2)}
    val taps=Array(ds.propsMap("radiation"))
    ds.props.foreach{prop=>
      (1 to prop._2).foreach{i=>
      //(1 to 1).foreach{i=>
        //println("dataset "+prop._1+i)
		val data = new CsvSeries(0,prop._1+i+".csv",0,0,prop._3)
		
		//if ( data.datainterval<1 ){
		//if (data.datainterval>=(1) && data.datainterval<=5 && data.period>50){  
		if (data.datainterval>(5) && data.datainterval<30){
		//if (data.datainterval>=(30) ){
		  println("dataset: "+prop._1+i+" period: "+data.period )
		  //resample(data,5)		  
		  //split(data,200)
		}
		if (data.datainterval<= -5){
		  println("compression "+96*data.period/data.maxCount)
		val comp =  new PwlhSummary(data,96*data.period/data.maxCount)
	    comp.pwlh
	    val d = comp.generateDistribution(Array(),20,false)
		//val d = new Distribution(prop._3,12,prop._3) 
		//val idxs= comp.buckets.map(b=>values.floorEntry{b.regression(b.h); b.lr.getSlope*20}).map(e=>e.getValue())
		//println(idxs.foldLeft(prop._3+i+": ")((e,d)=>e+","+d))
		println}
	    //println("perc "+d.percentages)
      }
    }
  }


}