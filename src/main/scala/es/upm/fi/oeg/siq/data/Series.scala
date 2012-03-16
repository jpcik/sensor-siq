package es.upm.fi.oeg.siq.data

import au.com.bytecode.opencsv.CSVReader
import scala.io.Source
import scala.collection.JavaConversions._
import math._
import es.upm.fi.oeg.siq.data.compr.PwlhSummary

abstract class Series(index:Int,val typeData:String) {
  var max:Double=0
  var min:Double=Double.PositiveInfinity	
  var closed=false	
  def count:Int
  val datainterval=1d
  val period=0d
  val maxCount=0
  
  protected def updateMaxMin(d:Double)={
    if (d>max) max=d
	if (d<min) min=d
  }  	
  def next():Option[Point]
  def stream:Stream[Point]
}

class CsvSeries(val index:Int,val filename:String,initPeriod:Double,startVal:Int=0,typeData:String)//,val newInterval:Double) 
     extends Series(index,typeData) {
  private val r="""###(\d+)""".r
  private def s=Source.fromFile(filename)
  val lines=s.getLines.length
  
  override val datainterval=s.getLine(1) match {  
    case r(st) => {st.toDouble/60} 
    case _ =>error("Data Interval unavailable! ")
  }
  override val maxCount={
    println("lines in file: "+lines+" "+filename)
    val initcount=(initPeriod*24*60/datainterval).toInt
    if (initPeriod<=0) lines
    else if (initcount>lines) lines
    else initcount
  }
  override val period=maxCount*datainterval/(24*60)
    //val totalPeriod=maxCount*datainterval/(24*60)
    //if (initPeriod<=0)  (maxCount*datainterval/(24*60))//.toInt 
  	//else initPeriod
  
  private val csv=initCsv 
  var countValues:Int=0		
  lazy val rawData=readAll 
  val (mean,stdev)=stats
  
  private def initCsv=new CSVReader(Source.fromFile(filename).bufferedReader(),
	    ',',CSVReader.DEFAULT_QUOTE_CHARACTER,1+startVal)
  
  private def readAll={
   val c=initCsv
   var i=0
   Stream.continually{i+=1;c.readNext}.takeWhile(l=>
     l!=null && i<=maxCount ).map(line=>line(index).toDouble) 
  }
  
  private def stats={
    val sum=rawData.sum    
    val mn=sum/maxCount      
    var sumsq=0d    
    rawData.foreach{p=>
      updateMaxMin(p)
      sumsq+=pow(p-mn,2)}    
    (mn,sqrt(sumsq/maxCount))    
  }

  def pwlh{
    val summary = PwlhSummary(this,0.01)
    summary
  }

  override def count=this.countValues

  override def toString = ";maxCount "+maxCount+";interval "+datainterval+
  		"; period "+period+ "; count "+count+"; maxmin:"+max+";"+min; 
  
  val interval = datainterval//if (newInterval==0) datainterval
    /*else if (newInterval<datainterval) 
      throw new IllegalArgumentException("Interval must be at least: "+datainterval)
    else newInterval
  val step=(interval/datainterval).toInt*/

  
  override def next:Option[Point]={//}interval:Double)={
    //if (interval<datainterval) 
    //  throw new IllegalArgumentException("Interval must be at least: "+datainterval)
    //val strLine =(1 to step).map(_=>csv.readNext).last
    
    //val hulls=(1 to 1).map{_=>
    val strLine=csv.readNext
	if (strLine == null || countValues>maxCount){
	  csv.close
	  closed=true
	  return None }
	else {
	  val d = strLine(index).toDouble
	  val p = new Point(countValues*datainterval,d)
	  countValues+=1
	  updateMaxMin(d)
	  Some(p) }
    //}
    //.map(_.get).reduceLeft((a,b)=>a merge b)
    //Option(hulls)
  }	
  
  override def stream=rawData.view.zipWithIndex.map{
    case (p,i)=>new Point(i*datainterval,p) }.toStream
  
  def normalized=rawData.view.zipWithIndex.map{
    case (p,i)=>new Point(i*datainterval,(p-mean)/stdev)}
  def reset=new CsvSeries(index,filename,initPeriod,startVal,typeData)//,newInterval)
  //def resample(intr:Double)=new CsvSeries(index,filename,initPeriod,startVal,typeData,intr)
}

object CsvSeries {
  def apply(filepath:String, typeData:String) =new CsvSeries(0,filepath,0,0,typeData)
}
