package es.upm.fi.oeg.siq.data

import au.com.bytecode.opencsv.CSVReader
import scala.io.Source
import scala.collection.JavaConversions._


abstract class Series(index:Int) {
  var max:Double = 0
  var min:Double = Double.PositiveInfinity	
  var closed=false	
  def count:Int
  val interval:Double = 1
  val period = 0
  val maxCount=0
		
  protected def updateMaxMin(d:Double)={
    if (d>max) max =d
	if (d<min) min =d
  }
  	
  def next():Option[Double]
  def stream:Stream[Double]
}
/*
object SeriesFactory {
  def newSeries(index:Int,interval:Double,seriesPaths:java.util.List[String],period:Int):Series={
    val list = seriesPaths.map(path=>new CsvSeries(index,path,period,0))
    new AggregateSeries(list)    
  }
}*/

class CsvSeries(val index:Int,val filename:String,initPeriod:Int,startVal:Int=0) extends Series(index) {
  
  val r = """###(\d+)""".r
  val s =  Source.fromFile(filename)
  override val interval=s.getLine(1) match {  
    case r(st) => { st.toDouble/60} 
    case _ =>error("Interval unavailable! ")
  }
  override val maxCount=
    if (initPeriod<=0) s.getLines().length
    else (initPeriod*24*60/interval).toInt
  override val period = 
    if (initPeriod<=0)  (maxCount*interval/(24*60)).toInt 
  	else initPeriod
  //override val maxCount = (period*24*60/interval).toInt
  
  val csv = new CSVReader(Source.fromFile(filename).bufferedReader(),
	    ',',CSVReader.DEFAULT_QUOTE_CHARACTER,1+startVal)
  var countValues:Int=0		
  	
  override def count=this.countValues

  override def toString = "maxCount "+maxCount+"; interval "+interval+
  		"; period "+period+ "; count "+count+" maxmin:"+max+";"+min; 
  
  override def next={
    val strLine = csv.readNext
	if (strLine == null || countValues>maxCount){
	  csv.close
	  closed=true
	  None }
	else if ( strLine(index).equals("null"))
	  next
	else {
	  countValues+=1
	  val d = strLine(index).toDouble
	  updateMaxMin(d)
	  Some(d) } 
  }	
  override def stream=Stream.continually(next).takeWhile(_.isDefined).map(_.get)
}

object CsvSeries {
  def apply(filepath:String) =new CsvSeries(0,filepath,0,0)
}
/*
class AggregateSeries(series:java.util.List[Series]) extends Series(0) {
  override def next ={
    val s= series.filter(!_.closed)
    if (s.size==0)  None
    else {
      val d = s.head.next
      if (d.isEmpty) next
      else {
        updateMaxMin(d.get)
    	d }
    }
  }

  override def count = series.map(a=>a.count) sum
}
*/