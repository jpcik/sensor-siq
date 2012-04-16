package es.upm.fi.oeg.siq.data.compr
import java.util.PriorityQueue
import java.util.Comparator
import scala.math._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import es.upm.fi.oeg.siq.data._
import com.weiglewilczek.slf4s.Logging
import es.upm.fi.oeg.siq.data.CsvSeries$
import au.com.bytecode.opencsv.CSVWriter
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVReader

abstract class Summary(val data:Series,val compression:Double) extends Logging{
  val max_buckets = (data.maxCount*compression).toInt
  if (max_buckets<1) 
    throw new IllegalArgumentException("Too few data: "+data.maxCount+ " for compression: "+compression)
  if (max_buckets>=data.maxCount/2) 
    throw new IllegalArgumentException("Too many buckets: "+max_buckets+ " insuff compression: "+compression)
}

class PwlhSummary(data:Series,compression:Double) extends Summary(data,compression){
  private val bucketlimit=min(10,max_buckets)
  val chuncks=(max_buckets)/bucketlimit
  //val chuncks=(max_buckets/data.asInstanceOf[CsvSeries].step)/bucketlimit
  val interval = //if (data.maxCount*compression>bucketlimit)
  //  data.period*24*60*compression/bucketlimit
  //else 
    data.datainterval
  private val dataSize=data.maxCount/(interval/data.datainterval).toInt//data.period*24*60/interval  
  private var size=0
  var firstLin:LinearBucket=_
  var lastLin:LinearBucket=_
  val index = new PriorityQueue[Index](10,new IndexComparator)
  println("data "+toString() )
  println("bucketlim: "+ bucketlimit)
  println("chunks: "+chuncks)
  
  
  lazy val buckets={
   var b:LinearBucket=null
   Stream.continually{
     if (b==null) {b=firstLin;firstLin}
     else {b=b.next;b}
     }.takeWhile(a=>a!=null) 
  }
  
  def computeValue(time:Double)={
    val bucks=buckets.filter(b=>time>=b.h.left.x && time<=b.h.right.x)
    if (bucks.isEmpty)
      throw new Exception("no value for time: "+time)
    val lb=bucks.first
    logger.debug("getting value "+time)
    lb.getValue(time)
  }
  
  def getError()={
	//var sum = 0d
    if (firstLin==null) Double.NaN
    else
    {
	//var count=0
	sqrt(buckets.map(_.totalErrorSqrSum).sum/data.maxCount)
    //val rdata=data.asInstanceOf[CsvSeries].reset
	//val sum=rdata.stream.map(h=>pow(h.left.y-computeValue(h.left.x),2)).sum
	
	/*
	var lb=firstLin
	while (lb!=lastLin){
	  sum+=lb.getErrorSqrSum
	  lb=lb.next;
	  count+=lb.h.size
	}*/			
	//sqrt(sum/rdata.count)}
    }
  }
	
  def pwlh {
	size=0
	lastLin=null
	firstLin=null
	var o:Option[Point]=None
	//var i=0d
	var c=0
	//while ({o=data.next;o.isDefined}){
	data.asInstanceOf[CsvSeries].normalized.foreach{p=>
	  logger.trace("Data: "+o+" Buckets: "+this.size+" "+max_buckets)
	  //println("Data: "+o+ " Buckets: "+this.size+" "+max_buckets)
	  //val p = new Point(i,o.get)
	  val b = new LinearBucket(Hull(p))			
	  b.points=List(p)//o.get.points.toList	
	  
	  b.next=null
	  b.prev=lastLin
	  if (b.prev!=null)
	    b.prev.next=b
	  if (size>1){
		val prev = b.prev
		index.add(new Index(b.getErrorMerge,prev))
	  }	
	  if (firstLin == null)
		firstLin = b
	  lastLin = b
	  this.size+=1//b.h.size
	  
	  if (this.size>bucketlimit){
		val ind = index.remove()
		logger.trace("removed "+ind.lb.h.left.x+" "+ind.err)
		val bprev = ind.lb.prev
		val b1 = ind.lb
		val b2 = ind.lb.next

		val merged = new LinearBucket(b1.h.merge(b2.h))
		merged.points=b1.h.points.toList:::b2.h.points.toList
		//logger.debug("merged: "+merged.h.mkString)
		//merged.h_$eq(b1.h().merge(b2.h()));
		if (bprev == null)
		  firstLin = merged
		else
		  bprev.next=merged
		if (b2.next!=null)
		  b2.next.prev=merged
		else
		  lastLin = merged;
	    merged.next=b2.next
		merged.prev=bprev

		if (bprev!=null){
		  index.remove(new Index(0d,bprev))
		}
		index.remove(new Index(0d,b2))
				
	    if (bprev!=null){
		  index.add(new Index(merged.getErrorMerge(),bprev))
		}
		if (merged.next!=null){
		  index.add(new Index(merged.next.getErrorMerge(),merged))				
		}
	  }
				
	  //buckets.foreach(b=>logger.debug("bucket: "+b.h.mkString))
	  //i+=interval
	  c+=1
	  //i=c*interval
      if (chuncks>1 && size==(data.maxCount)/chuncks){
        logger.trace("Reset index: "+size)
        index.clear
        size=0
      }	
	}
  }
  
  
  def export(bucketPerDay:Int,filename:String){
	val w = new CSVWriter(new FileWriter(filename+"_params.csv"),';',CSVWriter.NO_QUOTE_CHARACTER)
	val wd = new CSVWriter(new FileWriter(filename+"_data.csv"),';',CSVWriter.NO_QUOTE_CHARACTER)
	w.writeNext(Array(data.toString))
    
    val summary=buckets.foreach{b=>
      //println("slopesss")
      //b.regression(b.h)
      wd.writeNext(Array(b.h.left.x.toString,b.getValue(b.h.left.x).toString))
      wd.writeNext(Array(b.h.right.x.toString,b.getValue(b.h.right.x).toString))

      w.writeNext(Array(b.lr.getSlope.toString,b.lr.getIntercept.toString,
          b.h.left.x.toString,b.h.right.x.toString))
      
      }
	w.writeNext(Array("summary",toString))
	//w.writeNext(Array(summary.mkString))
	w.close
	wd.close
  }
	
  
  
  def generateDistribution(tangents:Array[Double],slp:Int,boostBucket:Boolean=false)={
	var bit=firstLin
	val d= new Distribution("",tangents,data.period,data.datainterval,data.typeData,0);				
	do {
	  bit.lr=null
	  bit.regression(bit.h)
	  println(bit.h.left.x+";"+bit.getValue(bit.h.left.x))
  	  println(bit.h.right.x+";"+bit.getValue(bit.h.right.x))
			//val slp = slope;///data.interval();
	  val slope = bit.lr.getSlope*slp
	  //val maxerr = (data.max-data.min)*0.1
	  //val fct = Math.sqrt(bit.getErrorSqrSum()/bit.h.count)/maxerr;
	  if (boostBucket)
		  d.add(slope,(1+bit.h.right.x-bit.h.left.x))//,(1-fct));//*bit.h.count);//bit.h.count);
	  else d.add(slope,1)
	  bit = bit.next
	}while (bit!=null)
    //println(d.values);		
	d 
  }
	
  override def toString={
    val d = data.asInstanceOf[CsvSeries]
    "interval "+interval+
                 ";compression "+compression+
                 data.toString+
                 "; error "+getError+
                 ";"+getError/(d.mean+d.stdev)//((data.max-data.min))///d.stdev)
  }
}

object PwlhSummary {
  private val maxMult = 50*1000000L
  def apply(data:CsvSeries,buckets:Double)={
      new PwlhSummary(data,buckets)      
  }
}

class IndexComparator extends Comparator[Index]{
  override def compare(a:Index,b:Index)=a.compareTo(b)
}

class Index(val err:Double,val lb:LinearBucket, val b:Bucket) extends Comparable[Index]{
  private lazy val rnd = new Random
  def this(err:Double,b:Bucket) = this(err,null,b)
  def this(err:Double,lb:LinearBucket)= this(err,lb,null)
  override def compareTo(i:Index)= 
    /*if (err==i.err) 
      if (lb.h.m rnd.nextBoolean) 1 else -1
    else*/ this.err.compareTo(i.err)
  override def equals(i:Any)= this.lb.h.left.x==i.asInstanceOf[Index].lb.h.left.x
}