package es.upm.fi.oeg.siq.data.compr
import java.util.PriorityQueue
import java.util.Comparator
import scala.math._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import es.upm.fi.oeg.siq.data._

abstract class Summary(val data:Series,val bucketsPerDay:Int){
  val max_buckets = (data.period*bucketsPerDay).toInt
  if (max_buckets<1) throw new IllegalArgumentException("Invalid period: "+data.period+ " and buckets: "+bucketsPerDay)
}

class PwlhSummary(data:Series,bucketsPerDay:Int) extends Summary(data,bucketsPerDay){
  var size = 0
  var firstLin:LinearBucket=_
  var lastLin:LinearBucket=_
  val index = new PriorityQueue[Index](10,new IndexComparator)
  println("data "+data.toString() )
  def getError()={
	var sum = 0d
	var count=0
	var lb=firstLin
	while (lb!=lastLin){
	  val err = lb.getErrorSqrSum
	  sum+=err
	  lb= lb.next;
	  count+=lb.h.size
	}			
	sqrt(sum/count)
  }
	
  def pwlh() {
	//index = new PriorityQueue[Index](10,new IndexComparator());
	size=0;
	lastLin=null;
	firstLin=null;
	var o:Option[Double]=None;
	var i:Double = 0d;
	while ({o=data.next;o.isDefined}){
	  System.out.println("Data: "+o+ " Buckets: "+this.size+" "+max_buckets);
	  val p = new Point(i,o.get);
	  val b = new LinearBucket(Hull(p));			
					
	  b.next_$eq(null);
	  b.prev_$eq(lastLin);
	  if (b.prev!=null)
	    b.prev.next_$eq(b);
	  if (size>1){
		val prev = b.prev;
		index.add(new Index(b.getErrorMerge(),prev));
	  }	
	  if (firstLin == null)
		firstLin = b;
	  lastLin = b;
	  this.size+=1;
			
	  if (this.size>2*max_buckets){
		val ind = index.remove();
		println("removed "+ind.lb.h.left.x+" "+ind.err);
		val bprev = ind.lb.prev;
		val b1 = ind.lb;
		val b2 = ind.lb.next;

		val merged = new LinearBucket(b1.h.merge(b2.h));
		//merged.h_$eq(b1.h().merge(b2.h()));
		if (bprev == null)
		  firstLin = merged;
		else
		  bprev.next_$eq(merged);
		if (b2.next!=null)
		  b2.next.prev_$eq(merged);
		else
		  lastLin = merged;
	    merged.next_$eq(b2.next);
		merged.prev_$eq(bprev);

		if (bprev!=null){
		  index.remove(new Index(0d,bprev));
		}
		index.remove(new Index(0d,b2));
				
	    if (bprev!=null){
		  index.add(new Index(merged.getErrorMerge(),bprev));
		}
		if (merged.next!=null){
		  index.add(new Index(merged.next.getErrorMerge(),merged));				
		}
	}
				
	  i+=data.interval;
		
	}
  }
	
  def generateDistribution(angle:Double,slp:Int,boostBucket:Boolean=false)={
	var bit=firstLin
	val d= new Distribution("",angle);				
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
	
  override def toString= data.toString+" error:"+getError+";"+getError/(data.max-data.min)

}

object PwlhSummary {
  private val maxMult = 500*1000000L
  def apply(data:CsvSeries,buckets:Int) = {
    //println("heck"+data.period*data.maxCount*buckets.toLong)
    if (data.period*data.maxCount*buckets.toLong>maxMult){
      val f =  data.period*data.maxCount*buckets.toLong/maxMult
      val per = sqrt(maxMult*data.interval/(f*24*60*buckets)).toInt
      println ("new period "+per)
      new PwlhSummary(new CsvSeries(data.index,data.filename,per,0),buckets)
    }
    else
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
