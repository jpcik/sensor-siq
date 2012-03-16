package es.upm.fi.oeg.siq.data.compr
import scala.collection.mutable.ArrayBuffer
import es.upm.fi.oeg.siq.data._

class LinearSummary(data:Series,compression:Double) extends Summary(data,compression){
  val buckets = new ArrayBuffer[LinearBucket]
  def linearApprox{
    var i=0d
    val bucksize = data.maxCount/(this.max_buckets*2)
    println("bucketsize: "+bucksize)
    var buffer:SHull = new EmptyHull //new ArrayBuffer[Point]
    
    data.stream.foreach{d=>
      //val p = new Point(i,d)      
      if (buffer.size<bucksize){        
        buffer=buffer merge Hull(d) }
      else {
        //val h=buffer.foldLeft(new EmptyHull:SHull)((a,b)=>a.merge(Hull(b)))
        //buckets+=new LinearBucket(h.asInstanceOf[Hull])
        buckets+=new LinearBucket(buffer.asInstanceOf[Hull])
        //buffer.clear
        buffer = new EmptyHull
      }
	  //i+=data.interval;
    }
  }
	
  def generateDistribution(angle:Double,slp:Int)={
	val d= new Distribution("",angle,data.period,data.datainterval,data.typeData,0);				
	buckets.foreach(bit=> {
	  //bit.lr=null
	  bit.regression(bit.h)
	  println(bit.h.left.x+";"+bit.getValue(bit.h.left.x))
  	  println(bit.h.right.x+";"+bit.getValue(bit.h.right.x))
			//val slp = slope;///data.interval();
	  val slope = bit.lr.getSlope*slp
	  //val maxerr = (data.max-data.min)*0.1
	  //val fct = Math.sqrt(bit.getErrorSqrSum()/bit.h.count)/maxerr;
	  d.add(slope,(1))//,(1-fct));//*bit.h.count);//bit.h.count);
	} )    	
	d 
  }
  
}