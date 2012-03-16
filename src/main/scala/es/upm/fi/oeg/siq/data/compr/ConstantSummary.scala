package es.upm.fi.oeg.siq.data.compr
import es.upm.fi.oeg.siq.data._
import java.util.PriorityQueue
import scala.math._

class ConstantSummary(data:Series,bucketsPerDay:Int) extends Summary(data,bucketsPerDay){
  var first:Bucket=_
  var last:Bucket=_ 
  var size = 0
  val index = new PriorityQueue[Index](10,new IndexComparator)

    def read(data:Series){
	var o:Option[Point]=None
	var i = 0
	while ({o=data.next;o.isDefined}){
	  System.out.println("Data: "+o+ " Buckets: "+this.size)
	  val b = new Bucket(i,i,o.get.y,o.get.y)
	  b.next = null
	  b.prev = last
	  if (b.prev!=null)
		b.prev.next = b
	  if (size>1){
		val prev = b.prev
		index.add(new Index(abs((b.max-prev.min)/2),prev))
	  }	
	  if (first == null)
		first = b
	  last = b
	  this.size+=1
			
	  if (this.size>max_buckets){
		val ind = index.remove()
		//System.out.println("removed "+ind.b.beg+ind.err);
		val bprev = ind.b.prev
		val b1 = ind.b
		val b2 = ind.b.next

		val merged = new Bucket(b1.beg,b2.beg,
						min(b1.min, b2.min),max(b1.max, b2.max));
		if (bprev == null)
		  first = merged
		else
		  bprev.next=merged
		if (b2.next!=null)
		  b2.next.prev = merged
		else
		  last = merged
		merged.next = b2.next;
		merged.prev = bprev;

		if (bprev!=null){
		  index.remove(new Index(0d,bprev));
		}
		index.remove(new Index(0d,b2));
				
		if (bprev!=null){
		  index.add(new Index(abs((merged.max-bprev.min)/2),bprev));
		}
		if (merged.next!=null){
		  index.add(new Index(abs((merged.next.max-merged.min)/2),merged));
		}
	  }
				
	  i+=1
			//System.out.println(o);
	}
	var bit = first;
	do 
	{
		System.out.println(bit.beg+";"+bit.getValue());
		System.out.println(bit.end+";"+bit.getValue());
		bit = bit.next;
	}while (bit.next!=null);
	System.out.println(bit.beg+";"+bit.getValue());
	System.out.println(bit.end+";"+bit.getValue());
  }
	
	/*def pwlh():Distribution ={
		pwlh(25);
	}*/


}