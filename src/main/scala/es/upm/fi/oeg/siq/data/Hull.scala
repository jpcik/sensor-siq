package es.upm.fi.oeg.siq.data

abstract class SHull(){
  def merge(hull:SHull):SHull
  def mkString:String
}

object SHull{
  def apply()=new EmptyHull
}

case class EmptyHull extends SHull(){
  override def merge(hull:SHull)=hull
  override def mkString="()"
}
case class IHull(points:Array[Point]) extends SHull(){
  private val minx = points.map(_.x).min
  private val maxx = points.map(_.x).max
  val (left,leftIdx)=points.zipWithIndex.find{case (p,i)=>p.x==minx} get
  val (right,rightIdx)=points.zipWithIndex.find{case (p,i)=>p.x==maxx} get
  val size=points.length  
  def mkString=points.mkString
  
  def next(i:Int)=
    if (i==points.length-1) points.first
    else points(i+1)
    
  def prev(i:Int)=
    if (i==0) points.last
    else points(i-1)
    
  def turnCcw(a:Point,i:Int) =
    Points.turn(a,points(i),next(i))<=0 && Points.turn(a,points(i),prev(i))<0
   
  def turnCw(a:Point,i:Int) =
    Points.turn(a,points(i),next(i))>=0 && Points.turn(a,points(i),prev(i))>0
  
  override def merge(hull:SHull):IHull= hull match {
    case EmptyHull() =>this
    case h@IHull(p)=>{     
      if (size==1 && h.size==1) new IHull(points++h.points)
      else {
        //println("merging: "+this.mkString+" and "+h.mkString )        
	    val pi = h.points.zipWithIndex
		val tpi = points.zipWithIndex
	    val lowTan2 = (pi.tail:+pi.head).reverse.iterator.
	    	dropWhile{case (d,i)=>(!h.turnCcw(right,i) && d!=h.right)}.next
		val lowTan1 = (tpi.slice(rightIdx,points.length):+tpi.head).iterator.
		    dropWhile{case (d,i)=> !turnCw(lowTan2._1,i) && d!=this.left}.next
		val hTan2 = pi.iterator.
		 	dropWhile{case (d,i)=> !h.turnCw(right,i) && d!=h.right}.next
		val hTan1=(tpi.slice(0,rightIdx+1)).reverse.iterator. 
			dropWhile{case (d,i)=> !turnCcw(hTan2._1,i) && d!=this.left}.next
		val l3:Array[Point] = if (lowTan1._1==left) Array() else
				points.slice(lowTan1._2,points.length)
		val lst=points.slice(0,hTan1._2+1)++h.points.slice(hTan2._2,lowTan2._2+1)++l3
		//println(hTan1+","+hTan2+","+lowTan2+";"+lowTan1)		     
		new IHull(lst)}		  
    }
  }
}

object IHull{
  def apply(p:Point)=new IHull(Array(p))
}
/*
class Hull(var left:Point,var right:Point,var count:Int) {
  //var count = 0
  //val points = 
  def iterator(start:Point=left,fwd:Boolean=true):Iterator[Point]=new Iterator[Point]{
    var curr:Point = _
    def next ={curr=if (curr==null) start else curr.move(fwd); curr }
    def hasNext = {(curr==null && start!=null) || (curr !=null && curr.move(fwd)!=start)}
  }
  
  def mergeIterator(highTan:(Point,Point),lowTan:(Point,Point))=new Iterator[Point]{
    var curr:Point=_
    private def peek={
      if (curr==null) left
      else if (curr==highTan._1) highTan._2
	  else if (curr==lowTan._2) lowTan._1
      else curr.next }
    def next ={curr=peek; curr }
    def hasNext = (curr==null && left!=null) || (peek!=left)
  }
    
  def merge(h:Hull)= {    
    val merged = new Hull(null,null,0)
    val a=right

	val lowTan2=h.iterator(fwd=false).dropWhile(d=> !turnCcw(a,d) && d!=h.right).next
	val lowTan1=iterator(start=this.right).dropWhile(d=> !turnCw(lowTan2,d) && d!=this.left).next			
	val hTan2=h.iterator().dropWhile(d=> !turnCw(a,d) && d!=h.right).next	
	val hTan1=iterator(right,false).dropWhile(d=> !turnCcw(hTan2,d) && d!=this.left).next			
	//var mpt:(Point,Point)=_	
	var newP:Point=null
		
	val it = mergeIterator((hTan1,hTan2),(lowTan1,lowTan2))
	it.foreach(p=> { 	
	  val prevP=newP
	  newP=p.copy
	  merged.count+=1
	  newP.prev=prevP
	  if (prevP!=null) prevP.next=newP		
	  if (p==left) merged.left=newP
	  else if (p==h.right) merged.right=newP })
	newP.next=merged.left
	merged.left.prev=newP
	merged
  }

}

object Hull{
  def apply(p:Point)={
    p.next=p
    p.prev=p
    new Hull(p,p,1)
  }
  def turn(p0:Point,p1:Point,p2:Point)=
    (p2.x-p0.x)*(p1.y-p0.y)-(p1.x-p0.x)*(p2.y-p0.y)
    
  def turnCcw(a:Point,d:Point) =
    turn(a,d,d.next)<=0 && turn(a,d,d.prev)<=0
   
  def turnCw(a:Point,d:Point) =
    turn(a,d,d.next)>0 && turn(a,d,d.prev)>=0
}
*/