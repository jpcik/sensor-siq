package es.upm.fi.oeg.siq.data

abstract class SHull(){
  def merge(hull:SHull):SHull
  def mkString:String
  val size:Int
}

object SHull{
  def apply()=new EmptyHull
}

case class EmptyHull extends SHull(){
  override def merge(hull:SHull)=hull
  override def mkString="()"
  override val size=0 
}
case class Hull(points:Array[Point]) extends SHull(){
  private val minx = points.map(_.x).min
  private val maxx = points.map(_.x).max
  val (left,leftIdx)=points.zipWithIndex.find{case (p,i)=>p.x==minx} get
  val (right,rightIdx)=points.zipWithIndex.find{case (p,i)=>p.x==maxx} get
  override val size=points.length  
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
  
  override def merge(hull:SHull):Hull= hull match {
    case EmptyHull() =>this
    case h@Hull(p)=>{     
      if (size==1 && h.size==1) new Hull(points++h.points)
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
		val l3:Array[Point] =  if (lowTan1._1==left) Array() else
				points.slice(lowTan1._2,points.length)
		val l2=if (lowTan2._1==h.left) h.points.slice(hTan2._2,h.points.length):+h.left
		  else h.points.slice(hTan2._2,lowTan2._2+1)
		val lst=points.slice(0,hTan1._2+1)++l2++l3
		//points.slice(0,hTan1._2+1).foreach(println)
		//l2.foreach(println)
		//l3.foreach(println)
		//println(hTan1+","+hTan2+","+lowTan2+";"+lowTan1)		     
		new Hull(lst)}		  
    }
  }
}

object Hull{
  def apply(p:Point)=new Hull(Array(p))
}
