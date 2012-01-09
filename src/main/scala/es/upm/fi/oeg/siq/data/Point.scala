package es.upm.fi.oeg.siq.data

class Point(val x:Double,val y:Double) {
  //var next:Point=_
  //var prev:Point=_
  //def move(fwd:Boolean=true)=if (fwd) next else prev 
  def copy = new Point(x,y)
  override def toString="("+x+","+y+")"
}

object Points{
  def apply(points:Array[Double])=
    points.zipWithIndex.map{case (d,i)=>new Point(i,d)}
  def turn(p0:Point,p1:Point,p2:Point)=
    (p2.x-p0.x)*(p1.y-p0.y)-(p1.x-p0.x)*(p2.y-p0.y)

}
