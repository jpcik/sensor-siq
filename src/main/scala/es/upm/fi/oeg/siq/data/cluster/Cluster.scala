package es.upm.fi.oeg.siq.data.cluster
import es.upm.fi.oeg.siq.data.Means
import es.upm.fi.oeg.siq.data.EmptyMeans
import scala.collection.mutable.ArrayBuffer

class Cluster[T<:Means](val set:Seq[T]) {
  def mkString=set.toList.map(a=>a.mkString)
  val mean=if(set.isEmpty) new EmptyMeans else set.first.mean(set)// set.reduceLeft((a,b)=>a.add(b))
  def ==(c:Cluster[T])=set.zip(c.set).forall(a=>a._1==a._2)
}

class CentroidCluster[T<:Means](centroid:T) extends Cluster(List()){
  override val mean=centroid
}


class IntMeans(val n:Int) extends Means{
  override def distance(item:Means)=item match {
    case EmptyMeans() => Double.MaxValue
    case _ => math.abs(n-item.asInstanceOf[IntMeans].n)}
  
  //override def add(item:Means)=new IntMeans(n+item.asInstanceOf[IntMeans].n)
  override def mean(list:Iterable[Means])=new IntMeans( list.map(_.asInstanceOf[IntMeans].n).sum/list.size)
  def mkString=" "+n
}