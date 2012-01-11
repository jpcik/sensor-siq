package es.upm.fi.oeg.siq.data.cluster
import es.upm.fi.oeg.siq.data.Means
import es.upm.fi.oeg.siq.data.EmptyMeans
import scala.collection.mutable.ArrayBuffer

class Cluster[T<:Means](val set:Seq[T]) {
  //val utils=set.first.companion
  val mean=if(set.isEmpty) {println("empty");new EmptyMeans()} else  set.first.companion.means(set) 
  def mkString=set.toList.map(a=>a.mkString+":"+a.distance(mean))
  def ==(c:Cluster[T])=set.zip(c.set).forall(a=>a._1==a._2)
}

class CentroidCluster[T<:Means](centroid:T) extends Cluster(List()){  
  override val mean=centroid
}
