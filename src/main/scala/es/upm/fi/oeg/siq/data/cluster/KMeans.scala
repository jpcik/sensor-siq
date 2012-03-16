package es.upm.fi.oeg.siq.data.cluster
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import es.upm.fi.oeg.siq.data.Means
import es.upm.fi.oeg.siq.data.MeansUtils

class KMeans[T<:Means](val k:Int,val data:Seq[T]) {
  if (data.isEmpty) throw new IllegalArgumentException("data should not be empty.")
  var clusters = kkzInit
  val utils=data.first.companion
  if (utils==null) throw new IllegalArgumentException("companion should not be empty.")
  
  private def randomInit={
    val rand = new Random
    data.map(a=>(a,rand.nextInt(k))).groupBy(a=>a._2).
           map(b=>new Cluster(b._2.map(_._1)))    
  }
  
  def kkzInit={
    var dataleft=data.toList
    val m=data.first.companion.means(data) 
    val (first,idx)=data.map(d=>d.distance(m)).zipWithIndex.max
    val centroids=ArrayBuffer(new CentroidCluster(dataleft(idx)))    
    dataleft=dataleft.remove(_==dataleft(idx))
    while (centroids.length<k){
      val (largest,lidx)=dataleft.map(d=>
          centroids.map(c=>d.distance(c.mean)).min).zipWithIndex.max
      centroids+=new CentroidCluster(dataleft(lidx))
      dataleft=dataleft.remove(_==dataleft(lidx))
    }
    
    val newClusters=(0 until k).map(a=>new ArrayBuffer[T])
    data.foreach(i=>newClusters(
        centroids.map(c=>i.distance(c.mean)).zipWithIndex.min._2)+=i)
    newClusters.map(c=>new Cluster(c))    
  }
  
  def cluster={
    val newClusters=(0 until k).map(a=>new ArrayBuffer[T])//new Array[ArrayBuffer[T]](k)
    clusters.foreach(c=>c.set.foreach(i=>newClusters(nearestMean(i)._2)+=i))
    //clusters.foreach(c=>println("old means: "+c.mean.mkString))
    clusters=newClusters.map(c=>new Cluster(c))
    //clusters.foreach(c=>println("new means: "+c.mean.mkString))
    //clusters.foreach(c=>println("mean "+c.mean.mkString))
  }
  def same(c1:Iterable[Cluster[T]],c2:Iterable[Cluster[T]])=
    c1.zip(c2).forall(a=>a._1==a._2)
  
  def nearestMean(item:T)=
    clusters.map(c=>item.distance(c.mean)).zipWithIndex.min
}