package es.upm.fi.oeg.siq.data.cluster
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import es.upm.fi.oeg.siq.data.Means

class KMeans[T<:Means](val k:Int,val data:Seq[T]) {
  var clusters = kkz
  private def initialize={
    val rand = new Random
    data.map(a=>(a,rand.nextInt(k))).groupBy(a=>a._2).
           map(b=>new Cluster(b._2.map(_._1)))    
  }
  
  def kkz={
    data.foreach(a=>println(a.mkString))
    val m = data.first.mean(data)
    var dataleft=data.toList.reverse
    val first = data.map(d=>d.distance(m)).zipWithIndex.max
    val cc=new CentroidCluster(dataleft(first._2))
    val centroids=ArrayBuffer(cc)    
    dataleft=dataleft.remove(_==dataleft(first._2))
    println(cc.mean.mkString)
    while (centroids.length<k){
      val largest=dataleft.map(d=>
          centroids.map(c=>d.distance(c.mean)).min).zipWithIndex.max
      centroids+=new CentroidCluster(dataleft(largest._2))
      dataleft=dataleft.remove(_==dataleft(largest._2))
    }
    
    val newClusters=(0 until k).map(a=>new ArrayBuffer[T])
    data.foreach(i=>newClusters(
        centroids.map(c=>i.distance(c.mean)).zipWithIndex.min._2)+=i)
    newClusters.map(c=>new Cluster(c))    
  }
  
  def cluster={
    val newClusters=(0 until k).map(a=>new ArrayBuffer[T])//new Array[ArrayBuffer[T]](k)
    clusters.foreach(c=>c.set.foreach(i=>newClusters(nearestMean(i)._2)+=i))
    clusters=newClusters.map(c=>new Cluster(c))    
  }
  
  def same(c1:Iterable[Cluster[T]],c2:Iterable[Cluster[T]])={
    c1.zip(c2).forall(a=>a._1==a._2)
  }
  
  def nearestMean(item:T)={
    clusters.map(c=>item.distance(c.mean)).zipWithIndex.min
  }
  
}