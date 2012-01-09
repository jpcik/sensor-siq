package es.upm.fi.oeg.siq.data.cluster

import org.scalatest.prop.Checkers
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import es.upm.fi.oeg.siq.data.cluster._

class ClusterTest extends JUnitSuite with ShouldMatchersForJUnit with Checkers {
  @Test def testInitCluster{
    
    val km=new KMeans[IntMeans](3,Array(53,4,3,1,2,42,5,62).map(i=>new IntMeans(i)))
    println((Array(4,6,7,1,8,4,3)).zipWithIndex.min)
    var c1=km.clusters
    c1.foreach(i=>println(i.mkString))

    //Stream.continually(km.cluster).take(6).last
    var cont=true
    
    do{
    km.cluster
    val c2=km.clusters
    km.clusters.foreach(i=>println(i.mkString))
    cont= !km.same(c1,c2)
    if (cont)
      c1=c2  
    } while (cont)
    c1.foreach(i=>println(i.mkString))
      
  }
}