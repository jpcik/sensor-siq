package es.upm.fi.oeg.siq.data
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.scalatest.prop.Checkers
import org.junit.Test
import es.upm.fi.oeg.siq.data.cluster.KMeans
import es.upm.fi.oeg.siq.profile.SensorProfiler._
import es.upm.fi.oeg.siq.profile.SwissExDataset
import scala.math._
import au.com.bytecode.opencsv.CSVReader
import scala.io.Source
import es.upm.fi.oeg.siq.profile.SensorDataset
import es.upm.fi.oeg.siq.data.compr.PwlhSummary
import org.junit.Ignore
import collection.JavaConversions._
import java.io.FileWriter
import es.upm.fi.oeg.siq.data.compr.LinearSummary
import es.upm.fi.oeg.siq.profile.AemetDataset

class PwlhTest extends JUnitSuite with ShouldMatchersForJUnit with Checkers {
  @Test def testReadSlopes{
    val ds:SensorDataset=AemetDataset
    ds.props.foreach{prop=>
    val s=Source.fromFile("c:/aemet/"+prop._3+".data")
    s.getLines.foreach{line=>
      line.split(';')(1).split(',').foreach(println)
    }}
  }
  
  @Test@Ignore def testClusterDistribution{
    //val dists=((1 to 35).toList--List(2,18,21,22,34,35)).map(i=>{
    val ds:SensorDataset=AemetDataset
     val f=new FileWriter("c:/aemet/humidity.data")
    val result=
   ds.props.filter(_._3.equals("humidity")).foreach{prop=>
    val dists=(1 to prop._2).foreach{i=>
      val code=ds.getCode(prop._3,i)
      val d = new CsvSeries(0,ds.dataPath(prop._3,code),0,0,prop._3)
	  val dataDay=60*24/d.interval
	  
      val data=new CsvSeries(0,ds.dataPath(prop._3,code),40000/dataDay,0,prop._3)
      
      val buckets=buck(data.interval)
      val p=new LinearSummary(data,buckets*data.period/data.maxCount)
      p.linearApprox
      val dm=p.buckets.map{b=>
        if (b.lr==null) b.regression(b.h)
        
        (b.lr.getSlope*30)}
     f.write(code+";"+dm.mkString(",")+"\r\n") 
      //dm
      /*
      val km=new KMeans[DoubleMeans](10,dm)
      val c1=Stream.continually{km.cluster;km.clusters}.take(5).last
      c1.foreach(a=>    println(a.set.size+"--- "+a.set.reduceLeft((a,b)=>new DoubleMeans(a.n+b.n)).n/a.set.size +"::"+a.mkString))*/
    }//.flatten  
    dists
    }//.flatten.sorted
    //result.foreach(println)
    /*
    val dists=(1 to 10).map(i=>{
    val summ = PwlhSummary(CsvSeries(dataPath+temp._1+i+".csv"),2)
        
    summ.pwlh
    val d = summ.generateDistribution(Pi/12,20,true)
    d
    })
    val km=new KMeans[Distribution](3,dists)
    var c1=km.clusters
    c1.foreach(a=>println(a.mkString))
    val clust=Stream.continually{km.cluster;km.clusters.foreach(a=>println(a.mkString)); km.clusters}.take(10).last
    clust.foreach(a=> println(a.set.size+"--- "+a.mkString))*/
  }
  
  @Test@Ignore def testCountAngles(){
    val ds=SwissExDataset
    ds.props foreach {prop=>
      val indexes= (1 to prop._2) 
      val dip=indexes.map{i=>
      //(1 to prop._2).foreach(i=>{
        loadDistributions(ds,prop._3,ds.getCode(prop._3,i))
      }.filter(_.isDefined).map(f=>f.get.find(r=>r._2==buck(r._1.interval))).filter(_.isDefined)
/*        if (ll.isDefined){
        val reb=ll.get.filter(f=>f._2==50)
        if (!reb.isEmpty)*/
        dip.map{d=>
          //if (d._2==buck(d._1.interval))
          val v=d.get._1.values.map(e=>(e._2*d.get._1.period).toInt)
          val sum=v.sum
          val v1=v.map(a=>a.toDouble/sum)
         (d.get._1.name,v1)
        }//.reduceLeft((a1,a2)=>("",a1._2.zip(a2._2).map(r=>r._1+r._2)))._2        
        .foreach{t=>
          println(t)
        }
        
      
    }
  }
}