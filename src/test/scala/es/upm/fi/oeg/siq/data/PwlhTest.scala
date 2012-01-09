package es.upm.fi.oeg.siq.data
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.scalatest.prop.Checkers
import org.junit.Test
import es.upm.fi.oeg.siq.data.cluster.KMeans
import es.upm.fi.oeg.siq.profile.SensorProfiler._
import scala.math._
import au.com.bytecode.opencsv.CSVReader
import scala.io.Source

class PwlhTest extends JUnitSuite with ShouldMatchersForJUnit with Checkers {
  @Test def testClusterDistribution{
    //val dists=((1 to 35).toList--List(2,18,21,22,34,35)).map(i=>{
    val dists=(1 to 10).map(i=>{
      val csv = new CSVReader(Source.fromFile(resultPath+moisture._1+i+".csv").bufferedReader,',',CSVReader.DEFAULT_QUOTE_CHARACTER)
      Stream.continually(csv.readNext()).takeWhile(_!=null).map(a=>{
        val d=( Distribution(temp._3+i+a(0)+"-"+a(1), a(1).toInt, a(2).dropRight(1).drop(5).split(",").map(_.toDouble)))       
        d
      }).filter(d=>d.name.endsWith("2-12")).first
    })
    dists.foreach(d=>println(d.percentages))
    /*
    val dists=(1 to 10).map(i=>{
    val summ = PwlhSummary(CsvSeries(dataPath+temp._1+i+".csv"),2)
        
    summ.pwlh
    val d = summ.generateDistribution(Pi/12,20,true)
    d
    })*/
    val km=new KMeans[Distribution](2,dists)
    var c1=km.clusters
    c1.foreach(a=>println(a.mkString))
    val clust=Stream.continually{km.cluster;km.clusters.foreach(a=>println(a.mkString)); km.clusters}.take(10).last
    clust.foreach(a=> println(a.set.size+"--- "+a.mkString))
  }
}