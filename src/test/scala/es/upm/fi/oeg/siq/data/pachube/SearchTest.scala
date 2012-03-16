package es.upm.fi.oeg.siq.data.pachube
import org.scalatest.prop.Checkers
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import es.upm.fi.oeg.siq.data.pachube.Feed._
import org.junit.Ignore
import scala.io.Source

class SearchTest extends JUnitSuite with ShouldMatchersForJUnit with Checkers {

  @Test def testSearchTag(){
    //val f=new Feed(2)
    search("weather")
  }
  @Test@Ignore def testExportData(){
    //val f=new Feed(2)
    exportData(getClass.getResourceAsStream("/pachube/datastreams"))
    /*
    val s=Source.fromInputStream(getClass.getResourceAsStream("/pachube/datastreams"))
    s.getLines.map{line=>
      line.split(':')(1).split(';').map(s=>
        s.split('(')(1).dropRight(1))}.foreach(a=>a.foreach(println))*/
    //exportData(46126,Array("Humidity","Pressure","Rainfall-Rate","Rainfall-Today","Temperature","Wind-Speed"))
  }
}