package es.upm.fi.oeg.siq.data

import org.scalatest.prop.Checkers
import org.scalatest.junit.ShouldMatchersForJUnit
import org.scalatest.junit.ShouldMatchersForJUnit._
import org.scalatest.junit.JUnitSuite
import org.junit.Test

class GeometryTest extends JUnitSuite with ShouldMatchersForJUnit with Checkers {

  val coords=Array(-2.086,-6.248,-8.98,-8.65,-9.03,-8.28,-7.79,-8.09)
    val points=coords.zipWithIndex.map{case (d,i)=>new Point(i,d)}
    val hulls=points.map(p=>Hull(p))

  @Test  
  def testHullUnitary{
    val h1=Hull(points(0))
    val h2=Hull(points(1))
    h1.size should be (1) 
    val m1=h1 merge h2
    m1.size should be (2)
    m1.points(1) should be (h2.left) 
  }
    
  @Test def testHull{
    val m1=hulls.foldLeft(SHull():SHull)((a:SHull,b:SHull)=> a.merge(b))
    println(m1.mkString)
  }
  
  @Test def testMergeLine{
    val pts = Points(Array(1,1,1))
    val h1 = Hull(pts(0))
    val h2 = new Hull(Array(pts(1),pts(2)))
    val m1 = h1 merge h2
    println("merged:"+ m1.mkString)
    m1.points(0) should be (pts(0))
    m1.points(1) should be (pts(2))     
  }
  
  @Test def testMergeLineAndPoint{
    val pts = Points(Array(2,2,1))
    val h1 = new Hull(Array(pts(0),pts(1)))
    val h2 = Hull(pts(2))
    val m1 = h1 merge h2
    println("merged:"+ m1.mkString)
    m1.points(0) should be (pts(0))
    m1.points(2) should be (pts(2))     
  }

  @Test def testMergeTriangle{
    val pts = Points(Array(1,2,0))
    val h1 = new Hull(Array(pts(0),pts(1)))
    val h2 = Hull(pts(2))
    val m1 = h1 merge h2
    println("merged:"+ m1.mkString)
    m1.points(0) should be (pts(0))
    m1.points(2) should be (pts(2))     
  }

  @Test def testMergeAligned{
    val h1 = new Hull(Array(new Point(0.1,67.4),new Point(0.2,67.7),new Point(0.23333333333333334,67.8),new Point(0.16666666666666666,67.6)))
    val h2 = new Hull(Array(new Point(0.26666666666666666,67.8),new Point(0.3,67.9),new Point(0.3333333333333333,68.0)))
    val m1 = h1 merge h2
    println("merged:"+ m1.mkString)
    m1.points(0) should be (h1.left)
    m1.right should be (h2.right)
    m1.points(m1.points.length-1) should be (h2.left)
    
  }
  //(0.1,67.4)(0.2,67.7)(0.23333333333333334,67.8)(0.16666666666666666,67.6)
//2012-01-20 19:16:28,136 [main] DEBUG es.upm.fi.oeg.siq.data.compr.PwlhSummary - bucket: (0.26666666666666666,67.8)(0.3,67.9)(0.3333333333333333,68.0)
  
  @Test def testMergeQuad{
    val pts = Points(Array(0,2,1,1))
    val h1 = new Hull(Array(pts(0),pts(1),pts(2)))
    val h2 = Hull(pts(3))
    val m1 = h1 merge h2
    println("merged:"+ m1.mkString)
    m1.points(0) should be (pts(0))
    m1.points(2) should be (pts(3))     
  }
    //println(h2.points+","+h2.left+","+h2.right)
    //println(m1.points.mkString)
/*
    m1.size should be (2)
    val m2 = m1 merge hulls(2)
    m2.size should be (3)
    val m3 = m2 merge new IHull(Array(points(3),points(4)))
    m3.size should be (4)
    println(m3.points.mkString)*/
 
}