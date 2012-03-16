package es.upm.fi.oeg.siq.data

import scala.math._

import weka.classifiers.functions.SimpleLinearRegression
import weka.core.FastVector
import weka.core.Attribute
import weka.core.Instances
import weka.core.Instance

class LinearBucket(val h:Hull) {
  var next:LinearBucket=_
  var prev:LinearBucket=_
  var points:List[Point]=Nil
  var lr:SimpleLinearRegression=_

  def regression(hull:Hull) {
	lr = new SimpleLinearRegression
	lr.setSuppressErrorMessage(true)
	val fv = new FastVector
	fv.addElement(new Attribute("x"))
	fv.addElement(new Attribute("y"))
	val is = new Instances("pnts", fv , 1)
	hull.points.foreach(p=>	{		
	  val inst = new Instance(2)
	  inst.setValue(0,p.x )
	  inst.setValue(1,p.y )
	  is.add(inst)})
	is.setClassIndex(1)
	lr.buildClassifier(is)	
  }
  def getErrorSqrSum()=
    //h.iterator().map(p=> pow(p.y-(lr.getIntercept+p.x*lr.getSlope),2)) sum
    h.points.map(p=> pow(p.y-(getValue(p.x)),2)) sum
	
	
  def getErrorMerge()={
	val m=prev.h.merge(h)
	regression(m)
	m.points.map(p=> abs(p.y-(lr.getIntercept+p.x*lr.getSlope))) max
  }
	
  def totalErrorSqrSum={
    points.map(p=>pow(p.y-getValue(p.x),2)).sum
  }
  
  def getMagnitude()=
	sqrt(pow(h.right.x-h.left.x, 2)+pow(getValue(h.right.x)-getValue(h.left.x),2))
  
  def getValue(d:Double)={
	if (lr==null) regression(h)
	lr.getIntercept+lr.getSlope*d;
  }		
}



class Bucket(val beg:Int,val end:Int,val min:Double,val max:Double) {
	//int beg,end;
	//double max,min;
	var mergeErr:Double=0;
	var next:Bucket=_;
	var prev:Bucket=_;
	
	def getValue()={
		(max+min)/2;
	}
	
	def getError()={		
		Math.abs((max-min)/2);
	}
	
	
	
}