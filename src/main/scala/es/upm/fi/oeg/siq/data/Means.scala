package es.upm.fi.oeg.siq.data

abstract class Means {
  val companion:MeansUtils
  def distance(item:Means):Double
  def mkString:String
}

trait MeansUtils{
  def means(list:Iterable[Means]):Means
}

case class EmptyMeans extends Means{
  override def distance(item:Means)=Double.MaxValue
  override def mkString="Empty"
  override val companion=EmptyMeans  
}

object EmptyMeans extends MeansUtils{
  def means(list:Iterable[Means])=new EmptyMeans 
}