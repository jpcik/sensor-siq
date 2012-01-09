package es.upm.fi.oeg.siq.data

abstract class Means{
  def distance(item:Means):Double
  //def add(item:Means):Means
  def mean(list:Iterable[Means]):Means
  def mkString:String
}
case class EmptyMeans extends Means{
  override def distance(item:Means)=Double.MaxValue
  override def mean(list:Iterable[Means])=new EmptyMeans
  override def mkString="Empty"
}