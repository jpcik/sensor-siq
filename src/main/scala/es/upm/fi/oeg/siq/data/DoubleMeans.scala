package es.upm.fi.oeg.siq.data


class DoubleMeans(val n:Double) extends Means{
  override val companion=DoubleMeans 
  override def distance(item:Means)=item match {
    case EmptyMeans() => Double.MaxValue
    case a:DoubleMeans => math.abs(n-a.n)}
  override def mkString=" "+n    
}

object DoubleMeans extends MeansUtils{
  override def means(list:Iterable[Means])=
    new DoubleMeans(list.map{case a:DoubleMeans => a.n}.sum/list.size) }
