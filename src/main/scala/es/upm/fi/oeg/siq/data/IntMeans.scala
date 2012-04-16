package es.upm.fi.oeg.siq.data

/*
 * This is a sample class implementing Means
 */
class IntMeans(val n:Int) extends Means{
  override val companion=IntMeans 
  override def distance(item:Means)=item match {
    case EmptyMeans() => Double.MaxValue
    case a:IntMeans => math.abs(n-a.n)}
  override def mkString=" "+n    
}

object IntMeans extends MeansUtils{
  override def means(list:Iterable[Means])=
    new IntMeans(list.map{case a:IntMeans => a.n}.sum/list.size) }

