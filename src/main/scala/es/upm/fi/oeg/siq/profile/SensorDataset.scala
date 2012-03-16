package es.upm.fi.oeg.siq.profile

import es.upm.fi.oeg.siq.data.AemetSeries

abstract class SensorDataset{
  val props:Array[PropertySpec]
  lazy val propsMap=props.map(p=>p._3->p).toMap
  type PropertySpec = (String,Int,String)

  def distPath(typeData:String,code:String):String
  def dataPath(typeData:String,code:String):String
  def getCode(typeData:String,i:Int):String
  def getTags(code:String):Array[String]
}

class MixedDataset(val sets:SensorDataset* ) extends SensorDataset{
  val types=sets.map(s=>s.props.map(_._3).toList).reduceLeft(_:::_).toSet
  override val props=types.map(p=>sets.map(s=>s.propsMap.getOrElse(p,("",0,p)))
      .reduceLeft((a,b) =>("",a._2+b._2,a._3))).toArray
  val codes=types.map{p=>
    p->sets.map{s=>
    (1 to s.propsMap.getOrElse(p,("",0,""))._2).map(i=>(s,s.getCode(p,i))).toList      
    }.reduceLeft(_:::_).toArray
  }.toMap
  override def getCode(typeData:String,i:Int)=codes(typeData)(i-1)._2
  override def distPath(typeData:String,code:String)=
    codes(typeData).find(_._2.equals(code)).get._1.distPath(typeData,code)
  override def dataPath(typeData:String,code:String)=""
  override def getTags(code:String)=Array()
}

object SwissExDataset extends SensorDataset{
  val temp = ("Temperature/temperature_",78,"temperature")//78)
  val co2 = ("CO2/co2_",11,"co")//11)
  val humidity = ("Humidity/humidity_",34,"humidity")
  val lysimeter = ("Lysimeter/Lysimeter",12,"lysimeter")
  val moisture = ("Moisture/Moisture",20,"moisture")
  val pressure = ("Pressure/pressure_",4,"pressure")
  val radiation = ("Radiation/radiation_",34,"radiation")
  val snowheight = ("Snow_height/snow_height_",4,"snowheight")
  val voltage = ("Voltage/voltage_",16,"voltage")
  val windspeed = ("Wind_speed/wind_speed_",46,"windspeed")
  val winddir = ("Wind_direction/wind_direction_",35,"winddir")  
  val rootPath="g:/doc/semint/SensorAnnotation/"
  override val props = Array(temp,co2,humidity,lysimeter,moisture,pressure,
      radiation,snowheight,voltage,windspeed,winddir)
  override def distPath(typeData:String,code:String)=
    rootPath+"result_swissex/"+propsMap(typeData)._1+code+".csv"
  override def dataPath(typeData:String,code:String)=
    rootPath+"benchmark/"+propsMap(typeData)._1+code+".csv"
  override def getCode(typeData:String,i:Int)=i.toString
  override def getTags(code:String)=Array()
}

object AemetDataset extends SensorDataset{
  val temp = ("temperature/",100,"temperature")//78)
  val soiltemp=("soiltemp/",81,"soiltemp")
  val humidity = ("humidity/",100,"humidity")
  val pressure = ("pressure/",99,"pressure")
  val precipitation = ("precipitation/",100,"precipitation")
  val battery=("battery/",81,"battery")
  val windspeed = ("windspeed/",100,"windspeed")
  val windspeedmax=("windspeedmax/",100,"windspeedmax")
  val winddir = ("winddir/",100,"winddir")  
  val winddirmax=("winddirmax/",100,"winddirmax")
  val rootPath="g:/doc/semint/SensorAnnotation/"
  override val props = Array(temp,soiltemp,pressure,precipitation,humidity,
      battery,windspeed,windspeedmax,winddir,winddirmax)
  val codesperType=props.map(c=>c._3->AemetSeries.loadCodesPerType(c._3).zipWithIndex).toMap
  //override def getCode(i:Int)=i.toString
  override def getCode(typeData:String,i:Int)={
    println("index:" +i)
   codesperType(typeData).find(_._2==i-1).get._1 
  }    
  override def distPath(typeData:String,code:String)=
    rootPath+"result_aemet/"+typeData+"/"+code+".csv"
    
  override def dataPath(typeData:String,code:String)=
    rootPath+"benchmark/aemet/"+typeData+"/"+code+".csv"
  override def getTags(code:String)=Array()
  //val props1=Array(co2,pressure,radiation)//,temp,winddir,windspeed,humidit,)
  //val props2=Array(temp,humidity,lysimeter,moisture,radiation,winddir,windspeed)//,temp,humidity,lysimeter,moisture)//,//,
  //val props3=Array(windspeed)//radiation,snowheight,temp,voltage,winddir,windspeed)//)//,//,
}

