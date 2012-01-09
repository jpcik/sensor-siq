object Demo {


def main(args:Array[String]) = {
    val someClassTrait:Object = Class.forName("SomeClass").newInstance().asInstanceOf[Object]
}  
}
class SomeClass extends Object{
  def method = "something"
}