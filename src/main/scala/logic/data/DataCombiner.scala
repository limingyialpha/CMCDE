package logic.data

import io.github.edouardfouche.generators.GeneratorFactory

object DataCombiner {
  def generate_combine(n:Int,
                       name1:String, dim1:Int, noise1:Double,
                       name2:String, dim2:Int, noise2:Double,
                       param1: Option[Double] = None, param2: Option[Double] = None): Array[Array[Double]] ={
    val gen1 = GeneratorFactory.get(name1,dim1,noise1,"gaussian",0,param1)
    val gen2 = GeneratorFactory.get(name2,dim2,noise2,"gaussian",0,param2)
    val data1 = gen1.generate(n)
    val data2 = gen2.generate(n)
    data1.zip(data2).map(tuple => tuple._1 ++ tuple._2)
  }
}
