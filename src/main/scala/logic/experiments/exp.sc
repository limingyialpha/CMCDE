import io.github.edouardfouche.generators.DataGenerator
import logic.data.DataCombiner
import logic.generators.IndependentLinearIterate
//val vec = Vector(4,2,3,1,4,2,3,1).zipWithIndex
////val vec = Vector(1,2,5,5,3,3,5,5,6).zipWithIndex
//val size = 4
//val array = (1 to size).map(i =>{
//  val selected_vec = vec.filter(x => x._2 % size == i-1).map(x => x._1)
//  selected_vec.sum
//}).zipWithIndex.sortBy(x => x._1).map(x=> x._2)

//def sum_slices(vec: Vector[Double], dims_vec: Vector[Int]): Vector[Int] = {
//  val zipped = vec.zipWithIndex
//  val num_dims = dims_vec.size
//  (0 until num_dims).map(i =>{
//    val selected_vec = zipped.filter(x => x._2 % num_dims == i).map(x => x._1)
//    selected_vec.sum
//    //zip with
//  }).zip(dims_vec).sortBy(x => x._1).map(x=> x._2).toVector
//}
//
//val vec = Vector(1.0,2,5,5,3,3,5,5,6)
//val dims_vec = Vector(4,3,7)
//sum_slices(vec, dims_vec)

//
//val dims = 29
//val iterations = 4
//val resize_array = (0 until iterations).map(x => {
//  val overflow = if (x < dims % iterations) 1 else 0
//  overflow + dims/iterations
//})
//import logic.data.Utility.cumulative_sum
//val vector = Vector(1,2,3,4,5,6,7)
//cumulative_sum(vector)
//vector.slice(0,7)

//import logic.data.Utility.cumulative_sum
//
//def get_tail_bins(num_dims: Int, tail_MC_num: Int): Vector[Int]={
//  (0 until tail_MC_num).map(x => {
//    val overflow = if (x < num_dims % tail_MC_num) 1 else 0
//    (overflow + num_dims/tail_MC_num)
//  }).toVector
//}
//
//val bins = get_tail_bins(100,7)
//cumulative_sum(bins)
//
//
//val generators: Vector[DataGenerator] = Vector(
//  IndependentLinearIterate(1,0.1,"gaussian",0)
//)
//
//var combined_data_points: Map[Array[Array[Double]], String] = Map(
//  DataCombiner.generate_combine(1,"i",1,0,"l",1,0.1) -> "independent-50-0-linear-50-0.1"
//)
//
//for (generator <- generators) {
//  combined_data_points += (generator.generate(1) -> generator.id)
//}
//combined_data_points

val x = List("S","iterate_tail_MC","fds",2)