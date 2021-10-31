//val vec = Vector(4,2,3,1,4,2,3,1).zipWithIndex
////val vec = Vector(1,2,5,5,3,3,5,5,6).zipWithIndex
//val size = 4
//val array = (1 to size).map(i =>{
//  val selected_vec = vec.filter(x => x._2 % size == i-1).map(x => x._1)
//  selected_vec.sum
//}).zipWithIndex.sortBy(x => x._1).map(x=> x._2)
//
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

import logic.data.Utility.cumulative_sum

def get_tail_bins(num_dims: Int, tail_MC_num: Int): Vector[Int]={
  (0 until tail_MC_num).map(x => {
    val overflow = if (x < num_dims % tail_MC_num) 1 else 0
    (overflow + num_dims/tail_MC_num)
  }).toVector
}

val bins = get_tail_bins(100,7)
cumulative_sum(bins)