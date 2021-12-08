import io.github.edouardfouche.generators.Independent
import logic.index.RankIndex

val data = Independent(9,0,"gaussian",0).generate(1000)
val m = new RankIndex(data)

val groups_of_dims = Set(Set(0,1,2), Set(3,4,5,6), Set(7,8))

val dim_vec = groups_of_dims.flatten.toVector

val num_dims = dim_vec.size

val dim_slice_size_dict = dim_vec.map(d => {
  val num_dims_to_slice = groups_of_dims.filter(g => !g.contains(d)).flatten.size
  d -> (math.pow(0.5, 1.0 / num_dims_to_slice) * m.num_obs).ceil.toInt
}) toMap

// Map(0 -> Set(5, 6, 2, 3, 4), 5 -> Set(0, 1, 2, 3), 1 -> Set(5, 6, 2, 3, 4),
// 6 -> Set(0, 1, 2, 3), 2 -> Set(0, 5, 1, 6, 4), 3 -> Set(0, 5, 1, 6, 4), 4 -> Set(0, 1, 2, 3))
val dim_slice_dimensions_dict = dim_vec.map(d => {
  d -> groups_of_dims.filter(g => !g.contains(d)).flatten
}) toMap

val MC_num = 50
// for example 50 MC iterations with 8 dimensions: 50/8 = 6 head iterations
val head_great_iteration_num: Int = MC_num / num_dims
// 6*8 = 48 small MC iterations
val head_MC_number: Int = head_great_iteration_num * num_dims
// tail MC number is then 50 - 48 = 2
val tail_MC_number: Int = MC_num - head_MC_number