import logic.index.RankIndex

import scala.language.postfixOps

def get_slice_size_without_ref_dim(dims_set: Set[Int], num_obs: Int): Int = {
  (math.pow(0.5, 1.0 / dims_set.size) * num_obs).ceil.toInt
}

def get_dim_slice_size_dict(m: RankIndex, groups_of_dims: Set[Set[Int]]): Map[Int, Int] = {
  groups_of_dims.flatten.map(d => {
    val dims_to_slice = groups_of_dims.filter(g => !g.contains(d)).flatten
    d -> get_slice_size_without_ref_dim(dims_to_slice, m.num_obs)
  }) toMap
}

def get_dim_slice_dims_dict(groups_of_dims: Set[Set[Int]]): Map[Int, Set[Int]] = {
  groups_of_dims.flatten.map(d => {
    d -> groups_of_dims.filter(g => !g.contains(d)).flatten
  }) toMap
}

val dgs = Set(Set(0,1,2,3), Set(4,5), Set(6,7,8))
get_dim_slice_dims_dict(dgs)