


create_oc_curve = function(criterion, lot_size, num_points = 100L, ...) {
  stopifnot(exprs = {
    vek::is_int_vec_x1(lot_size)
    vek::is_int_vec_x1(criterion)
    vek::is_int_vec_x1(num_points)
    #vek::is_num_vec_xyz1(aql)
    #vek::is_num_vec_xyz1(lql)
    #0 < aql && aql < 1
    #0 < lql && lql < 1
    #aql < lql
    num_points >= 100L
  })
  
  true_p = seq(0, 1, length.out = num_points)
  p_accept = pbinom(criterion, lot_size, true_p, lower.tail = TRUE)
  
  df = data.frame(true_p = true_p, p_accept = p_accept)
  
  plot(true_p, p_accept, type = "l", ...)
}
