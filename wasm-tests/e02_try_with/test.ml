let safe_inverse n =
  try Some (1/n)
  with Division_by_zero -> None
