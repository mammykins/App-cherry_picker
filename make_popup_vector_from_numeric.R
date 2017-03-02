# POPUP DIAGNOSIS ---------------------------------------------------------
# Useful if your Geography is bad
# 1 for longitude, 2 for latitude
make_popup_vector_from_numeric <- function(la_polygon, long1_or_lat2) {
  stopifnot(long1_or_lat2 == 1 | 2)
  stopifnot(class(la_polygon[[1]]) == "Polygons")
  output <- numeric(length = 152)
  
  for (i in 1:152) {
    output[i] <- la_s_ll@polygons[[i]]@labpt[long1_or_lat2]
  }
  return(output)
}


# TEST --------------------------------------------------------------------
# make_popup_vector_from_numeric(la_s_ll@polygons, 1)
# as.character(la_s_ll@data$LEA_NAME)