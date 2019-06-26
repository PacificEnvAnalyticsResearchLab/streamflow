composite_medoid <- function(data, index = F, ...) {

  data_matrix <- as.matrix(data, rownames.force = FALSE)


  cid_dist <- ciddm(m = data_matrix)
  sax_dist <- saxdm(m = data_matrix)
  comp_dist <- sax_dist + log(cid_dist + 1)


  dm <- as.matrix(comp_dist)
  names(dm) <- 1:ncol(dm)
  row.names(dm) <- 1:nrow(dm)



  not_inf <- which(dm[1, ] != Inf)
  dm <- dm[not_inf, not_inf]
  sum_dm <- apply(X = dm, MARGIN = 1, FUN = sum)


  centers <- which(sum_dm == min(sum_dm, na.rm = T))
  if (length(centers) > 1) {
    warning("Multiple equally minimal medoids exist (", length(centers), ").")
  }
  med <- as.numeric(names(centers))

  if (index == T)
  {
    message("Returning only the center index.")
    return(med)
  }
  else
  {
    message("Checking if multiple centers exist.")
    if (length(centers) == 1)
    {
      message("Returning one center vector.")
      return(t(data_matrix[centers, ]))
    }
    else
    {
      message("Returning multiple center vectors.")
      return(data_matrix[centers, ])
    }
  }
}
