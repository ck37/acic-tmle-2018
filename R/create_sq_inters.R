
# function to create the list to be fed into sl3 learner
create_sq_inters = function(inter_names, sq_names=NULL) {
  combos = combn(1:length(inter_names),2)
  int_vars = lapply(1:ncol(combos), FUN = function(col) {
    c(inter_names[combos[,col][1]], inter_names[combos[,col][2]])
  })
  int_names = lapply(int_vars, FUN = function(x) {
    paste0(x[1], x[2])
  })
  int_terms = int_vars
  names(int_terms) = int_names
  if (!is.null(sq_names)) {
    sq_vars = lapply(sq_names, FUN = function(x) {
      rep(x, 2)
    })
    square_names = lapply(sq_vars, FUN = function(x) {
      paste0(x[1], x[2])
    })
    sq_terms = sq_vars
    names(sq_terms) = square_names
  } else sq_terms = list()
  
  interactions_plus_sq = append(int_terms, sq_terms)
  return(interactions_plus_sq)
}

# interactions_plus_sq = create_sq_inters(colnames(data)[c(1,3:6)], colnames(data)[3:6])

# make the learner for the built-out matrix in the pipeline--first step of pipeline
# lrnr_interactions_squares = Lrnr_define_interactions$new(interactions = interactions_plus_sq)

