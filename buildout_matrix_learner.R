
gendata = function(n, g0, c0, Q0) {
  W1 = runif(n, -3, 3)
  W2 = rnorm(n)
  W3 = runif(n)
  W4 = rnorm(n)
  z = rbinom(n, 1, g0(W1, W2, W3, W4))
  C = rbinom(n, 1, c0(z, W1, W2, W3, W4))
  y = rbinom(n, 1, Q0(z, W1, W2, W3, W4))
  data.frame(z, C, W1, W2, W3, W4, y)
}


g0_linear = function(W1, W2, W3, W4) {
  plogis(0.5 * (-0.8 * W1 + 0.39 * W2 + 0.08 * W3 - 0.12 *
                  W4 - 0.15))
}

c0 = function(z, W1, W2, W3, W4) {
  plogis(0.5 * (-0.4 * W1 + 0.3 * W2^2 + 0.06 * abs(W3) - 0.30 *
                  .3*z*W4 -.5*W4 + 1-.2*z))
}

Q0_1 = function(z, W1, W2, W3, W4) {
  plogis(0.14 * (2 * z + 3 * z * W1 + 6 * z * W3 * W4 + W2 *
                   W1 + W3 * W4 + 10 * z * cos(W4)))
}

n=1000
data = gendata(n, g0_linear, c0, Q0_1)
head(data)

# function to create the list to be fed into sl3 learner
create_sq_inters = function(inter_names, sq_names) {
  combos = combn(1:length(inter_names),2)
  int_vars = lapply(1:ncol(combos), FUN = function(col) {
    c(inter_names[combos[,col][1]], inter_names[combos[,col][2]])
  })
  int_names = lapply(int_vars, FUN = function(x) {
    paste0(x[1], x[2])
    })
  int_terms = int_vars
  names(int_terms) = int_names

  sq_vars = lapply(sq_names, FUN = function(x) {
    rep(x, 2)
  })
  
  square_names = lapply(sq_vars, FUN = function(x) {
    paste0(x[1], x[2])
  })
  
  sq_terms = sq_vars
  names(sq_terms) = square_names
  
  interactions_plus_sq = append(int_terms, sq_terms)
  return(interactions_plus_sq)
}

interactions_plus_sq = create_sq_inters(colnames(data)[c(1,3:6)], colnames(data)[3:6])

# make the learner for the built-out matrix in the pipeline--first step of pipeline
lrnr_interactions_squares = Lrnr_define_interactions$new(interactions = interactions_plus_sq)


task = make_sl3_Task(data = data, covariates = colnames(data[c(1,3:6)]), outcome = "y")
lrnr_glm = make_learner(Lrnr_glm)
int_pipeline = make_learner(Pipeline, lrnr_interactions_squares, lrnr_glm)

int_pipeline_fit = int_pipeline$train(task)
int_pipeline_fit$predict()[1:10]

# check with regular chaining by hand
int_fit = lrnr_interactions_squares$train(task)
int_task = int_fit$chain()
int_glm_fit = lrnr_glm$train(int_task)
int_glm_fit$coefficients
int_glm_fit$predict()[1:10]



