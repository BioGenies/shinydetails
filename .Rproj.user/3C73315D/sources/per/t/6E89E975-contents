#' @importFrom igraph "graph_from_adjacency_matrix"
#' @importFrom igraph "clusters"

#' @title Preparation of lambda
#' @description  Prepares penalty parameters \eqn{\lambda} for further computations in graphical SLOPE.
#' @param lambda a vector of SLOPE regularizers.
#' @param low_tri_size a number of elements in lower (upper) triangle of a variance-covariance matrix excluding diagonal.\cr
#' Should be equal to \eqn{p * (p-1)/2}, where p in a number of variables (columns) in the data.
#' @keywords lambda
#' @return \code{prepare_lambda} returns a sorted descending vector of the length \eqn{p * (p-1)/2}.
#' @details In case of vector lambda being too short, it is filled with zeros up to the set length. If the vector is too long, it is cut to the proper size.
#' @examples
#' v <- sample(1:100,10)
#' prepare_lambda(v,10)
#' prepare_lambda(v,5)
#' prepare_lambda(v,17)
#' @export

prepare_lambda = function(lambda, low_tri_size) {
  lambda = sort(lambda, decreasing = T)
  if(length(lambda) < low_tri_size) {
    warning("The length of lambda is less than ncol(data) * (ncol(data)-1)/2.
        Zeros will be added.", immediate. = FALSE)
    lambda = c(lambda, rep(0, low_tri_size - length(lambda)))
  }
  else if(length(lambda) > low_tri_size) {
    warning("The length of lambda is greater than ncol(data) * (ncol(data)-1)/2.
        Lambda will be cut to the proper length.", immediate. = FALSE)
    lambda = lambda[1:low_tri_size]
  }
  lambda
}


#' @title Graphical SLOPE
#' @description  Computes precision matrix for graph models using graphical SLOPE.
#'
#' @param data a matrix containing observations of variables of interest.
#' @param lambda vector of regularizers for SLOPE. By default computed based on Benjamini-Hochberg's method.
#' @param sample_cov variance-covariance matrix.
#' @param scaled {logical. The data need to be scaled so that it has mean = 0 and variance = 1. If TRUE, build-in data scaling will be omitted.}
#' @param mu correction for lambda scaling in ADMM algorithm.
#' @param max_iter maximum number of iterations allowed in ADMM algorithm. Default 10 000.
#' @param threshold {a value used to determine which variables in graphical model are not linked by an edge. The larger the value, the more variables will be unlinked. Default 10e-4.}
#' @param epsilon a value used to determine accuracy of the ADMM algorithm. Default 10e-4.
#' @param alpha significance level.
#'
#' @keywords precision matrix, graphical slope
#'
#' @return {returns a list containing following components:
#' \itemize{
#' \item \code{precision_matrix} a precision matrix revealing graph structure for the data.
#' \item \code{covariance_matrix} covariance matrix equal to the inverse of the presicion matrix.
#' \item \code{scaled_precision_matrix} {An element with coordinates (k,l) is given by formula:\cr
#' k_lm=-Cov(X_l,X_m|X_(V\ \{l,m\}))/(Var(X_l|X_(V\ \{l,m\}))^(1/2)*Var(X_m|X_(V\ \{l,m\}))^(1/2)).}
#' \item \code{lambda} a vector of penalty parameters used in SLOPE.
#' \item \code{iterations} a number of iterations performed in ADMM algorithm.
#' \item \code{graph} an IGRAPH object returning set of edges between vertices in the model. For more details see ?igraph
#' \item \code{clusters} {a sub-graph structure. Numbers associeted with vertex indicates which sub-graph the vertex belongs to.
#' If all verices are marked with ones, then there is only one graph in the model. Size an number of clusters are additionaly
#' provided in separate vectors. For more details see ?clusters.}
#' }}
#'
#' @details \code{gslope} selects high probability graph structure for graphical model with likelihood-based methods combined with ordered L1-regularization. Namely, it solves - using ADMM algorithm - the following  maximization problem:
#' \deqn{ log det \Theta - tr(S \Theta) - \lambda(\Theta), subject to \Theta \in S+,}
#' where S is a sample covariance matrix, \eqn{\lambda(\Theta)} is a series of regularizers for SLOPE and S+ denotes a set of symmetric, semidefinite matrices.
#' @examples
#' gslope(mtcars, epsilon = 1e-3)
#' @references Makowski, M., (2018).
#' Precision matrix estimation in Gaussian graphical models.
#' Master's Thesis. Uniwersytet Wroclawski.
#' @export


gslope = function(data,
                  lambda = NULL,
                  scaled = FALSE,
                  mu = 1.1,
                  max_iter = 1e4,
                  epsilon = 1e-4,
                  threshold = 1e-4,
                  alpha = 0.05) {

  call = match.call()
  if(is.null(colnames(data)))
    names = 1:ncol(data) else
      names = colnames(data)
  sample_cov = if(!scaled) cov(scale(data)) else cov(data)
   if(is.null(lambda)) {
    lambda = gslope::create_lambda(sample_cov, nrow(data), alpha)
  }

  p = ncol(data)
  lambda = prepare_lambda(lambda, p*(p-1)/2)

  ADMM_results = ADMM_algorithm(sample_cov, lambda, mu, max_iter, epsilon)

  precision_matrix = ADMM_results[[1]]
  rownames(precision_matrix) = names
  colnames(precision_matrix) = names


  scaled_precision_matrix = -cov2cor(precision_matrix)
  scaled_precision_matrix[abs(scaled_precision_matrix) < threshold] = 0

  graph = graph_from_adjacency_matrix(scaled_precision_matrix,
                                      mode = c("undirected"),
                                      weighted = TRUE,
                                      diag = FALSE,
                                      add.colnames = NULL,
                                      add.rownames = NA)

  result = list(precision_matrix = precision_matrix,
                covariance_matrix = solve(precision_matrix),
                scaled_precision_matrix = scaled_precision_matrix,
                lambda = lambda,
                iterations = ADMM_results[[2]],
                graph = graph,
                clusters = clusters(graph),
                call = call)
  class(result) <- "gslope"
  result
}


