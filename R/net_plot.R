#' Network Plot of Connectedness
#'
#' Draws a network graph from a connectedness matrix, optionally filtered by the
#' Planar Maximally Filtered Graph (PMFG). Node sizes reflect total connectivity,
#' and node pies show the share of "FROM" vs "TO" connectivity.
#'
#' @param x A square connectedness matrix with the last row/column possibly containing totals.
#'   The function uses the top-left \eqn{p \times p} block where \eqn{p = ncol(x) - 1}.
#' @param Cnames Character vector of node labels (length \eqn{p}).
#' @param use_pmfg Logical; if \code{TRUE}, apply \code{\link[NetworkToolbox]{PMFG}}
#'   to the normalized matrix before plotting. Default \code{TRUE}.
#' @param layout Character; passed to \code{\link[qgraph]{qgraph}}. Common options:
#'   \code{"circle"} (default when \code{use_pmfg = FALSE}) or \code{"spring"}.
#'   Ignored if \code{use_pmfg = TRUE} (PMFG supplies the graph structure).
#' @param groups Optional vector/list of group memberships (for node coloring in \code{qgraph}).
#' @param div Positive number controlling edge thresholding when \code{use_pmfg = FALSE}.
#'   Larger values keep fewer edges (top \eqn{p^2/div}). Default \code{5}.
#' @param node.size Base node size multiplier. Default \code{20}.
#' @param edge Base edge width (passed to \code{qgraph} as \code{esize} and \code{asize}). Default \code{3}.
#' @param pie.colour Length-2 character vector for the pie colors (FROM, TO). Default \code{c("red","yellow")}.
#'
#' @details
#' The function normalizes the \eqn{p \times p} connectivity matrix by its sum, sets
#' the diagonal to zero, computes node sizes as the normalized sum of in- and out-going
#' strengths, and constructs a two-slice pie per node where the first slice is
#' the "FROM" share and the second is the "TO" share.
#'
#' If \code{use_pmfg = TRUE}, edges are filtered via the PMFG and \code{layout} is ignored.
#' Otherwise, the top \eqn{p^2/div} edges (by weight) are retained via \code{qgraph}'s
#' \code{threshold} argument.
#'
#' @return A \code{qgraph} object (invisibly prints the plot).
#'
#' @examples
#' \dontrun{
#' # Suppose 'TAB' is a connectedness TABLE with p+1 rows/cols (last is totals)
#' # and 'labs' is a character vector of length p:
#' g <- net_plot(TAB, Cnames = labs, use_pmfg = FALSE, layout = "circle")
#' }
#'
#' @importFrom qgraph qgraph
#' @importFrom NetworkToolbox PMFG
#' @export
net_plot <- function(x, Cnames, use_pmfg = TRUE, layout = "circle",
                     groups = NULL, div = 5, node.size = 20, edge = 3,
                     pie.colour = c("red", "yellow")) {

  p <- ncol(x) - 1L
  if (p <= 0) stop("x must have at least 2 columns (p+1 with totals).")

  # extract p x p core, coerce numeric matrix
  con <- as.matrix(x)[1:p, 1:p, drop = FALSE]
  storage.mode(con) <- "double"
  diag(con) <- 0

  # normalize weights
  s <- sum(con, na.rm = TRUE)
  if (s <= 0) stop("Sum of connectivity is non-positive.")
  W <- con / s

  # PMFG branch
  if (isTRUE(use_pmfg)) {
    net <- NetworkToolbox::PMFG(W)  # adjacency with PMFG edges
    thr <- NULL
    lay <- NULL
  } else {
    net <- W
    # keep top p^2/div edges; compute numeric threshold safely
    k <- max(1L, ceiling((p * p) / div))
    thr <- sort(as.vector(W), decreasing = TRUE, na.last = NA)[k]
    lay <- layout
  }

  # node sizes (sum out + in), normalized
  Nsize <- rowSums(con, na.rm = TRUE) + colSums(con, na.rm = TRUE)
  if (all(Nsize == 0)) {
    Nsize[] <- 1
  }
  Nsize <- Nsize / sum(Nsize)

  # pies: FROM vs TO shares
  NET <- abs(con)
  from_to <- rbind(colSums(NET, na.rm = TRUE), rowSums(NET, na.rm = TRUE))  # TO, FROM
  denom <- from_to[1, ] + from_to[2, ]
  n1 <- ifelse(denom > 0, from_to[2, ] / denom, 0)  # FROM share
  n1[!is.finite(n1)] <- 0

  g <- qgraph::qgraph(
    net,
    layout      = lay,
    groups      = groups,
    repulsion   = 0.8,
    palette     = "colorblind",
    labels      = Cnames,
    label.font  = 2,
    label.cex   = 1,
    shape       = "circle",
    label.color = "black",
    node.width  = Nsize * node.size,
    esize       = edge,
    borders     = FALSE,
    asize       = edge,
    threshold   = thr,
    legend      = FALSE,
    pie         = n1,
    pieColor    = pie.colour[1],
    pieColor2   = pie.colour[2]
  )

  g
}
