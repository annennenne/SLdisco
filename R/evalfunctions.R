

halfskel <- function(amat) {
  out <- amat + t(amat) != 0
  as.numeric(out[lower.tri(out)])
}

adj_fp <- function(est_halfskel, true_halfskel) {
  sum(est_halfskel == 1 & true_halfskel == 0)
}

adj_fn <- function(est_halfskel, true_halfskel) {
  sum(est_halfskel == 0 & true_halfskel == 1)
}

adj_tp <- function(est_halfskel, true_halfskel) {
  sum(est_halfskel == 1 & true_halfskel == 1)
}

adj_tn <- function(est_halfskel, true_halfskel) {
  sum(est_halfskel == 0 & true_halfskel == 0)
}

adj_confusion <- function(est_amat, true_amat) {
 # browser()
  est_halfskel <- halfskel(est_amat)
  true_halfskel <- halfskel(true_amat)
  
  list(tp = adj_tp(est_halfskel, true_halfskel), 
       tn = adj_tn(est_halfskel, true_halfskel), 
       fp = adj_fp(est_halfskel, true_halfskel),  
       fn = adj_fn(est_halfskel, true_halfskel))
  
}

precision <- function(confusion) {
  tp <- confusion$tp
  fp <- confusion$fp
  ifelse(tp + fp != 0, tp/(tp + fp), 1)
}

recall <- function(confusion) {
  tp <- confusion$tp
  fn <- confusion$fn
  ifelse(tp + fn != 0, tp/(tp + fn), 1)
}

specificity <- function(confusion) {
  tn <- confusion$tn
  fp <- confusion$fp
  ifelse(tn + fp != 0, tn/(tn + fp), 1)
}

FOR <- function(confusion) {
  fn <- confusion$fn
  tn <- confusion$tn
  ifelse(fn + tn != 0, fn/(fn + tn), 1)
}

FDR <- function(confusion) {
  fp <- confusion$fp
  tp <- confusion$tp
  ifelse(fp + tp != 0, fp/(fp + tp), 1)
}

NPV <- function(confusion) {
  tn <- confusion$tn
  fn <- confusion$fn
  ifelse(tn + fn != 0, tn/(tn + fn), 1)
}

F1 <- function(confusion) {
  tp <- confusion$tp
  fp <- confusion$fp
  fn <- confusion$fn
  ifelse(tp + fp + fn != 0, 2*tp/(2*tp + fp + fn), 1)
}

G1 <- function(confusion) {
  tn <- confusion$tn
  fn <- confusion$fn
  fp <- confusion$fp
  ifelse(tn + fn + fp != 0, 2*tn/(2*tn + fn + fp), 1)
}

fp <- function(confusion) {
  confusion$fp
}

fn <- function(confusion) {
  confusion$fn
}

tp <- function(confusion) {
  confusion$tp
}

tn <- function(confusion) {
  confusion$tn
}


nedges <- function(est_halfskel) {
  sum(est_halfskel)
}

shd_old <- function(est_amat, true_amat) {
  est_graph <- as.graphNEL(est_amat)
  true_graph <- as.graphNEL(true_amat)
  pcalg::shd(est_graph, true_graph)
}



edges <- function(amat) {
  p <- nrow(amat)
  edgeL <- lapply(split(amat, rep(1:p, each = p)), function(x) which(x == 1))
  
  out <- list()
  for (i in 1:p) {
    children <- edgeL[[i]]
    nchild <- length(children) 
    if (nchild > 0) {
      for (j in 1:nchild) {
        out <- c(out, list(c(i, children[j])))
      }
    }
  }
  oneway <- list()
  bothways <- list()
  if (length(out) > 0) {
    revout <- lapply(out, rev) 
    bothways <- base::intersect(revout, out)
    if (length(bothways) > 0) {
      oneway <-  base::setdiff(out, bothways)
      bothways <- unique(lapply(bothways, sort))
    } else{
      oneway <- out
    }
  }
  list(`dir` = oneway, `undir` = bothways)
}


dir_confusion <- function(est_amat, true_amat) {
  est_edges <- edges(est_amat)
  true_edges <- edges(true_amat)
  
  true_adj <- c(true_edges$undir, true_edges$dir)
  true_adj <- c(true_adj, lapply(true_adj, rev))
  
  true_dir <- true_edges$dir
  true_revdir <- lapply(true_dir, rev)
  true_undir <- true_edges$undir
  
  est_dir <- est_edges$dir
  est_undir <- est_edges$undir
  
  dir_fp <- 0
  dir_fn <- 0
  dir_tp <- 0
  dir_tn <- 0
  
  #count metrics for undirected edges
  if (length(est_undir) > 0) {
    for (i in 1:length(est_undir)) {
      thisedge <- est_undir[i]
      if (thisedge %in% true_adj) {
        if (thisedge %in% true_undir) { #is correctly undirected
          dir_tn <- dir_tn + 1
        } else if (thisedge %in% c(true_dir, true_revdir)) { #is undirected, should be directed
          dir_fn <- dir_fn + 1
        }
      }
    }
  }
  
  #count metrics for directed edges
  if (length(est_dir) > 0) {
    for (i in 1:length(est_dir)) {
    thisedge <- est_dir[i]
    if (thisedge %in% true_adj) {
      if (thisedge %in% true_undir) { #is directed, should be undirected
        dir_fp <- dir_fp + 1 
      } else if (thisedge %in% true_dir) { #is directed in correct direction
        dir_tp <- dir_tp + 1
      } 
      if (thisedge %in% true_revdir) { #is directed in incorrect direction
        dir_fp <- dir_fp + 1
        dir_fn <- dir_fn + 1
      }
    }
    }
  }
 list(tp = dir_tp, tn = dir_tn,
      fp = dir_fp, fn = dir_fn)
}
  



average_degree <- function(amat) {
  p <- nrow(amat)
  sum(amat + t(amat) > 0)/p
}


average_degree_true <- function(true_amat, ...) {
  average_degree(true_amat)
}


average_degree_est <- function(est_amat, ...) {
  average_degree(est_amat)
}



nedges <- function(amat) {
  sum(halfskel(amat))
}


nedges_true <- function(true_amat, ...) {
  nedges(true_amat)
}


nedges_est <- function(est_amat, ...) {
  nedges(est_amat)
}

evaluate <- function(est, true, metrics) {
  UseMethod("evaluate")
}

evaluate.matrix <- function(est, true, metrics, df.out = TRUE) {
 #browser()
  adj <- metrics$adj
  dir <- metrics$dir
  other <- metrics$other
  
  n_adj <- length(adj)
  n_dir <- length(dir)
  n_other <- length(other)
  
  adj_metrics <- list(adj)
  dir_metrics <- list(dir)
  other_metrics <- list(other)
  
  adj_names <- dir_names <- other_names <- NULL
  
  if (n_adj > 0) {
    adj_conf <- adj_confusion(est, true)
    for (i in 1:n_adj) {
      adj_metrics[[i]] <- do.call(adj[i], list(confusion = adj_conf))
    }
    adj_names <- paste0("adj_", adj, sep = "")
    
  }
  if (n_dir > 0) {
    dir_conf <- dir_confusion(est, true)
    for (i in 1:n_dir) {
      dir_metrics[[i]] <- do.call(dir[i], list(confusion = dir_conf))
    }
    dir_names <- paste0("dir_", dir, sep = "")
  }
  if (n_other > 0) {
    for (i in 1:n_other) {
      other_metrics[[i]] <- do.call(other[i], list(est_amat = est, true_amat = true))
    }
   other_names <- other
  }
  if (df.out) {
    out <- unlist(c(adj_metrics, dir_metrics, other_metrics))
    names(out) <- c(adj_names, dir_names, other_names)
    return(out)
  } else {
    names(adj_metrics) <- adj
    names(dir_metrics) <- dir
    names(other_metrics) <- other
    return(list(adj = adj_metrics, dir = dir_metrics, other = other_metrics))
  }
}


evaluate.array <- function(est, true, metrics) {
  n <- dim(est)[1]
  p <- length(metrics$adj) + length(metrics$dir) + length(metrics$other)
  out <- matrix(NA, n, p)
  for (i in 1:n) {
    res <- evaluate.matrix(est = est[i, , ], true = true[i, , ], metrics = metrics)
    out[i, ] <- res
  }
  colnames(out) <- names(res)
  data.frame(out)
}


#based on pcalg shd function but takes in amats (avoids costly conversion to
#graphNEL)
shd <- function(est_amat, true_amat) {
  m1 <- est_amat
  m2 <- true_amat
  shd <- 0
  s1 <- m1 + t(m1)
  s2 <- m2 + t(m2)
  s1[s1 == 2] <- 1
  s2[s2 == 2] <- 1
  ds <- s1 - s2
  ind <- which(ds > 0)
  m1[ind] <- 0
  shd <- shd + length(ind)/2
  ind <- which(ds < 0)
  m1[ind] <- m2[ind]
  shd <- shd + length(ind)/2
  d <- abs(m1 - m2)
  shd + sum((d + t(d)) > 0)/2
}

