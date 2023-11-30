# clustering algorithms
library(dplyr); library(tidyr); library(tictoc)
set.seed(124)
data_frame <- read.csv("sumer_ol/wrangled_pbp.csv", stringsAsFactors = FALSE)

example <- data_frame %>% filter(pff_positionLinedUp == "C")
trajectory_data <- example

# ancillary function
calc_new_beta <- function(W, X, Y){
  #W <- diag(weights)
  Beta_new <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Y
  return(Beta_new)
}

# ancillary function
calc_new_sigma <- function(W, X, Y, Beta){
  #W <- diag(weights)
  Sigma_new <- diag(diag(as.matrix(Matrix::t(Y - X %*% Beta) %*% W %*% (Y - X %*% Beta) / sum(W))))
}


# ancillary function
reorder_clusters <- function(em_results){
  
  
  # Save old Data
  Pik   <- em_results$Pik
  Beta  <- em_results$Beta
  Sigma <- em_results$Sigma
  Alpha <- em_results$Alpha
  K <- ncol(Pik)
  
  # reorder by depth
  reordered <-
    tibble::tibble(Beta, Sigma, Alpha) %>%
    dplyr::mutate(rank = dplyr::dense_rank(-Alpha),
                  row  = dplyr::row_number()) %>%
    dplyr::arrange(rank)
  
  # Reorder all of the variables
  row <-
    reordered %>%
    dplyr::pull(row)
  
  Pik <- Pik[, row]
  colnames(Pik) <- 1:K
  
  
  Beta <-
    reordered %>%
    dplyr::pull(Beta)
  
  Sigma <-
    reordered %>%
    dplyr::pull(Sigma)
  
  Alpha <-
    reordered %>%
    dplyr::pull(Alpha)
  
  em_results <-
    list("l_hood" = em_results$l_hood,
         "Pik"    = Pik,
         "Beta"   = Beta,
         "Sigma"   = Sigma,
         "Alpha"   = Alpha)
  
  return(em_results)
  
}

# clustering function
cluster_trajectory_data <- function(trajectory_data, P = 3, K = 5, niter = 20){
  
  # create data for em algorithm
  prepared_trajectory_data <-
    trajectory_data %>% select(-new_frameId) %>%
    dplyr::mutate(x = x - ball_snap_x, y = y - ball_snap_y) %>%
    tidyr::nest(data = c(x, y)) %>%
    dplyr::mutate(curve_i = row_number()) %>%
    dplyr::mutate(n_i = purrr::map_dbl(data, nrow),
                  t_i = purrr::map(n_i, ~ tibble::tibble(t = (1:. - 1)/(.-1))),
                  T_i = purrr::map(t_i, ~ dplyr::mutate(., p = list(0:P)) %>%
                                     tidyr::unnest(cols = c(p)) %>%
                                     dplyr::mutate(D_p_of_t = choose(P, p) * t^p * (1 - t)^(P - p)) %>%
                                     tidyr::spread(p, D_p_of_t, sep = "_") %>%
                                     dplyr::select(-t))) %>%
    dplyr::select(data, curve_i, n_i, t_i, T_i) %>%
    dplyr::arrange(curve_i) %>%
    ungroup()
  
  # create X matrix
  X <-
    prepared_trajectory_data %>%
    dplyr::select(T_i) %>%
    tidyr::unnest(cols = c(T_i)) %>%
    Matrix::as.matrix()
  
  # create Y matrix
  Y <-
    prepared_trajectory_data %>%
    dplyr::select(data) %>%
    tidyr::unnest(cols = data) %>%
    Matrix::as.matrix()
  
  SEQ <-
    prepared_trajectory_data %>%
    dplyr::select(n_i) %>%
    dplyr::mutate(n_i = cumsum(n_i)) %>%
    Matrix::as.matrix()
  
  INDEX <-
    prepared_trajectory_data %>%
    dplyr::select(curve_i, t_i) %>%
    tidyr::unnest(cols = c(t_i)) %>%
    dplyr::select(curve_i)
  
  ##################### INIT
  
  
  kmean_data <-
    prepared_trajectory_data %>%
    dplyr::transmute(ends = purrr::map(data, ~ filter(., row_number() == max(row_number())) %>% dplyr::select(x, y)),
                     ends = purrr::map(data, ~ summarise(., x = mean(x), y = mean(y)))) %>%
    tidyr::unnest(cols = c(ends))
  
  
  kmeans_results <- kmeans(kmean_data, centers = K, iter.max = 100)
  
  # kmean_data %>%
  #   dplyr::mutate(cluster = kmeans_results$cluster) %>%
  #   ggplot2::ggplot(aes(x = x, y = y, colour = factor(cluster))) +
  #   ggplot2::geom_point() +
  #   ggplot2::theme_bw()
  # 
  # K means
  init_clusters <-
    prepared_trajectory_data %>%
    dplyr::mutate(cluster = kmeans_results$cluster) 
  
  # random
  # init_clusters <-
  #   prepared_trajectory_data %>%
  #   dplyr::mutate(cluster = sample(rep(1:K, ceiling(nrow(.) / K)), size = nrow(.), replace = F))
  
  
  Alpha <-
    init_clusters %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(prop = n/sum(n)) %>%
    dplyr::pull(prop)
  
  c <- 
    init_clusters  %>%
    tidyr::unnest(data) %>%
    dplyr::select(cluster) %>% 
    dplyr::pull(cluster)
  
  
  calc_Piik <- function(data, Sigma){
    data %>%
      transmute(Piik = purrr::pmap_dbl(list(x, y, x1, y1), ~ emdbook::dmvnorm(x = c(..3, ..4),
                                                                              mu = c(..1, ..2),
                                                                              Sigma))) %>%
      dplyr::bind_cols(as_tibble(INDEX))
  }
  
  Beta <- list()
  Sigma <- list()
  tic()
  for(k in 1:K){
    Beta[[k]] <- solve(t(X[k == c, ]) %*% X[k == c, ]) %*% t(X[k == c, ]) %*% Y[k == c, ]
    Sigma[[k]] <- diag(diag(t(Y[k == c, ] - X[k == c, ] %*% Beta[[k]]) %*% (Y[k == c, ] - X[k == c, ] %*% Beta[[k]])/ nrow(X[k == c, ])))
  }
  toc()
  
  n <- length(SEQ)
  N <- SEQ[n]
  curve_lengths <- prepared_trajectory_data %>% dplyr::select(n_i)
  
  #################################################################################
  l_hood <- -Inf
  tic()
  
  
  for(i in 1:niter){
    print(i)
    
    #############################################################################
    # Expectation Step
    print("e_step time")
    tic()
    
    data_Piik <-
      tibble::tibble(Beta, Sigma) %>%
      dplyr::mutate(k = row_number()) %>%
      #partition() %>%
      dplyr::mutate(X_Beta = purrr::map(Beta,
                                        ~ Matrix::as.matrix(X) %*% .x %>%
                                          tibble::as_tibble() %>%
                                          dplyr::bind_cols(rename(tibble::as_tibble(Matrix::as.matrix(Y)), x1 = x, y1 = y)))) %>%
      dplyr::mutate(Piik = purrr::map2(X_Beta, Sigma, calc_Piik)) %>%
      dplyr::select(k, Piik) %>%
      #collect() %>%
      tidyr::unnest(cols = c(Piik))
    
    scale_m <-
      data_Piik %>%
      dplyr::ungroup() %>%
      dplyr::summarise(mean = mean(Piik)) %>%
      dplyr::pull(mean)
    
    Pik <-
      data_Piik %>%
      group_by(curve_i) %>%
      dplyr::mutate(Piik = Piik/mean(Piik)) %>%
      dplyr::group_by(curve_i, k) %>%
      dplyr::summarise(Pik = prod(Piik))  %>%
      tidyr::spread(k, Pik) %>%
      dplyr::ungroup() %>%
      dplyr::select(-curve_i) %>%
      Matrix::as.matrix()
    
    Pik[is.infinite(Pik) & Pik > 0] <-.Machine$double.xmax
    
    Pik <- Pik * Alpha
    
    toc()
    #############################################################################
    
    # Calculate Log Likelihood
    
    # Calculate Probability of data over all clusters
    s <- rowSums(Pik)
    
    # Since we're not on the log scale we might get 0
    if(any(s == 0)){
      # replace 0 with the smallest number possible
      # then weight by the alphas
      Pik[s == 0, ] <- .Machine$double.xmin * Alpha
      # recalculate the probability of observing this data over all clusters
      s <- rowSums(Pik)
    }
    
    # Now calculate the new log likelihood
    l_hood_new <- sum(log(s)) + N * log(scale_m)
    
    # If we've reached our tolerance stop the loop
    if(i> 1 & abs(l_hood_new - l_hood)/l_hood < 1e-6){
      break
    }
    
    # For monitoring
    #print(l_hood)
    # overwrite the old log likelihood
    l_hood <- l_hood_new
    
    # Calculate the Pi_ik
    Pik <- Pik/s
    
    # Perform Maximization Step
    
    print("m_step time")
    tic()
    #############################################################################
    Alpha <- colSums(Pik) / n
    
    
    weights <-
      Pik %>%
      tibble::as_tibble() %>%
      dplyr::mutate(curve_i = row_number()) %>%
      dplyr::right_join(INDEX, by = "curve_i") %>%
      dplyr::select(matches("\\d")) %>%
      as.list()
    
    param_updates <-
      tibble::tibble(k = 1:K, weights = weights) %>%
      dplyr::mutate(weights = purrr::map(weights, ~ Matrix::Diagonal(x = .))) %>%
      #partition() %>%
      dplyr::mutate(Beta_new  = purrr::map(weights, ~ calc_new_beta(., X, Y)),
                    Sigma_new = purrr::map2(weights, Beta_new, ~ calc_new_sigma(.x, X, Y, .y)),
                    Beta_new  = purrr::map(Beta_new, as.matrix)) %>%
      #collect() %>%
      dplyr::arrange(k)
    
    Beta <- param_updates %>% dplyr::pull(Beta_new)
    Sigma <- param_updates %>% dplyr::pull(Sigma_new)
    
    #############################################################################
    toc()
    
    
    
  }
  toc()
  
  em_results <-
    list("l_hood" = l_hood_new,
         "Pik"    = Pik,
         "Beta"   = Beta,
         "Sigma"  = Sigma,
         "Alpha"  = Alpha)
  
  em_results <-
    em_results %>%
    reorder_clusters()
  
  return(list(nested = trajectory_data %>% tidyr::nest(data = c(new_frameId, x, y)), 
              init_clusters = init_clusters$cluster, em_results = em_results))
}

# building clusters
x <- cluster_trajectory_data(trajectory_data %>%
                               filter(new_frameId <= decel_frameId, 
                                      new_frameId <= 35), P = 3, K = 2, niter = 100)
data <- cbind(x$nested, as.data.frame(x$init_clusters))
colnames(data)[ncol(data)] <- "cluster"
data <- data %>% as.data.frame() %>% 
  select(season, week, gameId, playId, nflId, cluster)

#saving clusters
data_frame <- inner_join(example, data)
write.csv(data_frame, "C_clusters.csv", row.names = FALSE)

data_frame <- read.csv("C_clusters.csv")
averages <- data_frame %>% group_by(cluster, new_frameId) %>% 
  summarize(x_mean = mean(x - ball_snap_x), y_mean = mean(y - ball_snap_y)) %>%
  as.data.frame() %>%
  ungroup()

#ancillary function
bezier_function <- function(locations) {
  if (nrow(locations) > 0) {
    
    t = seq(0, 1, by = 1/(nrow(locations) - 1))
    p = matrix(c(locations$y_mean, locations$x_mean), ncol = 2)
    
    return(data.frame(locations$new_frameId, bezier(t = t, p = p)))
  } else {return()}}

averages <- averages %>% tidyr::nest(.by = c("cluster")) %>% 
  mutate(bezier_curve = future_map(data, bezier_function)) %>%
  tidyr::unnest(bezier_curve) %>%
  select(-data) %>%
  as.data.frame() %>%
  as.matrix() %>%
  as.data.frame()

colnames(averages) <- c("cluster", "new_frameId", 
                        "x_mean", "y_mean")

averages <- averages %>% filter(new_frameId <= 33)

#data_frame <- read.csv("LT_clusters.csv")
data_frame <- data_frame %>% left_join(averages)

library(ggplot2)
# if we need to change things
#data_frame <- data_frame %>% mutate(cluster = ifelse(cluster == 1, 2, 1))
ggplot(data_frame %>% mutate(cluster_nar = ifelse(cluster == 1, "First Cluster", "Second Cluster")), 
               aes(y - ball_snap_y, x - ball_snap_x, 
                   color = as.factor(cluster),
                   group = as.factor(paste(gameId, playId))), #color = as.factor(over_under)), 
               size = 2) + geom_point() + # geom_smooth(se = FALSE) + 
  scale_colour_manual(values = c("gold", "black")) + 
 # scale_colour_manual(values = c("gold", "black")) + 
  theme_minimal() + labs(x = "horizontal distance from position at snap",
                         y = "vertical distance from position at snap") + 
  ggtitle("Centers") + 
  facet_wrap(~cluster_nar) + theme(legend.position="none") +
  geom_path(aes(x_mean, y_mean), color = "red", size = 3) 
 # geom_path(aes(y_mean, x_mean), color = "red", size = 3, se = FALSE)

agg_x <- data_frame %>% filter(week <= 5) %>% select(playId, nflId, cluster, beaten) %>% 
  unique() %>% group_by(nflId, cluster) %>% summarize(n = n(), win = mean(beaten == "no"))
agg_y <- data_frame %>% filter(week > 5) %>% select(playId, nflId, cluster, beaten) %>% 
  unique() %>% group_by(nflId, cluster) %>% summarize(n = n(), win = mean(beaten == "no"))

agg_big_x <- data_frame %>% filter(week <= 5) %>% select(playId, nflId, cluster, beaten) %>% 
  unique() %>% group_by(nflId) %>% summarize(n_tot = n(), win_tot = mean(beaten == "no"))
agg_big_y <- data_frame %>% filter(week > 5) %>% select(playId, nflId, cluster, beaten) %>% 
  unique() %>% group_by(nflId) %>% summarize(n_tot = n(), win_tot = mean(beaten == "no"))

agg_x <- agg_x %>% left_join(agg_big_x) %>% mutate(n = n/n_tot, win_ratio = win/win_tot)
agg_y <- agg_y %>% left_join(agg_big_y) %>% mutate(n = n/n_tot, win_ratio = win/win_tot)

agg_xy <- full_join(agg_x, agg_y, by = c("nflId", "cluster"))
agg_xy[is.na(agg_xy)] <- 0

agg_xy <- agg_xy %>% filter(n_tot.x >= 50, n_tot.y >= 50)
CLUST <- 1
cor(filter(agg_xy, n.x >= 1/6, n.y >= 1/6, cluster == CLUST)$n.x, filter(agg_xy, n.x >= 1/6, n.y >= 1/6, cluster == CLUST)$n.y)
cor(filter(agg_xy, n.x >= 1/6, n.y >= 1/6, cluster == CLUST)$win.x, filter(agg_xy, n.x >= 1/6, n.y >= 1/6, cluster == CLUST)$win.y)
nrow(filter(agg_xy, n.x >= 1/6, n.y >= 1/6, cluster == CLUST))

CLUST <- 2
cor(filter(agg_xy, n.x >= 1/6, n.y >= 1/6, cluster == CLUST)$n.x, filter(agg_xy, n.x >= 1/6, n.y >= 1/6, cluster == CLUST)$n.y)
cor(filter(agg_xy, n.x >= 1/6, n.y >= 1/6, cluster == CLUST)$win.x, filter(agg_xy, n.x >= 1/6, n.y >= 1/6, cluster == CLUST)$win.y)
nrow(filter(agg_xy, n.x >= 1/6, n.y >= 1/6, cluster == CLUST))

C_clusters <- read.csv("C_clusters.csv", stringsAsFactors = FALSE)
LG_clusters <- read.csv("LG_clusters.csv", stringsAsFactors = FALSE)
RG_clusters <- read.csv("RG_clusters.csv", stringsAsFactors = FALSE)
LT_clusters <- read.csv("LT_clusters.csv", stringsAsFactors = FALSE)
RT_clusters <- read.csv("RT_clusters.csv", stringsAsFactors = FALSE)

wrangled_pbp <- rbind(C_clusters, LG_clusters, RG_clusters, LT_clusters, RT_clusters)
write.csv(wrangled_pbp, "sumer_ol/wrangled_pbp_w_clusters.csv", row.names = FALSE)