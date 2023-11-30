# this is the function for the OL stuff

ol_function <- function(data_frame, player, position, TTT_l, TTT_u, play_action, beaten, depth, 
                        success, cluster, N_0) {
  
  if (cluster == "Deep") {
    data_frame <- data_frame %>% filter(cluster == 1)
    
  } else if (cluster == "Shallow") {
    data_frame <- data_frame %>% filter(cluster == 2)
  }
  
  if (success == "yes") {
    data_frame <- data_frame %>% filter(success == 1)
  } else if (success == "no") {
    data_frame <- data_frame %>% filter(success == 0)
  }
  
 if (play_action == "yes") {
   data_frame <- data_frame %>% filter(pff_playAction == 1)
 } else if (play_action == "no") {
   data_frame <- data_frame %>% filter(pff_playAction == 0)
 }
  
  if (beaten == "yes") {
    data_frame <- data_frame %>% filter(beaten == "yes")
  } else if (beaten == "no") {
    data_frame <- data_frame %>% filter(beaten == "no")
  }
  
  if (depth == "yes") {
    data_frame <- data_frame %>% filter(over_under == "yes")
  } else if (depth == "no") {
    data_frame <- data_frame %>% filter(over_under == "no")
  }
  
  
 data_frame <- data_frame %>% filter(pff_positionLinedUp == position, time_to_throw >= TTT_l, time_to_throw <= TTT_u, 
                                     new_frameId <= 20) 
 
 table <- data_frame %>% 
    group_by(nflId, displayName, pff_positionLinedUp, new_frameId) %>%
    summarize(n = n(), y_mean = mean(y - ball_snap_y), x_mean = mean(x - ball_snap_x)) %>%
   ungroup()
 
 table_n <- table %>% group_by(nflId, displayName, pff_positionLinedUp) %>%
   summarize(N = max(n)) %>%
   filter(N >= N_0)
 
 bezier_function <- function(locations) {
   if (nrow(locations) > 0) {
   
   t = seq(0, 1, by = 1/(nrow(locations) - 1))
   p = matrix(c(locations$y_mean, locations$x_mean), ncol = 2)
   
   return(data.frame(locations$new_frameId, bezier(t = t, p = p)))
   } else {return()}}
 
 table <- table %>% tidyr::nest(.by = c("nflId", "displayName", "pff_positionLinedUp")) %>% 
   mutate(bezier_curve = future_map(data, bezier_function)) %>%
   tidyr::unnest(bezier_curve) %>%
   select(-data) %>%
   as.data.frame() %>%
   as.matrix() %>%
   as.data.frame() 
 
colnames(table) <- c("nflId", "displayName", "pff_positionLinedUp", 
                     "new_frameId", "y_mean", "x_mean") 

table <- table %>% mutate(nflId = as.integer(nflId), 
                          new_frameId = as.integer(new_frameId),
                          y_mean = as.numeric(y_mean), 
                          x_mean = as.numeric(x_mean))

 data_frame <- data_frame %>% left_join(table, by = c("nflId", "displayName", 
                                                      "pff_positionLinedUp", "new_frameId"))
 
rates <- data_frame %>% group_by(displayName) %>% summarize(win = round(100*mean(beaten == "no"), 1),  
                                                      reach_depth = round(100*mean(over_under == "yes"), 1)) %>%
  filter(displayName == player)

 plot <- ggplot(data_frame %>% filter(displayName == player), 
         aes(y - ball_snap_y, x - ball_snap_x, group = as.factor(paste(gameId, playId)), color = as.factor(over_under)), 
         size = 2) + geom_smooth(se = FALSE) + 
    scale_colour_manual(values = c("gold", "black")) + 
    xlim(-2.5, 2.5) + ylim(-6.5, 1) + 
    theme_minimal() + labs(x = "horizontal distance from position at snap",
                           y = "vertical distance from position at snap", 
                           color = "Did He Reach Exp. Depth?", 
                           title0 = paste0("Pass Blocking Map for ", player, ": Win Rate is ", rates$win[1], 
                                          "%, Reached Depth ", rates$reach_depth[1], "%")) +
   geom_path(aes(y_mean, x_mean), color = "red", size = 3, se = FALSE) +
   theme(text = element_text(size=15))
 
# plot <- ggplot(data_frame %>% filter(displayName == player), 
#                aes(y - ball_snap_y, x - ball_snap_x, group = as.factor(paste(gameId, playId)), color = as.factor(over_under)), 
#                size = 2) + geom_smooth(se = FALSE) + 
#   scale_colour_manual(values = c("black", "black")) + 
#   theme_minimal() + labs(x = "horizontal distance from position at snap",
#                          y = "vertical distance from position at snap", 
#                          title = paste("Pass Blocking Map for", player)) +
#   geom_path(aes(y_mean, x_mean), color = "red", size = 3, se = FALSE) +
#   theme(text = element_text(size=15))

    
    player_table <- table %>% filter(displayName == player, pff_positionLinedUp == position) %>%
      select(new_frameId, x_mean_player = x_mean, y_mean_player = y_mean)
    
    table <- table %>% filter(displayName != player, pff_positionLinedUp == position) %>%
      inner_join(table_n, by = c("nflId", "displayName", "pff_positionLinedUp"))
    
    frechet_function <- function(locations) {
      if (nrow(locations) > 0) {
        return(Frechet(matrix(c(locations$y_mean, locations$x_mean), ncol = 2),
                       matrix(c(locations$y_mean_player, locations$x_mean_player), ncol = 2)))
      } else {return()}}
    
    table <- table %>% inner_join(player_table, by = "new_frameId") %>% 
      tidyr::nest(.by = c("nflId", "displayName", "pff_positionLinedUp")) %>%
      mutate(dist = future_map(data, frechet_function)) %>%
      select(-data) %>%
      as.data.frame() %>%
      mutate(dist = as.numeric(dist)) %>%
      mutate(dist = round(1/dist, 2)) %>%
      arrange(-dist)
    colnames(table) <- c("Player_id", "Player", "Position", "Average Curve Similarity")
      
    return(list(plot = plot, table = table))
}