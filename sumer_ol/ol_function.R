# this is the function for the OL stuff

ol_function <- function(data_frame, player, position, TTT_l, TTT_u, play_action, beaten, depth) {
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
  
  
 data_frame <- data_frame %>% filter(pff_positionLinedUp == position, time_to_throw >= TTT_l, time_to_throw <= TTT_u)
 
 table <- data_frame %>% 
    group_by(nflId, displayName, pff_positionLinedUp, new_frameId) %>%
    summarize(y_mean = mean(y - ball_snap_y), x_mean = mean(x - ball_snap_x)) %>%
   ungroup()
 
data_frame <- data_frame %>% inner_join(table, by = c("nflId", "displayName", "pff_positionLinedUp", "new_frameId")) 
  
 plot <- ggplot(data_frame %>% filter(displayName == player), 
         aes(y - ball_snap_y, x - ball_snap_x, group = as.factor(paste(gameId, playId)), color = as.factor(over_under)), 
         size = 2) + geom_smooth(se = FALSE) + 
    scale_colour_manual(values = c("gold", "black")) + 
    theme_minimal() + labs(x = "horizontal distance from position at snap",
                           y = "vertical distance from position at snap", 
                           color = "Did He Reach Exp. Depth?", 
                           title = paste("Pass Blocking Map for", player)) +
   geom_path(aes(y_mean, x_mean), color = "red", size = 3, se = FALSE)
    
    player_table <- table %>% filter(displayName == player, pff_positionLinedUp == position) %>%
      select(new_frameId, x_mean_player = x_mean, y_mean_player = y_mean)
    table <- table %>% filter(displayName != player, pff_positionLinedUp == position) 
    
    table <- table %>% inner_join(player_table, by = "new_frameId") %>% 
      group_by(displayName) %>% summarize(dist = sqrt(sum((x_mean - x_mean_player)^2 + (y_mean - y_mean_player)^2))) %>%
      arrange(dist) %>% mutate(dist = round(1/dist, 2))
    colnames(table) <- c("Player", "Average Curve Similarity")
      
    return(list(plot = plot, table = table))
}