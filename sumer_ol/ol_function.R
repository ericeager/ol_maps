# this is the function for the OL stuff

ol_function <- function(data_frame, table, player, position) {
 plot <- ggplot(data_frame %>% filter(displayName == player, pff_positionLinedUp == position), 
         aes(y - ball_snap_y, x - ball_snap_x, group = as.factor(paste(gameId, playId)), color = as.factor(over_under), 
             linetype = as.factor(beaten)), size = 2) + geom_smooth(se = FALSE) + 
    geom_point(aes(y_mean, x_mean), color = "red", size = 5) + 
    scale_colour_manual(values = c("gold", "black")) + 
    theme_minimal() + labs(x = "horizontal distance from position at snap", y = "vertical distance from position at snap", 
                           color = "Did He Reach Depth?", 
                           linetype = "Was He Beaten (PFF)?",
                           title = paste("Pass Blocking Map for", player))
    
    player_table <- table %>% filter(displayName == player, pff_positionLinedUp == position) %>%
      select(new_frameId, x_mean_player = x_mean, y_mean_player = y_mean) 
    table <- table %>% filter(displayName != player, pff_positionLinedUp == position) 
    
    table <- table %>% inner_join(player_table, by = "new_frameId") %>% 
      group_by(displayName) %>% summarize(dist = round(sqrt(sum((x_mean - x_mean_player)^2 + (y_mean - y_mean_player)^2)), 2)) %>%
      arrange(dist)
    colnames(table) <- c("Player", "Average Curve Distance")
      
    return(list(plot = plot, table = table))
}