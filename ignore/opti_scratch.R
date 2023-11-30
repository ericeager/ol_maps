opti_path <- function(data.frame, new_frameId, mod, x_delta = 0.2, y_delta = 0.1) {
  
  build_grid <- data.frame %>%
    select(gameId, playId, nflId, down, yardsToGo, field_pos, is_home, 
           ball_snap_x, ball_snap_y,
           x_prev = x, 
           y_prev = y, 
           pff_positionLinedUp, 
           defendersInBox, pff_playAction,
           dropBackType,
           offenseFormation) %>%
    mutate(new_frameId = new_frameId + 1)
  
  
  grid_xy <- expand.grid(x = seq(data.frame$x - x_delta, data.frame$x + x_delta, by = x_delta/10),
                         y = seq(data.frame$y - y_delta, data.frame$y + y_delta, by = y_delta/10))
  
  grid <- build_grid %>% cross_join(grid_xy) %>%
    mutate(x_rel = x - ball_snap_x, 
           y_rel = y - ball_snap_y, 
           x_prev_rel = x_prev - ball_snap_x, 
           y_prev_rel = y_prev - ball_snap_y)
  #grid[, "pred_win"] <- predict(mod, grid, type = "prob")[1]
  #grid[, "pred_win"] <- predict(mod, grid, type = "response")
  grid[, "pred_win"] <- predict(mod, grid)
  
  return(grid %>% arrange(-pred_win, x_rel) %>% head(1) %>%
           select(-x_rel, -y_rel, -x_prev_rel, -y_prev_rel))
}


data.frame <- pbp %>% filter(gameId == 2021102800, 
                                    playId == 189, 
                                    nflId == 38779, 
                                    new_frameId == 1) %>%
  select(gameId, playId, nflId, down, yardsToGo, field_pos, is_home, ball_snap_x, 
         ball_snap_y, x_prev, y_prev, pff_positionLinedUp, 
         defendersInBox, pff_playAction, dropBackType, 
         offenseFormation, new_frameId, x,y) %>%
  mutate(pred_win = NA)

test <- pbp %>% filter(gameId == 2021102800, 
                       playId == 189, 
                       nflId == 38779)

traj <- data.frame

for (i in 1:nrow(test)) {
data.frame <- opti_path(data.frame, i, fit_lm, 0.2, 0.1)
traj <- rbind(data.frame, traj)
}

test <- test %>% left_join(traj %>% select(new_frameId, x, y), by = "new_frameId")
head(test %>% select(x.x, y.x, x.y, y.y))

#fit_opti <- train(beaten ~ new_frameId + 
#                    down + yardsToGo + field_pos + 
#                    is_home + 
#                    ball_snap_x + ball_snap_y + 
#                    x_prev + y_prev + 
#                    x + y + 
#                    pff_positionLinedUp +  
#                    defendersInBox + pff_playAction + 
#                    dropBackType + offenseFormation, 
#                  data = pbp, method = "xgbTree", trControl = ControlParamteres,
#                  tuneGrid = parametersGrid)

#varImp(fit)
#varImp(fit_opti)

#df <- data.frame(
#  down = 1,
#  yardsToGo = 10, 
#  field_pos = 75, 
#  is_home = 1, 
#  pass_away = "no",
#  ball_snap_x = 21.08,
#  ball_snap_y = 20.21,
#  x_prev = 21.14, 
#  y_prev = 20.24, 
#  x = 21.08,
#  y = 20.21,
#  pff_positionLinedUp = "RT",
#  defendersInBox = 5,
#  pff_playAction = 0,
#  dropBackType = "0",
#  offenseFormation = "EMPTY")

#x_delta <- 0.2
#y_delta <- 0.1






