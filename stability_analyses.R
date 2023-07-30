# this is deeper analysis of the models

pbp_slim <- read.csv("C:/Users/eric/Dropbox/ol_maps/sumer_ol/wrangled_pbp.csv", stringsAsFactors = FALSE)

table <- pbp_slim %>% 
  group_by(nflId, displayName, pff_positionLinedUp, new_frameId) %>%
  summarize(y_mean = mean(y - ball_snap_y), x_mean = mean(x - ball_snap_x)) %>%
  ungroup()

pbp_slim <- pbp_slim %>% inner_join(table, by = c("nflId", "displayName", "pff_positionLinedUp", "new_frameId"))

x <- pbp_slim %>% filter(week <= 4) %>% group_by(season, week, nflId, displayName, team, gameId, playId) %>% 
  summarize(diff = sum(sqrt((y - ball_snap_y - y_mean)^2 + (x - ball_snap_x - x_mean)^2)), beaten = mean(beaten == "yes"), 
            over_under = mean(over_under == "yes", na.rm = TRUE)) %>% 
  group_by(nflId, displayName) %>% 
  summarize(n = n(), diff = mean(diff), depth = mean(over_under, na.rm = TRUE), beaten = mean(beaten)) %>% arrange(-diff) %>% filter(n >= 100) %>% as.data.frame() 

y <- pbp_slim %>% filter(week >= 5) %>% group_by(season, week, nflId, displayName, team, gameId, playId) %>% 
  summarize(diff = sum(sqrt((y - ball_snap_y - y_mean)^2 + (x - ball_snap_x - x_mean)^2)), beaten = mean(beaten == "yes"), 
            over_under = mean(over_under == "yes", na.rm = TRUE)) %>% 
  group_by(nflId, displayName) %>% 
  summarize(n = n(), diff = mean(diff), depth = mean(over_under, na.rm = TRUE), beaten = mean(beaten)) %>% arrange(-diff) %>% filter(n >= 100) %>% as.data.frame() 


XY <- inner_join(x,y, by = c("nflId", "displayName"))
cor(XY$diff.x, XY$diff.y)
cor(XY$depth.x, XY$depth.y)
cor(XY$beaten.x, XY$beaten.y)

ggplot(XY, aes(diff.x, diff.y)) + geom_point() + geom_smooth(method = 'lm')
ggplot(XY, aes(depth.x, depth.y)) + geom_point() + geom_smooth(method = 'lm')
ggplot(XY, aes(beaten.x, beaten.y)) + geom_point() + geom_smooth(method = "lm")

pbp_slim_test <- pbp_slim %>% filter(time_to_throw > 0.0, time_to_throw < 2.75) %>% group_by(season, week, nflId, displayName, team, gameId, playId, pff_positionLinedUp) %>% 
  summarize(diff = sum(sqrt((y - ball_snap_y - y_mean)^2 + (x - ball_snap_x - x_mean)^2)), beaten = mean(beaten == "yes"), 
            over_under = mean(over_under == "yes", na.rm = TRUE))

glm(beaten ~ as.factor(over_under) + diff + pff_positionLinedUp, data = pbp_slim_test) %>% summary()

# probably need a curve for each time to throw threshold?
# probably need an optimal curve for each time to throw threshold?


