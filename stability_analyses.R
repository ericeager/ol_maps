# this is deeper analysis of the models
library(SimilarityMeasures)
pbp_slim <- read.csv("C:/Users/eric/Dropbox/ol_maps/sumer_ol/wrangled_pbp.csv", stringsAsFactors = FALSE)

# creating averages curves
table <- pbp_slim %>% 
  group_by(nflId, displayName, pff_positionLinedUp, new_frameId) %>%
  summarize(y_mean = mean(y - ball_snap_y), x_mean = mean(x - ball_snap_x)) %>%
  ungroup() 

# smoothing functions
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

table <- table %>% mutate(nflId = as.numeric(nflId), 
                          new_frameId = as.numeric(new_frameId))

pbp_slim <- pbp_slim %>% inner_join(table, by = c("nflId", "displayName", "pff_positionLinedUp", "new_frameId"))

# distance function
frechet_function <- function(locations) {
  if (!is.null(locations)) {
    return(Frechet(matrix(c(locations$y, locations$x), ncol = 2),
                   matrix(c(locations$y_mean, locations$x_mean), ncol = 2)))
  } else {return()}}

pbp_dist <- pbp_slim %>% select(season, week, gameId, playId, nflId, displayName, pff_positionLinedUp, new_frameId, x, y, x_mean, y_mean) %>%
  mutate(x = as.numeric(x), y = as.numeric(y), x_mean = as.numeric(x_mean), y_mean = as.numeric(y_mean)) %>%
  tidyr::nest(.by = c("season", "week", "gameId", "playId", "nflId", "displayName", "pff_positionLinedUp")) %>%
  mutate(dist = future_map(data, frechet_function)) %>%
  select(-data) %>%
  as.data.frame() %>%
  mutate(dist = as.numeric(dist)) 

# writing distance function into 
write.csv(pbp_dist, "frechet_distances.csv", row.names = FALSE)

x <- pbp_slim %>% filter(week <= 4) %>% inner_join(pbp_dist) %>%
  group_by(season, week, nflId, displayName, team, gameId, playId) %>% 
  summarize(over_under = mean(over_under == "yes", na.rm = TRUE), 
            beaten = mean(beaten == "yes", na.rm = TRUE), 
            dist = mean(dist)) %>% 
  group_by(nflId, displayName) %>% 
  summarize(n = n(), depth = mean(over_under, na.rm = TRUE), beaten = mean(beaten), 
            diff = mean(dist)) %>% arrange(-diff) %>% filter(n >= 100) %>% as.data.frame() 

y <- pbp_slim %>% filter(week >= 5) %>% inner_join(pbp_dist) %>%
  group_by(season, week, nflId, displayName, team, gameId, playId) %>% 
  summarize(over_under = mean(over_under == "yes", na.rm = TRUE),
            beaten = mean(beaten == "yes", na.rm = TRUE), 
            dist = mean(dist)) %>% 
  group_by(nflId, displayName) %>% 
  summarize(n = n(), depth = mean(over_under, na.rm = TRUE), beaten = mean(beaten), 
            diff = mean(dist)) %>% arrange(-diff) %>% filter(n >= 100) %>% as.data.frame() 

# Evaluation
XY <- inner_join(x,y, by = c("nflId", "displayName"))
cor(XY$diff.x, XY$diff.y)
cor(XY$depth.x, XY$depth.y)
cor(XY$beaten.x, XY$beaten.y)

ggplot(XY, aes(diff.x, diff.y)) + geom_point(size = 2) + 
  geom_smooth(method = 'lm', se = FALSE, size = 2) +
  theme_minimal() + theme(legend.position = "none") + 
  ggtitle("Stability of Pass Blocking Path Deviation") + 
  labs(x = "Average Path Deviation Weeks 1-4", 
       y = "Average Path Deviation Weeks 5-8") +
  theme(text = element_text(size=15))

ggplot(XY, aes(depth.x, depth.y)) + geom_point(size = 2) + 
  geom_smooth(method = 'lm', se = FALSE, size = 2) +
  theme_minimal() + theme(legend.position = "none") + 
  ggtitle("Stability of Reaching Expected Depth") + 
  labs(x = "Reach Depth Pct. Weeks 1-4", 
       y = "Reach Depth Pct. Weeks 5-8") +
  theme(text = element_text(size=15))

ggplot(XY, aes(1 - beaten.x, 1 - beaten.y)) + geom_point(size = 2) + 
  geom_smooth(method = 'lm', se = FALSE, size = 2) +
  theme_minimal() + theme(legend.position = "none") + 
  ggtitle("Stability of Win Rate in Pass Protection") + 
  labs(x = "Win Rate Weeks 1-4", 
       y = "Win Rate Weeks 5-8") +
  theme(text = element_text(size=15))

x <- pbp_slim %>% inner_join(pbp_dist) %>%
  group_by(gameId, playId, nflId, displayName, team, pff_positionLinedUp) %>% 
  summarize(over_under = mean(over_under == "yes", na.rm = TRUE),
            beaten = mean(beaten == "yes", na.rm = TRUE), 
            dist = mean(dist))

# A quick evaluation of beaten versus depth
glm(beaten ~ as.factor(over_under) + dist + pff_positionLinedUp, data = x) %>% summary()

x <- pbp_slim %>% filter(week <= 4) %>% inner_join(pbp_dist) %>%
  group_by(season, week, nflId, displayName, team, gameId, playId) %>% 
  summarize(over_under = mean(over_under == "yes", na.rm = TRUE), 
            beaten = mean(beaten == "yes", na.rm = TRUE), 
            dist = mean(dist)) %>% 
  group_by(nflId, displayName) %>% 
  summarize(n = n(), depth = mean(over_under, na.rm = TRUE), beaten = mean(beaten), 
            diff = mean(dist)) %>% arrange(-diff) %>% filter(n >= 100) %>% as.data.frame() 

# probably need a curve for each time to throw threshold?
# probably need an optimal curve for each time to throw threshold?


