remove_dupes <- function(moments) {
  moments[!duplicated(moments), ]
}

# Given the moments information for a game, extract the two
# global team ids
get_teams <- function(moments) {
  teams <- unique(moments$team)
  teams[teams != -1]
}

# Function that takes a game and determines when each
# team is on offense based on which side of the court 
# they are on. Does not work for a flipped court.
assign_offense <- function(game) {
  require(fields)
  
  N <- nrow(game)
  home_team <- as.numeric(substr(game$game[1], 9, 10))
  if (home_team == 30){
    home_team <- 5312
  }
  away_team <- unique(game$team)[which(unique(game$team) != home_team)]
  if (away_team == 30){
    away_team <- 5312
  }
  
  left_basket <- cbind(5.25,25)
  right_basket <- cbind(88.75,25)
  baskets <- rbind(left_basket,right_basket)
  
  get_shot_xy <- function(team, half) {
    shot_ids <- c(3, 4)
    shots <- game[game$event_id %in% shot_ids & game$team == team, ]
    if (half == 1) {
      cbind(mean(shots$x[shots$quarter < 3]), mean(shots$y[shots$quarter < 3]))
    } else {
      cbind(mean(shots$x[shots$quarter > 2]), mean(shots$y[shots$quarter > 2]))
    }
  }

  home_fg1_xy <- get_shot_xy(home_team, 1)
  away_fg1_xy <- get_shot_xy(away_team, 1)
  
  home_fg2_xy <- get_shot_xy(home_team, 2)
  away_fg2_xy <- get_shot_xy(away_team, 2)
  
  home_basket_1 <- which.min(rdist(rbind(home_fg1_xy, baskets))[2:3,1])
  away_basket_1 <- which.min(rdist(rbind(away_fg1_xy, baskets))[2:3,1])
  
  home_basket_2 <- which.min(rdist(rbind(home_fg2_xy, baskets))[2:3,1])
  away_basket_2 <- which.min(rdist(rbind(away_fg2_xy, baskets))[2:3,1])
  
  if (home_basket_1 == away_basket_1 | 
      home_basket_2 == away_basket_2 | 
      home_basket_1 == home_basket_2 | 
      away_basket_1 == away_basket_2) {
    print(game$game[1])
    stop("Baskets are mixed up.")
  }
  
  offense_basket <- rep("L", N)
  right_idx <- if (home_basket_1 == 1 & away_basket_1 == 2)  {
    which((game$team == home_team & game$quarter > 2) |
          (game$team == away_team & game$quarter < 3)) 
  } else {
    which((game$team == home_team & game$quarter < 3) |
          (game$team == away_team & game$quarter > 2))
  }
  offense_basket[right_idx] <- "R"
  game$basket <- offense_basket
  
  midcourt <- 47
  game$offense <- logical(N)
  game$offense[game$basket == "R" & game$x > midcourt] <- T
  game$offense[game$basket == "L" & game$x < midcourt] <- T

  game$defending_team_id[game$team == home_team & game$offense] <- away_team
  game$defending_team_id[game$team == away_team & game$offense] <- home_team
  game
}

# Note that this only works for unflipped court
remove_transition <- function(game) {
  remove_rows <- which((game$basket == "R" & game$x < 55 ) |
                         (game$basket == "L" & game$x > 39))
  game[-remove_rows, ]
}

compare_NA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  same
}

remove_fts <- function(game) {
  free_throw_idx <- game$event_id %in% c(1,2)
  game[!free_throw_idx, ]
}

flip_court <- function(game) {
  midcourt <- 47
  game$x <- game$x - midcourt
  flip <- game$x > 0
  game$x[flip] <- -game$x[flip]
  game$y[flip] <- -(game$y[flip] - 50)
  game$x <- game$x + midcourt
  game
}

mark_possessions <- function(moments) {
  # 21 - Dribble
  # 22 - Pass
  # 23 - Possession
  # 25 - Assist
  p_markers <- c(21, 22, 23, 25)
  
  # End of Possession Markers
  # 3 - Field Goal Made
  # 4 - Field Goal Missed
  # 5 - Offensive Rebound
  # 6 - Defensive Rebound
  # 7 - Turnover
  # 8 - Foul
  # 9 - Violation
  # 11 - Timeout
  # 12 - Jump Ball
  # 15 - End Period
  # 19 - Game Over
  # 24 - Blocked Shot
  # 28 - Defense OOB
  eop_markers <- c(3, 4, 5, 6, 7, 8, 9, 11, 12, 15, 19, 24, 28)
  
  N <- nrow(moments)
  is_poss <- T
  possession <- numeric(N)
  poss_id <- 1
  possession[1] <- poss_id
  for (i in 2:N) {
    if (is_poss & moments$event_id[i] %in% eop_markers) {
      possession[i] <- poss_id
      is_poss <- F
      poss_id <- poss_id + 1
    } else if (!is_poss & moments$event_id[i] %in% p_markers) {
      is_poss <- T
      possession[i] <- poss_id
    } else if (!is_poss & moments$event_id[i] %in% eop_markers) {
      possession[i] <- NA
    } else if (moments$team[i] != moments$team[i-1]) {
      poss_id <- poss_id + 1
      possession[i] <- poss_id
    } else {
      possession[i] <- poss_id
    }
  }
  moments$possession <- possession
  moments
}

mark_eop <- function(moments) {
  moments <- moments[!is.na(moments$possession), ]
  moments$eop <- c(diff(moments$possession), NA)
  moments$eop[c(diff(moments$team), 0) != 0] <- 1
  moments
}

# Classify locations into zones approximately the same
# as stats.nba's advanced zones. Note that this only works
# with a court that has already been flipped.
classify_zone <- function(game) {
  location_id <- rep(NA, nrow(game))
  
  x <- game$x
  y <- game$y
  y <- y - 25
  x <- x - 5.25
  root_xy <- sqrt(x^2 + y^2)
  location_id[root_xy < 8] <- 'hoop'
  location_id[root_xy < 16 & y < (-8/13.75)*x & is.na(location_id)] <- 'low-left'
  location_id[root_xy < 16 & y > (8/13.75)*x & is.na(location_id)] <- 'low-right'
  location_id[root_xy < 16 & is.na(location_id)] <- 'low-center'
  location_id[root_xy < 23.75 & y < (-19/13.75)*x & is.na(location_id)] <- 'mid-far-left'
  location_id[root_xy < 23.75 & y < (-5.5/13.75)*x & is.na(location_id)] <- 'mid-left'
  location_id[root_xy < 23.75 & y > (19/13.75)*x & is.na(location_id)] <- 'mid-far-right'
  location_id[root_xy < 23.75 & y > (5.5/13.75)*x & is.na(location_id)] <- 'mid-right'
  location_id[root_xy < 23.75 & is.na(location_id)] <- 'mid-center'
  location_id[y > 22  & x < 8.75] <- 'right-corner3'
  location_id[y < -22 & x < 8.75] <- 'left-corner3'
  location_id[is.na(location_id) & y > (6/13.75)*x] <- 'right-arc3'
  location_id[is.na(location_id) & y < (-6/13.75)*x] <- 'left-arc3'
  location_id[is.na(location_id)] <- 'middle-arc3'
  location_id[(sqrt(x^2 + y^2) >= 33.75)] <- 'heave'
  location_id[x < -5.25 | y < -25 | y > 25] <- 'oob'
  game$location_id <- factor(location_id)
  game
}

next_locations <- function(moments) {
  require(dplyr)
  # End of Possession Event Codes
  # 3 - Field Goal Made
  # 4 - Field Goal Missed
  # 5 - Offensive Rebound
  # 6 - Defensive Rebound
  # 7 - Turnover
  # 8 - Foul
  # 9 - Violation
  # 11 - Timeout
  # 12 - Jump Ball
  # 15 - End Period
  # 19 - Game Over
  # 24 - Blocked Shot
  # 28 - Defense OOB
  moments <- moments %>% 
    mutate(next_location = lead(location_id, 1),
           to_x = lead(x, 1), to_y = lead(y, 1))
  end_of_possession <- c(5, 6, 9, 12, 15, 19, 24, 28)
  moments$next_location <- c(as.character(moments$location_id[-1]), NA)
  moments$next_location[moments$event_id %in% c(3, 4)] <- 'shot'
  moments$next_location[moments$event_id == 7] <- 'turnover'
  moments$next_location[moments$event_id == 8] <- 'foul'
  moments$next_location[moments$event_id == 11] <- 'timeout'
  moments$next_location[moments$event_id %in% end_of_possession] <- 'eop' 
  moments
}



## Calculate Transition Matrix
calc_transmat <- function(moments, probs = T, include_end_states = F, NA.omit = F) {
  zones <- c('hoop', 'low-right', 'low-left', 'low-center', 'mid-far-right', 
             'mid-right', 'mid-far-left', 'mid-left', 'mid-center', 
             'left-corner3', 'right-corner3', 'left-arc3', 'right-arc3',
             'middle-arc3')
  end_states <- c('shot', 'turnover', 'foul', 'timeout')
  zones_plus <- if (include_end_states) c(zones, end_states) else zones
  t_mat <- matrix(0, nrow=length(zones), ncol=length(zones_plus))
  colnames(t_mat) <- zones_plus
  rownames(t_mat) <- zones
  for (i in 1:length(zones)) {
    for (j in 1:length(zones_plus)) {
      t_mat[i, j] <- sum(paste(moments$location_id, moments$next_location) == paste(zones[i], zones_plus[j]))
    }
    if (probs) {
      t_mat[i, ] <- if (NA.omit & sum(t_mat[i, ]) == 0) {
        rep(0, length(length(zones_plus)))
      } else {
        round(t_mat[i, ] / sum(t_mat[i, ]), digits=4)
      }
    }
  }
  t_mat
}

# Given two TPM's, this plots the difference between them
plot_diff <- function(tpm1, tpm2, g1_name, g2_name) {
  diff_df <- as.data.frame(tpm1 - tpm2)
  diff_df$location <- factor(rownames(tpm1), levels=rownames(tpm1))
  diff_df <- melt(diff_df)
  p <- ggplot(diff_df, aes(variable, location)) +
    geom_tile(aes(fill=value), colour="white") +
    scale_fill_gradient2("Diff.", low="steelblue", mid = "white", high="firebrick") +
    theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1)) +
    xlab("Destination") + 
    ylab("Origin") +
    ggtitle(paste(g1_name, "-", g2_name))
  p
}

plot_tpm <- function(tpm, title) {
  require("reshape2")
  df <- as.data.frame(tpm)
  df$location <- factor(rownames(tpm), levels=rownames(tpm))
  df <- melt(df)
  p <- ggplot(df, aes(variable, location)) +
    geom_tile(aes(fill=value), colour="white") +
    scale_fill_gradient2("Value", low="steelblue", mid = "white", high="firebrick", midpoint = mean(tpm)) +
    theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1)) +
    xlab("Destination") + 
    ylab("Origin") +
    ggtitle(title)
  print(p)
}

prepare_moments <- function(moments) {
  moments <- assign_offense(moments)
  moments <- flip_court(moments)
  moments <- remove_transition(moments)
  moments <- remove_fts(moments)
  moments <- classify_zone(moments)
  moments
}

markov_like <- function(tpm, tcm, log = T) {
  tpm[tpm == 0] <- 1e-6
  if (log) {
    sum(tcm * log(tpm))
  } else {
    prod(tpm^tcm)
  }
}

log_loss <- function(tpm, tcm) {
  tpm <- tpm + 1e-16
  for (i in 1:nrow(tpm)) {
    tpm[i, ] <- tpm[i, ] / sum(tpm[i, ])
  }
  N <- sum(tcm)
  -1 / N * sum(tcm * log(tpm))
}

get_logos <- function(size) {
  require(png)
  require(grid)
  hawks <- rasterGrob(readPNG("./images/logos/atlanta_hawks.png"), 
                      width = unit(size, "points"))
  celtics <- rasterGrob(readPNG("./images/logos/boston_celtics.png"),
                        width = unit(size, "points"))
  pelicans <- rasterGrob(readPNG("./images/logos/new_orleans_pelicans.png"),
                         width = unit(size, "points"))
  bulls <- rasterGrob(readPNG("./images/logos/chicago_bulls.png"), 
                      width = unit(size, "points"))
  cavaliers <- rasterGrob(readPNG("./images/logos/cleveland_cavaliers.png"),
                          width = unit(size, "points"))
  mavericks <- rasterGrob(readPNG("./images/logos/dallas_mavericks.png"),
                          width = unit(size, "points"))
  nuggets <- rasterGrob(readPNG("./images/logos/denver_nuggets.png"),
                        width = unit(size, "points"))
  pistons <- rasterGrob(readPNG("./images/logos/detroit_pistons.png"),
                        width = unit(size, "points"))
  warriors <- rasterGrob(readPNG("./images/logos/golden_state_warriors.png"),
                         width = unit(size, "points"))
  rockets <- rasterGrob(readPNG("./images/logos/houston_rockets.png"),
                        width = unit(size, "points"))
  pacers <- rasterGrob(readPNG("./images/logos/indiana_pacers.png"),
                       width = unit(size, "points"))
  clippers <- rasterGrob(readPNG("./images/logos/los_angeles_clippers.png"),
                         width = unit(size, "points"))
  lakers <- rasterGrob(readPNG("./images/logos/los_angeles_lakers.png"),
                       width = unit(size, "points"))
  heat <- rasterGrob(readPNG("./images/logos/miami_heat.png"),
                     width = unit(size, "points"))
  bucks <- rasterGrob(readPNG("./images/logos/milwaukee_bucks.png"),
                      width = unit(size, "points"))
  timberwolves <- rasterGrob(readPNG("./images/logos/minnesota_timberwolves.png"),
                             width = unit(size, "points"))
  nets <- rasterGrob(readPNG("./images/logos/brooklyn_nets.png"),
                     width = unit(size, "points"))
  knicks <- rasterGrob(readPNG("./images/logos/new_york_knicks.png"),
                       width = unit(size, "points"))
  magic <- rasterGrob(readPNG("./images/logos/orlando_magic.png"),
                      width = unit(size, "points"))
  sixers <- rasterGrob(readPNG("./images/logos/philadelphia_76ers.png"),
                       width = unit(size, "points"))
  suns <- rasterGrob(readPNG("./images/logos/phoenix_suns.png"),
                     width = unit(size, "points"))
  trailblazers <- rasterGrob(readPNG("./images/logos/portland_trailblazers.png"),
                             width = unit(size, "points"))
  kings <- rasterGrob(readPNG("./images/logos/sacramento_kings.png"),
                      width = unit(size, "points"))
  spurs <- rasterGrob(readPNG("./images/logos/san_antonio_spurs.png"),
                      width = unit(size, "points"))
  thunder <- rasterGrob(readPNG("./images/logos/okc_thunder.png"),
                        width = unit(size, "points"))
  jazz <- rasterGrob(readPNG("./images/logos/utah_jazz.png"),
                     width = unit(size, "points"))
  wizards <- rasterGrob(readPNG("./images/logos/washington_wizards.png"),
                        width = unit(size, "points"))
  raptors <- rasterGrob(readPNG("./images/logos/toronto_raptors.png"),
                        width = unit(size, "points"))
  grizzlies <- rasterGrob(readPNG("./images/logos/memphis_grizzlies.png"),
                          width = unit(size, "points"))
  hornets <- rasterGrob(readPNG("./images/logos/charlotte_hornets.png"),
                        width = unit(size, "points"))
  list(hawks = hawks, celtics = celtics, pelicans = pelicans, 
       bulls = bulls, cavaliers = cavaliers, mavericks = mavericks, 
       nuggets = nuggets, pistons = pistons, warriors = warriors,
       rockets = rockets, pacers = pacers, clippers = clippers, 
       lakers = lakers, heat = heat, bucks = bucks, 
       timberwolves = timberwolves, nets = nets, knicks = knicks,
       magic = magic, sixers = sixers, suns = suns,
       trailblazers = trailblazers, kings = kings, spurs = spurs,
       thunder = thunder, jazz = jazz, wizards = wizards, 
       raptors = raptors, grizzlies = grizzlies, hornets = hornets)
}

calc_tpm_for_group <- function(cluster) {
  alias <- str_extract(cluster, "[a-zA-Z]+")
  season <- str_extract(cluster, "[0-9]+")
  seasons <- c("1314", "1415", "1516")
  for (i in seasons) {
    if (i == "1314") {
      ids <- all_teams$global_id[all_teams$team_alias %in% alias[season %in% i]]
      all_plays <- offplays_1314[as.character(ids)]
      plays <- do.call("rbind", all_plays)
    } else if (i == "1415") {
      ids <- all_teams$global_id[all_teams$team_alias %in% alias[season %in% i]]
      all_plays <- offplays_1415[as.character(ids)]
      plays <- rbind(plays, do.call("rbind", all_plays))
    } else {
      ids <- all_teams$global_id[all_teams$team_alias %in% alias[season %in% i]]
      all_plays <- offplays_1516[as.character(ids)]
      plays <- rbind(plays, do.call("rbind", all_plays))
    }
  }
  tpm <- calc_transmat(plays, T, T)
  tcm <- calc_transmat(plays, F, T)
  list(tpm = tpm, tcm = tcm)
}

# Matrix distance functions ===================================================
KL_dist <- function(P, Q) {
  P <- P + 1e-6
  Q <- Q + 1e-6
  sum(P * log(P / Q)) + sum(Q * log(Q / P))
}

abs_dist <- function(P, Q) {
  D <- P - Q
  sum(abs(D))
}

frob_dist <- function(P, Q) {
  D <- P - Q
  sqrt(sum(D^2))
}

abs_col_dist <- function(P, Q) {
  D <- P - Q
  if (class(D) != "matrix") {
    D <- t(as.matrix(D))
  }
  max(colSums(abs(D)))
}

abs_row_dist <- function(P, Q) {
  D <- P - Q
  if (class(D) != "matrix") {
    D <- t(as.matrix(D))
  }
  max(rowSums(abs(D)))
}

Lpq_norm_dist <- function(P, Q, p, q) {
  D <- P - Q
  if (class(D) != "matrix") {
    D <- t(as.matrix(D))
  }
  sum(colSums(abs(D)^p)^(q/p))^(1/q)
}

### Plotting functions
draw_court <- function() {
  require(plotrix)
  rect(0, 0, 94, 50)
  circle <- function(x, y, r, from = 0, to = 2 * pi, lines = FALSE, ...) {
    theta <- seq(from, to, length = 100)
    if (lines)
      lines(x + r * cos(theta), y + r * sin(theta), ...)
    else polygon(x + r * cos(theta), y + r * sin(theta), ...)
  }
  points(c(5.25, 94 - 5.25), c(25, 25), cex = 2)
  segments(47, 0, 47, 50)
  circle(47, 25, 8)
  circle(47, 25, 2, col = "lightgray")
  theta1 <- acos((25 - 35/12)/23.75)
  circle(5.25, 25, 23.75, -pi/2 + theta1, pi/2 - theta1, TRUE)
  circle(94 - 5.25, 25, 23.75, pi/2 + theta1, 3 * pi/2 - theta1, TRUE)
  segments(0, 35/12, 5.25 + 23.75 * sin(theta1), 35/12)
  segments(0, 50 - 35/12, 5.25 + 23.75 * sin(theta1), 50 - 35/12)
  segments(94, 35/12, 94 - 5.25 - 23.75 * sin(theta1), 35/12)
  segments(94, 50 - 35/12, 94 - 5.25 - 23.75 * sin(theta1), 50 - 35/12)
  circle(19, 25, 6, -pi/2, pi/2, TRUE)
  circle(19, 25, 6, pi/2, 3 * pi/2, TRUE, lty = 2)
  circle(94 - 19, 25, 6, pi/2, 3 * pi/2, TRUE)
  circle(94 - 19, 25, 6, -pi/2, pi/2, TRUE, lty = 2)
  circle(5.25, 25, 4, -pi/2, pi/2, TRUE)
  circle(94 - 5.25, 25, 4, pi/2, 3 * pi/2, TRUE)
  rect(0, 17, 19, 33, border = "gray")
  rect(94, 17, 94 - 19, 33, border = "gray")
}

# Function that plots the court
plot_court <- function(main) {
  plot(0,0,pch=46,xlim=c(0,94), ylim=c(0,50), main=main, xlab = '', ylab = '')
  draw_court()
}

draw_half_court <- function() {
  circle <- function(x, y, r, from = 0, to = 2 * pi, lines = FALSE, ...) {
    theta <- seq(from, to, length = 100)
    if (lines)
      lines(x + r * cos(theta), y + r * sin(theta), ...)
    else polygon(x + r * cos(theta), y + r * sin(theta), ...)
  }
  
  rect(0, 0, 47, 50)
  points(5.25, 25, cex = 2)
  circle(47, 25, 8, from = pi/2, to = 3*pi/2)
  circle(47, 25, 2, from = pi/2, to = 3*pi/2, col = "lightgray")
  theta1 <- acos((25 - 35/12)/23.75)
  circle(5.25, 25, 23.75, -pi/2 + theta1, pi/2 - theta1, TRUE)
  segments(0, 35/12, 5.25 + 23.75 * sin(theta1), 35/12)
  segments(0, 50 - 35/12, 5.25 + 23.75 * sin(theta1), 50 - 35/12)
  circle(19, 25, 6, -pi/2, pi/2, TRUE)
  circle(19, 25, 6, pi/2, 3 * pi/2, TRUE, lty = 2)
  circle(5.25, 25, 4, -pi/2, pi/2, TRUE)
  rect(0, 17, 19, 33, border = "gray")
}

plot_half_court <- function(main, axes = T) {
  plot(0,0, pch=46, xlim = c(0, 47), ylim = c(0, 50), 
       main = main, xlab = '', axes = axes,
       xaxt = 'n', yaxt = 'n', 
       ylab = '')
  draw_half_court()
}

draw_regions <- function() {
  
  hoop_x <- 5.25
  hoop_y <- 25
  
  circle <- function(x, y, r, from = 0, to = 2 * pi, lines = FALSE, ...) {
    theta <- seq(from, to, length = 100)
    if (lines)
      lines(x + r * cos(theta), y + r * sin(theta), ...)
    else polygon(x + r * cos(theta), y + r * sin(theta), ...)
  }
  
  calc_angle <- function(x, cx, r) {
    acos((x - cx)/r)
  }
  
  calc_y_angle <- function(y, cy, r) {
    asin((y - cy)/r)
  }
  
  arc_coords <- function(x, y, r, from = 0, to = 2 * pi) {
    theta <- seq(from, to, length = 100)
    list(x = x + r * cos(theta), y = y + r * sin(theta))
  }
  
  line_func <- function(slope) {
    function(x) {
      slope * (x - hoop_x) + hoop_y
    }
  }
  
  find_intersection <- function(cx, cy, r, p1, p2) {
    x1 <- p1[1] - cx
    y1 <- p1[2] - cy
    x2 <- p2[1] - cx
    y2 <- p2[2] - cy
    
    dx <- x2 - x1
    dy <- y2 - y1
    dr <- sqrt(dx^2 + dy^2)
    D <- x1 * y2 - x2 * y1
    
    sgn_dy <- if(dy != 0) dy / abs(dy) else 1
    
    x <- (D * dy + c(-1, 1) * sgn_dy * dx * sqrt(r^2 * dr^2 - D^2)) / dr^2
    y <- (-D * dx + c(-1, 1) * abs(dy) * sqrt(r^2 * dr^2 - D^2)) / dr^2
    coords <- cbind(x + cx, y + cy)
    coords[coords[, 1] > 0, ]
  }
  
  hoop <- function(...) {
    r1 <- 8
    t1 <- calc_angle(0, hoop_x, r1)
    circle(hoop_x, hoop_y, r1, t1, -t1, ...)
    arc_coords(hoop_x, hoop_y, r1, from = -t1, to = t1)
  }
  
  low_right <- function(...) {
    b <- 50 - 28.0545
    m <- 8/13.75
    p1 <- c(0, b)
    p2 <- c(47, m * 47 + b)
    r1 <- 16
    r2 <- 8
    f1 <- calc_angle(0, hoop_x, r1)
    c1 <- find_intersection(hoop_x, hoop_y, r1, p1, p2)
    t1 <- calc_angle(c1[1], hoop_x, r1)
    c2 <- find_intersection(hoop_x, hoop_y, r2, p1, p2)
    f2 <- calc_angle(c2[1], hoop_x, r2)
    t2 <- calc_angle(0, hoop_x, r2)
    arc1 <- arc_coords(hoop_x, hoop_y, r1, from = f1, to = t1)
    arc2 <- arc_coords(hoop_x, hoop_y, r2, from = f2, to = t2)
    polygon(c(arc1$x, arc2$x), c(arc1$y, arc2$y), ...)
  }
  
  low_mid <- function(...) {
    b1 <- 50 - 28.0545
    m1 <- 8/13.75
    p11 <- c(0, b1)
    p12 <- c(47, m1 * 47 + b1)
    
    b2 <- 28.0545
    m2 <- -8/13.75
    p21 <- c(0, b2)
    p22 <- c(47, m2 * 47 + b2)
    
    r1 <- 16
    r2 <- 8
    
    # Use y angle because x coordinate is the same for both intersections
    c1 <- find_intersection(hoop_x, hoop_y, r1, p11, p12)
    f1 <- calc_y_angle(c1[2], hoop_y, r1)
    c2 <- find_intersection(hoop_x, hoop_y, r1, p21, p22)
    t1 <- calc_y_angle(c2[2], hoop_y, r1)
    
    c3 <- find_intersection(hoop_x, hoop_y, r2, p21, p22)
    f2 <- calc_y_angle(c3[2], hoop_y, r2)
    c4 <- find_intersection(hoop_x, hoop_y, r2, p11, p12)
    t2 <- calc_y_angle(c4[2], hoop_y, r2)
    
    arc1 <- arc_coords(hoop_x, hoop_y, r1, from = f1, to = t1)
    arc2 <- arc_coords(hoop_x, hoop_y, r2, from = f2, to = t2)
    polygon(c(arc1$x, arc2$x), c(arc1$y, arc2$y), ...)
  }
  
  low_left <- function(...) {
    b <- 28.0545
    m <- -8/13.75
    p1 <- c(0, b)
    p2 <- c(47, m * 47 + b)
    r1 <- 16
    r2 <- 8
    
    c1 <- find_intersection(hoop_x, hoop_y, r1, p1, p2)
    f1 <- calc_y_angle(c1[2], hoop_y, r1)
    t1 <- -calc_angle(0, hoop_x, r1) # Negative to draw clockwise
    
    c2 <- find_intersection(hoop_x, hoop_y, r2, p1, p2)
    f2 <- -calc_angle(0, hoop_x, r2)
    t2 <- -calc_y_angle(c2[2], hoop_y, r2)
    
    arc1 <- arc_coords(hoop_x, hoop_y, r1, from = f1, to = t1)
    arc2 <- arc_coords(hoop_x, hoop_y, r2, from = f2, to = -t2)
    
    polygon(c(arc1$x, arc2$x), c(arc1$y, arc2$y), ...)
  }
  
  mid_far_right <- function(...) {
    m <- (19/13.75)
    fx <- line_func(m)
    p1 <- c(0, fx(0))
    p2 <- c(47, fx(47))
    
    r1 <- 23.75
    r2 <- 16
    
    # straight edge for corner 3 area
    coords1 <- list(x = c(0, 14), y = c(47, 47))
    f1 <- calc_angle(coords1$x[2], hoop_x, r1)
    c1 <- find_intersection(hoop_x, hoop_y, r1, p1, p2)
    t1 <- calc_angle(c1[1], hoop_x, r1)
    c2 <- find_intersection(hoop_x, hoop_y, r2, p1, p2)
    f2 <- calc_angle(c2[1], hoop_x, r2)
    t2 <- calc_angle(0, hoop_x, r2)
    
    arc1 <- arc_coords(hoop_x, hoop_y, r1, from = f1, to = t1)
    arc2 <- arc_coords(hoop_x, hoop_y, r2, from = f2, to = t2)
    polygon(c(coords1$x, arc1$x, arc2$x), c(coords1$y, arc1$y, arc2$y), ...)
  }
  
  mid_right <- function(...) {
    m1 <- (19/13.75)
    f1x <- line_func(m1)
    p11 <- c(0, f1x(0))
    p12 <- c(47, f1x(47))
    
    m2 <- (5.5/13.75)
    f2x <- line_func(m2)
    p21 <- c(0, f2x(0))
    p22 <- c(47, f2x(47))
    
    r1 <- 23.75
    r2 <- 16
    
    c1 <- find_intersection(hoop_x, hoop_y, r1, p11, p12)
    c2 <- find_intersection(hoop_x, hoop_y, r1, p21, p22)
    f1 <- calc_angle(c1[1], hoop_x, r1)
    t1 <- calc_angle(c2[1], hoop_x, r1)
    
    c3 <- find_intersection(hoop_x, hoop_y, r2, p21, p22)
    c4 <- find_intersection(hoop_x, hoop_y, r2, p11, p12)
    f2 <- calc_angle(c3[1], hoop_x, r2)
    t2 <- calc_angle(c4[1], hoop_x, r2)
    
    arc1 <- arc_coords(hoop_x, hoop_y, r1, from = f1, to = t1)
    arc2 <- arc_coords(hoop_x, hoop_y, r2, from = f2, to = t2)
    polygon(c(arc1$x, arc2$x), c(arc1$y, arc2$y), ...)
  }
  
  mid_center <- function(...) {
    m1 <- (5.5/13.75)
    f1x <- line_func(m1)
    p11 <- c(0, f1x(0))
    p12 <- c(47, f1x(47))
    
    m2 <- (-5.5/13.75)
    f2x <- line_func(m2)
    p21 <- c(0, f2x(0))
    p22 <- c(47, f2x(47))
    
    r1 <- 23.75
    r2 <- 16
    
    c1 <- find_intersection(hoop_x, hoop_y, r1, p11, p12)
    c2 <- find_intersection(hoop_x, hoop_y, r1, p21, p22)
    f1 <- calc_y_angle(c1[2], hoop_y, r1)
    t1 <- calc_y_angle(c2[2], hoop_y, r1)
    
    c3 <- find_intersection(hoop_x, hoop_y, r2, p21, p22)
    c4 <- find_intersection(hoop_x, hoop_y, r2, p11, p12)
    f2 <- calc_y_angle(c3[2], hoop_y, r2)
    t2 <- calc_y_angle(c4[2], hoop_y, r2)
    
    arc1 <- arc_coords(hoop_x, hoop_y, r1, from = f1, to = t1)
    arc2 <- arc_coords(hoop_x, hoop_y, r2, from = f2, to = t2)
    polygon(c(arc1$x, arc2$x), c(arc1$y, arc2$y), ...)
  }
  
  mid_left <- function(...) {
    m1 <- (-5.5/13.75)
    f1x <- line_func(m1)
    p11 <- c(0, f1x(0))
    p12 <- c(47, f1x(47))
    
    m2 <- c(-19/13.75)
    f2x <- line_func(m2)
    p21 <- c(0, f2x(0))
    p22 <- c(47, f2x(47))
    
    r1 <- 23.75
    r2 <- 16
    
    c1 <- find_intersection(hoop_x, hoop_y, r1, p11, p12)
    c2 <- find_intersection(hoop_x, hoop_y, r1, p21, p22)
    f1 <- calc_y_angle(c1[2], hoop_y, r1)
    t1 <- calc_y_angle(c2[2], hoop_y, r1) 
    
    c3 <- find_intersection(hoop_x, hoop_y, r2, p21, p22)
    c4 <- find_intersection(hoop_x, hoop_y, r2, p11, p12)
    f2 <- calc_y_angle(c3[2], hoop_y, r2)
    t2 <- calc_y_angle(c4[2], hoop_y, r2)
    
    arc1 <- arc_coords(hoop_x, hoop_y, r1, from = f1, to = t1)
    arc2 <- arc_coords(hoop_x, hoop_y, r2, from = f2, to = t2)
    
    polygon(c(arc1$x, arc2$x), c(arc1$y, arc2$y), ...)
  }
  
  mid_far_left <- function(...) {
    m <- (-19/13.75)
    fx <- line_func(m)
    p1 <- c(0, fx(0))
    p2 <- c(47, fx(47))
    
    r1 <- 23.75
    r2 <- 16
    
    # straight edge for corner 3 area
    coords1 <- list(x = c(14, 0), y = c(3, 3))
    c1 <- find_intersection(hoop_x, hoop_y, r1, p1, p2)
    f1 <- calc_y_angle(c1[2], hoop_y, r1)
    t1 <- calc_y_angle(coords1$y[2], hoop_y, r1)
    
    c2 <- find_intersection(hoop_x, hoop_y, r2, p1, p2)
    f2 <- -calc_angle(0, hoop_x, r2)
    t2 <- calc_y_angle(c2[2], hoop_y, r2)
    
    arc1 <- arc_coords(hoop_x, hoop_y, r1, from = f1, to = t1)
    arc2 <- arc_coords(hoop_x, hoop_y, r2, from = f2, to = t2)
    polygon(c(arc1$x, coords1$x, arc2$x), c(arc1$y, coords1$y, arc2$y), ...)
  }
  
  right_corner3 <- function(...) {
    rect(0, 47, 14, 50, ...)
  }
  
  right_arc3 <- function(...) {
    m <- (6/13.75)
    fx <- line_func(m)
    p11 <- c(0, 50)
    p12 <- c(47, 50)
    
    p21 <- c(0, fx(0))
    p22 <- c(47, fx(47))
    
    r1 <- 33.72
    r2 <- 23.75
    
    c1 <- find_intersection(hoop_x, hoop_y, r1, p11, p12)
    c2 <- find_intersection(hoop_x, hoop_y, r1, p21, p22)
    f1 <- calc_y_angle(50, hoop_y, r1)
    t1 <- calc_y_angle(c2[2], hoop_y, r1)
    
    c3 <- find_intersection(hoop_x, hoop_y, r2, p21, p22)
    f2 <- calc_y_angle(c3[2], hoop_y, r2)
    t2 <- calc_angle(14, hoop_x, r2)
    
    coords1 <- list(x = c(14, c1[1]), y = c(50, 50))
    arc1 <- arc_coords(hoop_x, hoop_y, r1, from = f1, to = t1)
    arc2 <- arc_coords(hoop_x, hoop_y, r2, from = f2, to = t2)
    polygon(c(coords1$x, arc1$x, arc2$x), c(coords1$y, arc1$y, arc2$y), ...)
  }
  
  mid_arc3 <- function(...) {
    m1 <- (6/13.75)
    f1x <- line_func(m1)
    p11 <- c(0, f1x(0))
    p12 <- c(47, f1x(47))
    
    m2 <- (-6/13.75)
    f2x <- line_func(m2)
    p21 <- c(0, f2x(0))
    p22 <- c(47, f2x(47))
    
    r1 <- 33.72
    r2 <- 23.75
    
    c1 <- find_intersection(hoop_x, hoop_y, r1, p11, p12)
    c2 <- find_intersection(hoop_x, hoop_y, r1, p21, p22)
    f1 <- calc_y_angle(c1[2], hoop_y, r1)
    t1 <- calc_y_angle(c2[2], hoop_y, r1)
    
    c3 <- find_intersection(hoop_x, hoop_y, r2, p21, p22)
    c4 <- find_intersection(hoop_x, hoop_y, r2, p11, p12)
    f2 <- calc_y_angle(c3[2], hoop_y, r2)
    t2 <- calc_y_angle(c4[2], hoop_y, r2)
    
    arc1 <- arc_coords(hoop_x, hoop_y, r1, from = f1, to = t1)
    arc2 <- arc_coords(hoop_x, hoop_y, r2, from = f2, to = t2)
    polygon(c(arc1$x, arc2$x), c(arc1$y, arc2$y), ...)
  }
  
  left_arc3 <- function(...) {
    m <- (-6/13.75)
    fx <- line_func(m)
    p11 <- c(0, fx(0))
    p12 <- c(47, fx(47))
    
    p21 <- c(0, 0)
    p22 <- c(47, 0)
    
    r1 <- 33.72
    r2 <- 23.75
    
    c1 <- find_intersection(hoop_x, hoop_y, r1, p11, p12)
    c2 <- find_intersection(hoop_x, hoop_y, r1, p21, p22)
    f1 <- calc_y_angle(c1[2], hoop_y, r1)
    t1 <- calc_y_angle(0, hoop_y, r1)
    
    c3 <- find_intersection(hoop_x, hoop_y, r2, p11, p12)
    f2 <- -calc_angle(14, hoop_x, r2)
    t2 <- calc_y_angle(c3[2], hoop_y, r2)
    
    coords1 <- list(x = c(c2[1], 14), y = c(0, 0))
    arc1 <- arc_coords(hoop_x, hoop_y, r1, from = f1, to = t1)
    arc2 <- arc_coords(hoop_x, hoop_y, r2, from = f2, to = t2)
    polygon(c(arc1$x, coords1$x, arc2$x), c(arc1$y, coords1$y, arc2$y), ...)
  }
  
  left_corner3 <- function(...) {
    rect(0, 0, 14, 3, ...)
  }
  
  hoop(col = rgb(177, 89, 40, 200, maxColorValue = 255))
  low_mid(col = rgb(102, 194, 165, 200, maxColorValue = 255))
  low_right(col = rgb(158, 1, 66, 200, maxColorValue = 255))
  low_left(col = rgb(94, 79, 162, 200, maxColorValue = 255))
  mid_far_right(col = rgb(166, 206, 227, 200, maxColorValue = 255))
  mid_right(col = rgb(31, 120, 180, 200,maxColorValue = 255))
  mid_center(col = rgb(255, 255, 153, 200, maxColorValue = 255))
  mid_left(col = rgb(51, 160, 44, 200, maxColorValue = 255))
  mid_far_left(col = rgb(178, 223, 138, 200, maxColorValue = 255))
  right_corner3(col = rgb(227, 26, 28, 200, maxColorValue = 255))
  right_arc3(col = rgb(251, 154, 153, 200, maxColorValue = 255))
  mid_arc3(col = rgb(106, 61, 154, 200, maxColorValue = 255))
  left_arc3(col = rgb(253, 191, 111, 200, maxColorValue = 255))
  left_corner3(col = rgb(255, 127, 0, 200, maxColorValue = 255))
}
