# devtools::install_github("bdilday/GeomMLBStadiums")

#load libraries 
library(tidyverse)
library(baseballr)
library(GeomMLBStadiums)
library(ggforce)
library(ggraph)
library(igraph)
library(stringi)
library(cowplot)
library(MASS)
library(shiny)
library(shinythemes)
library(latex2exp)
library(patchwork)
library(png)
library(Boruta)

#get data from statcast -- only pulling one week - just going to load in the actual dataset from pybaseball
# statcast_df <- statcast_search(start_date = "2023-03-01", end_date = "2023-12-01")
# 
# #load in data from pybaseball request
statcast_df <- read.csv('/Users/sam/Desktop/personal/NickWanBootcamp/data.csv')

# unique(statcast_df$pitch_type)

# colnames(statcast_df)

hits <- c('single','double','triple','home_run')

statcast_df <- statcast_df %>%
  rename(
    batterid = batter,
    pitcherid = pitcher,
    event_type = events) %>%
  mutate(
    # create binary handedness indicators
    is_lhb = ifelse(stand == "L", "1", "0"),
    is_lhp = ifelse(p_throws == "L", "1", "0"),
    # binary inning half indicator
    is_bottom = ifelse(inning_topbot == "Bot", "1", "0"),
    # add spray angle 
    spray_angle = round(atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75, 1),
    # pitch information
    is_bip = ifelse(type == "X", 1, 0),
    is_stk = ifelse(type == "S", 1, 0),
    location_x = hc_x - 125.42,
    location_y = 198.27 - hc_y,
    location_x_ft = 2.5 * (hc_x - 125.42),
    location_y_ft = 2.5 * (198.27 - hc_y),
    hit = ifelse(event_type %in% hits,1,0),
    pfx_x_inches = pfx_x *12,
    pfx_z_inches = pfx_z *12,
    #change launch angle to radial
    Xcoord = launch_speed / 120 *
             cos(launch_angle * pi / 180),
    Ycoord = launch_speed / 120 *
             sin(launch_angle * pi / 180),
    contact_type = case_when(
             launch_speed_angle == 1 ~ 'Weak Contact',
             launch_speed_angle == 2 ~ 'Topped',
             launch_speed_angle == 3 ~ 'Hit Under',
             launch_speed_angle == 4 ~ 'Flare/Burner',
             launch_speed_angle == 5 ~ 'Solid Contact',
             launch_speed_angle == 6 ~ 'Barrels'
           ),
    #get the full name of pitch
    pitch_type_name = case_when(
      pitch_type == 'CH' ~ 'Changeup',
      pitch_type == 'CU' ~ 'Curveball',
      pitch_type == 'FC' ~ 'Cutter',
      pitch_type == 'FF' ~ 'Four-Seam Fastball',
      pitch_type == 'FS' ~ 'Splitter',
      pitch_type == 'KC' ~ 'Knuckle Curve',
      pitch_type == 'SL' ~ 'Slider',
      pitch_type == 'SI' ~ 'Sinker',
      pitch_type == 'ST' ~ 'Sweeper',
      TRUE ~ 'Other'
    ),
    #get the zone locations
    rel_plate_x = plate_x / ((17/2 + 1.456) / 12),
    rel_plate_z = (plate_z - (sz_bot - 1.456/12)) /
      (sz_top - sz_bot + 1.456*2/12) * 2 - 1,
    attack_region = case_when(
      abs(rel_plate_x) < 0.67 & abs(rel_plate_z) < 0.67 ~ "Heart",
      abs(rel_plate_x) < 1.33 & abs(rel_plate_z) < 1.33 ~ "Shadow",
      abs(rel_plate_x) < 2.00 & abs(rel_plate_z) < 2.00 ~ "Chase",
      TRUE ~ "Waste"),
    attack_region = ifelse(is.na(plate_x) | is.na(plate_z), 'No Region Listed', attack_region),
    zone = ifelse(is.na(zone),'No Zone Listed',zone),
    #swings and misses
    )

  
#perform some statcast data formatting using https://jacobrichey.github.io/2020-06-06-Build-a-Statcast-Database-in-R/
#creating radial plots using https://baseballwithr.wordpress.com/2021/04/12/constructing-a-radial-chart-using-ggplot2/comment-page-1/

#get all players info to join to the player names to statcast data
# mlb_players <- baseballr::chadwick_player_lu()

# write.csv(mlb_players,'/Users/sam/Desktop/personal/NickWanBootcamp/player_ids.csv', row.names = FALSE)

mlb_players <- read.csv('/Users/sam/Desktop/personal/NickWanBootcamp/player_ids.csv')

#create a full name column
mlb_players <- mlb_players %>%
  mutate(full_name = paste0(name_last,', ',name_first)) %>%
  dplyr::select(full_name, name_last, name_first, key_mlbam)

statcast_df <- statcast_df %>%
  mutate(batter_name = mlb_players$full_name[match(batterid, mlb_players$key_mlbam)],
         pitcher_name = mlb_players$full_name[match(pitcherid, mlb_players$key_mlbam)])

#change statcast names to english characters
statcast_df$pitcher_name <- stri_trans_general(statcast_df$pitcher_name, "Latin-ASCII")
statcast_df$batter_name <- stri_trans_general(statcast_df$batter_name, "Latin-ASCII")

#get a list of all mlb players by getting unique batters and pitchers by id
unique_ids <- unique(c(statcast_df$batterid, statcast_df$pitcherid))
unique_ids_df <- data.frame(ids = unique_ids)
unique_ids_df <- unique_ids_df %>%
  mutate(full_name = mlb_players$full_name[match(ids, mlb_players$key_mlbam)])
unique_ids_df$full_name <- stri_trans_general(unique_ids_df$full_name, "Latin-ASCII")

# #create a column with pitcher name and handedness to allow for filtering in pitch arsenal
unique_pitchers <- statcast_df %>%
  distinct(pitcher_name, p_throws) %>%
  mutate(p_throws = case_when(
    p_throws == 'L' ~ 'LHP',
    p_throws == 'R' ~ 'RHP',
    TRUE ~ 'No Hands'
  ))

frequent_pitchers <- statcast_df %>%
  group_by(pitcherid) %>%
  mutate(pitches_thrown_cnt = n()) %>%
  filter(pitches_thrown_cnt >= 100) %>%
  ungroup()

league_avg_by_handedness <- frequent_pitchers %>%
  group_by(p_throws,pitch_name) %>%
  summarize(
    la_horz_break = mean(pfx_x_inches[!is.na(pfx_x_inches)]),
    la_ivb = mean(pfx_z_inches[!is.na(pfx_z_inches)]),
    league_horz_break_sd = sd(pfx_x_inches[!is.na(pfx_x_inches)]),
    league_ivb_sd = sd(pfx_z_inches[!is.na(pfx_z_inches)]),
  ) %>%
  ungroup() %>%
  na.omit() 

# #some players have the same full name
# length(unique_ids_df$full_name)
# length(unique(unique_ids_df$full_name))

#set color scheme
pitch_palette <- rep(c("Four-Seam Fastball" = "goldenrod2", "Sinker" = "deepskyblue4", "Slider" = "red2", "Changeup" = "forestgreen",
                 "Curveball" = "violetred3", "Cutter" = "purple3", "Knuckle Curve" = "wheat3","Splitter" = "darkgreen","Sweeper" = "lightblue1",
                 "Other" = "grey30"))

result_palette <- rep(c("field_out" = "goldenrod2", "single" = "deepskyblue4", "home_run" = "red2", "force_out" = "forestgreen",
                 "double" = "violetred3", "triple" = "purple3", "grounded_into_double_play" = "wheat3",
                 "sac_fly" = "grey30", 'field_error' = 'darkgreen', 'sac_bunt' = 'lightblue1', 'fielders_choice'= 'lightyellow',
                 'fielders_choice_out' = 'darkorange1', 'double_play' = 'brown','sac_fly_double_play' = 'magenta'))

contact_palette <- rep(c("Weak Contact" = "goldenrod2", "Topped" = "deepskyblue4", "Hit Under" = "red2", "Flare/Burner" = "forestgreen",
                        "Solid Contact" = "violetred3", "Barrels" = "purple3"))

pitch_arsenal_palette <- rep(c("4-Seam Fastball" = "goldenrod2", "Sinker" = "deepskyblue4", "Slider" = "red2", "Changeup" = "forestgreen",
                               "Curveball" = "violetred3", "Cutter" = "purple3", "Knuckle Curve" = "wheat3",
                               "Other" = "grey30", 'Splitter' = 'darkgreen', 'Eephus' = 'lightblue1', 'Forkball'= 'lightyellow',
                               'Sweeper' = 'darkorange1', 'Screwball' = 'brown','Slurve' = 'magenta', 'Slow Curve' = 'navyblue', 'Knuckleball' = 'orangered' ))

swings <- c('hit_into_play','swinging_strike_blocked','foul','swinging_strike','foul_tip')
misses <- c('swinging_strike','swinging_strike_blocked','foul_tip')

#create whiff column by taking the swings and misses 
statcast_df_ <- statcast_df %>%
  mutate(swing = ifelse(description %in% swings,1,0),
         miss = ifelse(description %in% misses,1,0),
         whiff = case_when(
           swing == 1 & miss == 1 ~ 1,
           swing == 1 & miss == 0 ~ 0,
           TRUE ~ 2
         )
  ) %>%
  filter(whiff != 2)

#filter to just features and target data
statcast_stuff_df <- statcast_df_ %>%
  mutate(release_pos_x_adj = ifelse(p_throws == "R", release_pos_x, -release_pos_x),
         pfx_x_adj = ifelse(p_throws == "R", pfx_x, -pfx_x)) %>%
  dplyr::select(pfx_x_adj, pfx_z, plate_x, plate_z, release_speed, pitch_type_name,
                release_spin_rate, release_pos_x_adj, release_pos_z,
                release_extension, release_pos_y, spin_axis, whiff,
                pitcher_name, pitcherid) %>%
  na.omit()

statcast_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name != 'Other')

stuff_predictions <- function(df, pitch_type) {
  
  rf_model <-readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/',paste0('fit_rf_',pitch_type,'.rds')))
  
  prediction_probs <- predict(rf_model, type = "prob")
  
  # Extract the probability of the positive class (class 1)
  positive_class_prob <- prediction_probs[, "1"]
  
  league_avg_pitch_types <- mean(positive_class_prob)
  
  stuff_df <- df %>%
    filter(pitch_type_name == pitch_type) %>%
    mutate(preds = positive_class_prob,
           stuff_plus = (preds/league_avg_pitch_types)*100)
  
  return(stuff_df)
  
}

# Create an empty list to store each df
stuff_dfs <- list()

# Loop through each pitch type
for (i in unique(statcast_stuff_df$pitch_type_name)){
  stuff_df <- stuff_predictions(statcast_stuff_df, i)
  stuff_dfs <- append(stuff_dfs, list(stuff_df))
}

# Bind the modified dataframes together
statcast_stuff_df_complete <- do.call(rbind, stuff_dfs)




















#### creating the first tab panel pitch visualizations ####
# davis <- statcast_df %>%
#   filter(batter_name == 'Wisdom, Patrick', !is.na(release_pos_x),!is.na(release_pos_y))
# 
# davis_ab <- davis %>%
#   summarize(total_pitches = nrow(davis),
#     hits = sum(hit),
#             single = sum(event_type == 'single'),
#             double = sum(event_type == 'double'),
#             triple = sum(event_type == 'triple'),
#             hr = sum(event_type == 'home_run'),
#             pa = hits + sum(event_type == 'walk' | event_type == 'strikeout' | event_type == 'hit_by_pitch' |
#                               event_type == 'sac_fly' | event_type == 'sac_fly_double_play' | event_type == 'sac_bunt' |
#                               event_type == 'grounded_into_double_play'| event_type == 'field_out'|
#                               event_type == 'field_error'| event_type == 'fielders_choice_out'|
#                               event_type == 'fielders_choice'| event_type == 'strikeout_double_play'| event_type == 'catcher_interf' |
#                               event_type == 'force_out' |event_type == 'other_out' | event_type == 'double_play'),
#             ab = hits + sum(event_type == 'fielders_choice' | event_type == 'field_error' | event_type == 'fielders_choice_out' |
#                               event_type == 'grounded_into_double_play' | event_type == 'force_out' | event_type == 'field_out' |
#                               event_type == 'strikeout' | event_type == 'strikeout_double_play' | event_type == 'double_play'),
#             # ba = hits/at_bats,
#             so = sum(event_type == 'strikeout'),
#             ev = mean(launch_speed[is_bip==1 & !is.na(launch_speed)]),
#             spin_rate = mean(release_spin_rate[!is.na(release_spin_rate)]),
#     swing = sum(description == 'hit_into_play'|description == 'swinging_strike_blocked'|description == 'foul'|
#                   description == 'swinging_strike'|description == 'foul_tip'),
#     misses = sum(description == 'swinging_strike' | description == 'swinging_strike_blocked' | description == 'foul_tip'),
#     sw_pct = swing/total_pitches, #switch to total filtered pitches in app
#     whiff_pct = misses/swing,
#     k_pct = so/pa,
#     woba = mean(woba_value[!is.na(woba_value)]))

# #first visualization will be for pitch highlighter
# ggplot(data = samp_statcast_df)+
#        # filter(player_name=='Verlander, Justin')) +
#   geom_point(aes(x = plate_x, y = plate_z, color = as.factor(pitch_type_name))) +
#   geom_rect(xmin = -0.83,
#             xmax = 0.83,
#             ymin = 1.5,
#             ymax = 3.5, color = "black", fill = "transparent",size=1.1) +
#   geom_segment(aes(x = 0, y = -.1, xend = -.9, yend = .1), size = 0.5, color = "black") +
#   geom_segment(aes(x = -.9, y = .1, xend = -.7, yend = .4), size = 0.5, color = "black") +
#   geom_segment(aes(x =  -.7, y = .4, xend = .7, yend = 0.4), size = 0.5, color = "black") +
#   geom_segment(aes(x = .7, y = 0.4, xend = .9, yend = .1), size = 0.5, color = "black") +
#   geom_segment(aes(x = .9, y = .1, xend = 0, yend = -.1), size = 0.5, color = "black") +
#   scale_color_manual(values = palette, name = 'Pitch Type') +
#   labs(title="Pitch Location",
#        x ="Plate Side", y = "Plate Height",
#        fill = 'Pitch Type Breakdown') +
#   coord_equal() +
#   theme_classic()
# 
# #results of balls put into play
# ggplot(data = statcast_df %>%
#          filter(is_bip == 1 & player_name=='Verlander, Justin')) +
#   geom_point(aes(x = location_x_ft, y = location_y_ft, color = as.factor(event_type))) +
#   coord_equal() +
#   geom_segment(aes(x = 0, y = 0, xend = 63, yend = 63), size = 0.5, color = "black") +
#   geom_segment(aes(x = 0, y = 0, xend = -63, yend = 63), size = 0.5, color = "black") +
#   geom_segment(aes(x = 63, y = 63, xend = 0, yend = 126), size = 0.5, color = "black") +
#   geom_segment(aes(x = -63, y = 63, xend = 0, yend = 126), size = 0.5, color = "black") +
#   geom_segment(aes(x = 0, y = 0, xend = -220.5, yend = 220.5), size = 0.5, color = "black") + #change the foul lines to fit all balls in play
#   geom_segment(aes(x = 0, y = 0, xend = 220.5, yend = 220.5), size = 0.5, color = "black")
# 
# ggplot() +
#   coord_equal() +
#   geom_polygon(data = df_new2, aes(x, y),
#                fill = "lightblue") +
#   geom_polygon(data = df_new2, aes(x / 2, y / 2),
#                fill = "lightyellow") +
#   geom_segment(aes(x = 0, y = 0, 
#                    xend = cos(pi / 4), 
#                    yend = sin(pi / 4)),
#                color = "grey") +
#   geom_segment(aes(x = 0, y = 0, 
#                    xend = 1, 
#                    yend = 0),
#                color = "grey") +
#   geom_point(data = statcast_df %>%
#                filter(player_name=='Diaz, Yandy' & !is.na(launch_speed_angle) & is_bip == 1), 
#              aes(Xcoord, Ycoord,
#                  color = contact_type),
#              size = 1) +
#   annotate(geom = "text", x = 0.75, y = 0.75,
#            label = TeX("45$^o"), color = "black") +
#   annotate(geom = "text", x = 1.05, y = 0,
#            label = TeX("0$^o"), color = "black") +
#   annotate(geom = "text", x = 0.05, y = 1.05,
#            label = TeX("90$^o"), color = "black") +
#   annotate(geom = "text", x = 0.05, y = -1.05,
#            label = TeX("-90$^o"), color = "black") +
#   annotate(geom = "text", x = 0.57, y = 0.91,
#            label = "120mph", color = "black") +
#   annotate(geom = "text", x = 0.1, y = 0.45,
#            label = "60mph", color = "black") +
#   theme(panel.grid = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.background = element_blank())

#add png of hitter on radial plot
#add interactivity on plots

#### creating tab panel 2 ####

#bar plot, scatter plot for mph and scatterplot for IVB and Horz break

# justin <- frequent_pitchers %>%
#   filter(pitcher_name == 'Verlander, Justin', 
#          !is.na(release_pos_x), !is.na(release_pos_y), !is.na(pitch_type),
#          pitch_type != 'Pitch Out',pitch_type != '')
# 
# justin_freq_dist <- justin %>%
#   group_by(pitch_name) %>%
#   summarize(
#     pitch_type_cnt = n(),
#     total_pitches = nrow(justin),
#     pitch_type_freq = round((pitch_type_cnt / total_pitches) *100,2),
#     pitch_velo = mean(release_speed[!is.na(release_speed)])
#   ) %>%
#   ungroup()
# 
# # Create the bar plot
# bar_plot <- ggplot(justin_freq_dist, aes(x = pitch_type_freq, y = reorder(pitch_name,pitch_type_freq))) +
#   geom_bar(stat = "identity",aes( fill = pitch_name),width = 0.5) +
#   geom_text(aes(label=pitch_name, x = 8), position = position_nudge(y = 0.33), size = 4.5) +
#   geom_text(aes(label = paste0(pitch_type_freq,'%')), hjust = 1.1) +
#   scale_fill_manual(values = pitch_arsenal_palette, name = 'Pitch Type') +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.ticks.x=element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major.x = element_line(color = "gray85",linetype = "dashed"),
#         panel.grid.major.y = element_blank(), 
#         panel.grid.minor = element_blank()) +
#   xlab("Pitch Type Frequency") +
#   theme(legend.position = "none") +
#   scale_x_continuous(expand = c(0, 0), limits = c(0,max(justin_freq_dist$pitch_type_freq) + 10),position = "top") +
#   coord_trans(x = "reverse")
# 
# # Create the scatter plot
# scatter_plot <- ggplot(justin_freq_dist, aes(x = pitch_velo, y = reorder(pitch_name,pitch_type_freq))) +
#   geom_point(aes(color = pitch_name, size = 10)) +
#   geom_text(aes(label = paste(round(pitch_velo,1),'mph')), vjust = -1) +
#   scale_color_manual(values = pitch_arsenal_palette, name = 'Pitch Type') +
#   xlab("Velocity (mph)") +
#   ylab(NULL) +
#   theme(legend.position = "none",
#         axis.ticks.x=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.text.y=element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major.x = element_line(color = "gray92",linetype = "dashed"),
#         panel.grid.major.y = element_line(color = "gray85"), 
#         panel.grid.minor = element_blank()) +
#   scale_x_continuous(limits = c(65, 105)) 
# 
# # Arrange the two plots together
# plot_grid(bar_plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
#                            axis.ticks.y = element_blank(),plot.margin = margin(0, 0, 0, 0)),
#           scatter_plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
#                                axis.line.y = element_line(color = "black", size = 1),
#                                axis.ticks.y = element_blank(),plot.margin = margin(0, 0, 0, 0)),
#           align = 'h')

# #league average plot
# ggplot(league_avg_by_handedness %>%
#          filter(p_throws == 'L'), aes(la_horz_break, la_ivb)) +
#   coord_equal() +
#   geom_point(aes(color = pitch_name),size = 1) +
#   geom_ellipse(aes(x0 = la_horz_break, y0 = la_ivb, a = league_horz_break_sd, b = league_ivb_sd, angle = 0, fill = pitch_name), alpha = 0.2, linetype = 'dashed') +
#   theme_minimal() +
#   labs(x = "Mean Horizontal Break", y = "Mean Vertical Break") +
#   ggtitle("League Averages by Pitch Type") +
#   theme(axis.title.y = element_blank()) +
#   xlim(-30,30) +
#   ylim(-30,30)


# #pitcher swing% over plate break plate into 72 squares based on location 
# 
# justin_heatmap <- statcast_df %>%
#   filter(pitcher_name == 'Verlander, Justin', !is.na(pfx_x) , !is.na(pfx_z))
# 
# justin_density <- kde2d(justin_heatmap$pfx_x, justin_heatmap$pfx_z)
# 
# justin_density_matrix_df <- as.data.frame(justin_density$z)
# 
# 
# ggplot() +
#   geom_point(data = statcast_df %>%
#                filter(pitcher_name == 'Verlander, Justin'), aes(x = pfx_x, y = pfx_z, color = delta_run_exp))



#### affinity plots ####

#hitter by hitting profile, pitcher by hitting profile and pitcher by movement and speed

unique(statcast_df$event_type)

batter_outcomes <- statcast_df %>%
  dplyr::select(batterid, batter_name, launch_speed_angle, contact_type, event_type) %>%
  mutate(at_bat_outcome = case_when(
    contact_type == 'Weak Contact' ~ 'Weak Contact',
    contact_type == 'Topped' ~ 'Topped',
    contact_type == 'Hit Under' ~ 'Hit Under',
    contact_type == 'Flare/Burner' ~ 'Flare/Burner',
    contact_type == 'Solid Contact' ~ 'Solid Contact',
    contact_type == 'Barrels' ~ 'Barrels',
    event_type == 'strikeout' ~ 'Strikeout',
    event_type == 'walk' ~ 'Walk',
    event_type == 'hit_by_pitch' ~ 'Walk',
    TRUE ~ 'No Outcome'
  ))

#only batters with at least 100 total outcomes at bat 
batter_outcomes_summary_table <- batter_outcomes %>%
  filter(at_bat_outcome != 'No Outcome') %>%
  group_by(batterid,at_bat_outcome) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(batterid) %>%
  mutate(total_batter_outcomes = sum(count),
         percentage_each_outcome = count / total_batter_outcomes) %>%
  ungroup() %>%
  filter(total_batter_outcomes >= 100)

#widen this dataframe to have 1 row for each player and a column that describes the outcome percentage
batter_outcomes_by_pct <- batter_outcomes_summary_table %>%
  dplyr::select(-c(count,total_batter_outcomes)) %>%
  group_by(batterid) %>%
  pivot_wider(names_from = at_bat_outcome, values_from = percentage_each_outcome, values_fill = 0) %>%
  ungroup()

#get all the batters names and id for selective filtering in shiny
batter_names <- batter_outcomes_by_pct %>%
  left_join(batter_outcomes %>%
              dplyr::select(batterid,batter_name), by= c('batterid' = 'batterid')) %>%
  distinct() %>%
  dplyr::select(batter_name, batterid)

#check to make sure all outcomes together adds to 1 for each player
# outcomes_sum <- rowSums(batter_outcomes_by_pct[, -1])

#can also view the distribution of each outcome to see what is common
# ggplot() +
#   geom_col(data = batter_outcomes_summary_table, aes(x = count, y = at_bat_outcome))

# #this is what i will use to cluster the batters
action_data <- batter_outcomes_by_pct[, -1]

# # Calculate similarity matrix
similarity_dist <- dist(action_data, method = 'euclidean')

similarity_matrix <- as.matrix(similarity_dist)

similarity_df <- as.data.frame(similarity_matrix)

# test_bat_score <- similarity_df %>%
#   mutate(batterid = batter_outcomes_by_pct$batterid) %>%
#   inner_join(batter_outcomes %>%
#           select(batterid,batter_name), by= c('batterid' = 'batterid')) %>%
#   distinct()
# 
# colnames(test_bat_score) <- c(batter_names$batter_name,'batterid','batter_name')
# 
# #function that takes the user input batter, the dataframe of batters and the distance to filter similarity
# bat_similarity_func <- function(selected_batter, df, selected_distance = 0.8) {
#   
#  selected_hitter <-  df %>%
#    select(batter_similarity_dist = selected_batter, batter_name) %>%
#    mutate(batter_similarity = 1 - batter_similarity_dist) %>%
#    select(batter_name, batter_similarity) %>%
#    arrange(desc(batter_similarity)) %>%
#    filter(batter_similarity >= selected_distance)
#   
#  return(selected_hitter)
#  
# }
# 
# lindor <- test_bat_score %>%
#   select('Thompson, Trayce', batter_name) %>%
#   mutate(batter_similarity = 1 - `Thompson, Trayce`) %>%
#   select(batter_name, batter_similarity) %>%
#   arrange(desc(batter_similarity)) %>%
#   head(5)

#name the matrix row and column names
rownames(similarity_matrix) <- batter_names$batter_name
colnames(similarity_matrix) <- batter_names$batter_name

#ego plot with center being selected batter

#matrix of all batters by all batters with their distance from each other
target_row_name <- "Lindor, Francisco"

# Find the index of the target row name in the row names
target_row_index <- which(row.names(similarity_matrix) == target_row_name)

selected_row <- as.data.frame(similarity_matrix[target_row_index, ])

distance_matrix_df_ <- selected_row %>%
  rownames_to_column(var = "batters") %>%
  rename(distance = `similarity_matrix[target_row_index, ]`) %>%
  dplyr::select(batters, distance) %>%
  arrange(distance)

distance_matrix_df_ %>%
  rowwise() %>%
  mutate(CountLessThanMax = sum(distance < distance, na.rm = TRUE),
         TotalRows = n())



#### pitch clustering and stuff modeling using linear weights ####

statcast_df_re24 <- baseballr::run_expectancy_code(statcast_df, level = 'pitch')

statcast_re24_table <- linear_weights_savant(statcast_df_re24, level = 'pitch') 

#input the linear weights back into the statcast df as our target variable

#define non outs 
non_outs <- c("home_run", "triple", "double", "single",
              "walk", "hit_by_pitch", "pickoff_2B", "caught_stealing_2b",
              "caught_stealing_3b", "caught_stealing_home", "pickoff_1b",
              "pickoff_3b", "pickoff_caught_stealing_2b",
              "pickoff_caught_stealing_home", "catcher_interf",
              "batter_interference")

outs <- c(unique(statcast_df_re24 %>% dplyr::filter(!events %in% non_outs) %>%
                 dplyr::select(events) %>%
                 filter(events != '')))

outs <- c(outs$events)

re24_events <- c("home_run", "triple", "double", "single", "walk", "hit_by_pitch")

unique(statcast_df_re24 %>% dplyr::filter(!events %in% non_outs) %>%
         dplyr::select(events))

statcast_df_re24 <- statcast_df_re24 %>%
  dplyr::mutate(events = ifelse(is.na(events) | events == "", type, events)) %>%
  dplyr::mutate(events = ifelse(events == "B", "ball",
                                ifelse(events == "S", "strikes", events)))


statcast_df_re24_ <- statcast_df_re24 %>%
  mutate(pitcher_events = case_when(
    events %in% outs ~ 'outs',
    events == 'ball' ~ 'ball',
    events == 'strikes' ~ 'strikes',
    events == 'home_run' ~ 'home_run',
    events == 'triple' ~ 'triple',
    events == 'double' ~ 'double',
    events == 'single' ~ 'single',
    events == 'walk' ~ 'walk',
    events == 'hit_by_pitch' ~ 'hit_by_pitch',
    TRUE ~ 'non pitcher event'
  ),
  pitcher_events = ifelse(events == 'strikeout','strikes', pitcher_events), #if the out event is a strikeout change event to strikes for the pitcher
  pitcher_events = ifelse(events == 'walk','ball', pitcher_events)) #if the out event is a walk change event to ball for the pitcher
  

#make sure that all pitcher events are listed
non_pitcher_events <- statcast_df_re24_ %>%
  filter(pitcher_events == 'non pitcher event')

#add the values from the linear weights to the df
statcast_df_re24__ <- statcast_df_re24_ %>%
  left_join(statcast_re24_table, by = c('pitcher_events' = 'events')) %>%
  filter(pitcher_events != 'non pitcher event')

hits <- c('single','double','triple','home_run')

statcast_df_re24__ <- statcast_df_re24__ %>%
  rename(
    batterid = batter,
    pitcherid = pitcher,
    event_type = events) %>%
  mutate(
    # create binary handedness indicators
    is_lhb = ifelse(stand == "L", "1", "0"),
    is_lhp = ifelse(p_throws == "L", "1", "0"),
    # binary inning half indicator
    is_bottom = ifelse(inning_topbot == "Bot", "1", "0"),
    # add spray angle 
    spray_angle = round(atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75, 1),
    # pitch information
    is_bip = ifelse(type == "X", 1, 0),
    is_stk = ifelse(type == "S", 1, 0),
    location_x = hc_x - 125.42,
    location_y = 198.27 - hc_y,
    location_x_ft = 2.5 * (hc_x - 125.42),
    location_y_ft = 2.5 * (198.27 - hc_y),
    hit = ifelse(event_type %in% hits,1,0),
    pfx_x_inches = pfx_x *12,
    pfx_z_inches = pfx_z *12)

statcast_df_re24__ <- statcast_df_re24__ %>%
  mutate(batter_name = mlb_players$full_name[match(batterid, mlb_players$key_mlbam)],
         pitcher_name = mlb_players$full_name[match(pitcherid, mlb_players$key_mlbam)])

#change statcast names to english characters
statcast_df_re24__$pitcher_name <- stri_trans_general(statcast_df_re24__$pitcher_name, "Latin-ASCII")
statcast_df_re24__$batter_name <- stri_trans_general(statcast_df_re24__$batter_name, "Latin-ASCII")

#get a list of all mlb players by getting unique batters and pitchers by id
unique_ids <- unique(c(statcast_df_re24__$batterid, statcast_df_re24__$pitcherid))
unique_ids_df <- data.frame(ids = unique_ids)
unique_ids_df <- unique_ids_df %>%
  mutate(full_name = mlb_players$full_name[match(ids, mlb_players$key_mlbam)])
unique_ids_df$full_name <- stri_trans_general(unique_ids_df$full_name, "Latin-ASCII")

statcast_df_re24__ <- statcast_df_re24__ %>%
  mutate(pitch_type_name = case_when(
    pitch_type == 'CH' ~ 'Changeup',
    pitch_type == 'CU' ~ 'Curveball',
    pitch_type == 'FC' ~ 'Cutter',
    pitch_type == 'FF' ~ 'Four-Seam Fastball',
    pitch_type == 'FS' ~ 'Splitter',
    pitch_type == 'KC' ~ 'Knuckle Curve',
    pitch_type == 'SL' ~ 'Slider',
    pitch_type == 'SI' ~ 'Sinker',
    pitch_type == 'ST' ~ 'Sweeper',
    TRUE ~ 'Other'
  ))

statcast_stuff_df <- statcast_df_re24__ %>%
  mutate(release_pos_x_adj = ifelse(p_throws == "R", release_pos_x, -release_pos_x),
         pfx_x_adj = ifelse(p_throws == "R", pfx_x, -pfx_x)) %>%
  dplyr::select(pfx_x_adj, pfx_z, plate_x, plate_z, release_speed, pitch_type_name,
                release_spin_rate, release_pos_x_adj, release_pos_z,
                release_extension, release_pos_y, spin_axis, linear_weights_above_average,
                pitcher_name, pitcherid, pitcher_events) %>%
  na.omit()

# corrplot::corrplot(cor(statcast_stuff_df %>%
#                          filter(pitch_type_name == 'Four-Seam Fastball') %>%
#                          dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid','pitcher_events'))))
# 
# statcast_stuff_df %>%
#   filter(pitch_type_name == 'Four-Seam Fastball') %>%
#   group_by(pitcher_events) %>%
#   summarise(count = n()) %>%
#   arrange(desc(count))
# 
# statcast_stuff_df %>%
#          filter(pitch_type_name == 'Four-Seam Fastball') %>%
#          group_by(pitcher_events) %>%
#          summarise(count = n()) %>%
#   ggplot(aes(x=count, y=reorder(pitcher_events, count))) +
#   geom_col(fill='blue') +
#   geom_label(aes(label = (count)))

#no real linear relationship between pitch result and pitch metrics
# stuff_lm <- lm(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
#                  release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
#                  release_pos_y + spin_axis,
#                  data = statcast_stuff_df %>%
#                  filter(pitch_type_name == 'Four-Seam Fastball') %>%
#                  dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
#                    
# summary(stuff_lm)

#perform the next few steps for each pitch type

#split df into different pitch types for modeling
four_seam_fastball_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Four-Seam Fastball')

sample_four_seam_fastball_stuff_df <- four_seam_fastball_stuff_df %>%
  sample_frac(.1)

#sample the large df for features using boruta
boruta_fb <- Boruta(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                      release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                      release_pos_y + spin_axis,
                    data = sample_four_seam_fastball_stuff_df %>%
                      dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

#all 9 features were important for fastball
boruta_fb

#fit a random forest with the fastball data
fit_rf_fb <- randomForest::randomForest(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                                       release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                       release_pos_y + spin_axis, data = four_seam_fastball_stuff_df %>%
                                       dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
min(fit_rf_fb$mse)

#save the best model
saveRDS(fit_rf_fb, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_fb_stuff.rds'))

#load model 
fit_rf_fb <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_fb_stuff.rds'))

fit_rf_fb$predicted


plot(fit_rf_fb)
which.min(fit_rf_fb$mse)

names(fit_rf_fb)
fit_rf_fb$importance
randomForest::varImpPlot(fit_rf_fb) #release speed and vertical movement are biggest predictors

########################################### NEW PITCH TYPE

unique(statcast_stuff_df$pitch_type_name)

#split df into different pitch types for modeling
cutter_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Cutter')

sample_cutter_stuff_df <- cutter_stuff_df %>%
  sample_frac(.1)

#sample the large df for features using boruta
boruta_fc <- Boruta(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                      release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                      release_pos_y + spin_axis,
                    data = sample_cutter_stuff_df %>%
                      dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

#all 9 features were important for fastball
boruta_fc

#fir a random forest with the fastball data
fit_rf_fc <- randomForest::randomForest(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                                       release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                       release_pos_y + spin_axis, data = cutter_stuff_df %>%
                                       dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
fit_rf_fc

#save the best model
saveRDS(fit_rf_fc, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_fc_stuff.rds'))

#load model 
fit_rf_fc <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_fc_stuff.rds'))

fit_rf_fc$importance


fastball_rf

names(fit_rf_fb)
fit_rf_fb$importance
randomForest::varImpPlot(fit_rf_fb) #release speed and vertical movement are biggest predictors

########################################### NEW PITCH TYPE

unique(statcast_stuff_df$pitch_type_name)

#split df into different pitch types for modeling
slider_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Slider')

sample_slider_stuff_df <- slider_stuff_df %>%
  sample_frac(.1)

#sample the large df for features using boruta
boruta_sl <- Boruta(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                      release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                      release_pos_y + spin_axis,
                    data = sample_slider_stuff_df %>%
                      dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

#all 9 features were important for fastball
boruta_sl

#fir a random forest with the fastball data
fit_rf_sl <- randomForest::randomForest(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                                          release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                          release_pos_y + spin_axis, data = slider_stuff_df %>%
                                          dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
fit_rf_sl

#save the best model
saveRDS(fit_rf_sl, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_sl_stuff.rds'))

#load model 
fit_rf_sl <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_sl_stuff.rds'))

fit_rf_sl$importance

names(fit_rf_sl)
fit_rf_sl$importance
randomForest::varImpPlot(fit_rf_sl) #release speed and vertical movement are biggest predictors

########################################### NEW PITCH TYPE

unique(statcast_stuff_df$pitch_type_name)

#split df into different pitch types for modeling
sinker_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Sinker')

sample_sinker_stuff_df <- sinker_stuff_df %>%
  sample_frac(.1)

#sample the large df for features using boruta
boruta_si <- Boruta(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                      release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                      release_pos_y + spin_axis,
                    data = sample_sinker_stuff_df %>%
                      dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

#all 9 features were important for fastball
boruta_si

#fir a random forest with the fastball data
fit_rf_si <- randomForest::randomForest(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                                          release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                          release_pos_y + spin_axis, data = sinker_stuff_df %>%
                                          dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
fit_rf_si

#save the best model
saveRDS(fit_rf_si, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_si_stuff.rds'))

#load model 
fit_rf_si <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_si_stuff.rds'))

fit_rf_si$importance

names(fit_rf_si)
fit_rf_si$importance
randomForest::varImpPlot(fit_rf_si) #release speed and vertical movement are biggest predictors

########################################### NEW PITCH TYPE

unique(statcast_stuff_df$pitch_type_name)

#split df into different pitch types for modeling
curveball_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Curveball')

sample_curveball_stuff_df <- curveball_stuff_df %>%
  sample_frac(.1)

#sample the large df for features using boruta
boruta_cu <- Boruta(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                      release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                      release_pos_y + spin_axis,
                    data = sample_curveball_stuff_df %>%
                      dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

#all 9 features were important for fastball
boruta_cu

#fir a random forest with the fastball data
fit_rf_cu <- randomForest::randomForest(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                                          release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                          release_pos_y + spin_axis, data = curveball_stuff_df %>%
                                          dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
fit_rf_cu

#save the best model
saveRDS(fit_rf_cu, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_cu_stuff.rds'))

#load model 
fit_rf_cu <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_cu_stuff.rds'))

fit_rf_cu$importance

names(fit_rf_cu)
fit_rf_cu$importance
randomForest::varImpPlot(fit_rf_cu) #release speed and vertical movement are biggest predictors

########################################### NEW PITCH TYPE

unique(statcast_stuff_df$pitch_type_name)

#split df into different pitch types for modeling
changeup_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Changeup')

sample_changeup_stuff_df <- curveball_stuff_df %>%
  sample_frac(.1)

#sample the large df for features using boruta
boruta_ch <- Boruta(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                      release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                      release_pos_y + spin_axis,
                    data = sample_changeup_stuff_df %>%
                      dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

#all 9 features were important for fastball
boruta_ch

#fir a random forest with the fastball data
fit_rf_ch <- randomForest::randomForest(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                                          release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                          release_pos_y + spin_axis, data = changeup_stuff_df %>%
                                          dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
fit_rf_ch

#save the best model
saveRDS(fit_rf_ch, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_ch_stuff.rds'))

#load model 
fit_rf_ch <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_ch_stuff.rds'))

fit_rf_ch$importance

names(fit_rf_ch)
fit_rf_ch$importance
randomForest::varImpPlot(fit_rf_ch) #release speed and vertical movement are biggest predictors

########################################### NEW PITCH TYPE

unique(statcast_stuff_df$pitch_type_name)

#split df into different pitch types for modeling
splitter_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Splitter')

sample_splitter_stuff_df <- splitter_stuff_df %>%
  sample_frac(.1)

#sample the large df for features using boruta
boruta_fs <- Boruta(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                      release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                      release_pos_y + spin_axis,
                    data = sample_splitter_stuff_df %>%
                      dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

#release speed not important for splitter
boruta_fs

#fir a random forest with the fastball data
fit_rf_fs <- randomForest::randomForest(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                                          release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                          release_pos_y + spin_axis, data = splitter_stuff_df %>%
                                          dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
fit_rf_fs

#save the best model
saveRDS(fit_rf_fs, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_fs_stuff.rds'))

#load model 
fit_rf_fs <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_fs_stuff.rds'))

fit_rf_fs$importance

names(fit_rf_fs)
fit_rf_fs$importance
randomForest::varImpPlot(fit_rf_fs) #release speed and vertical movement are biggest predictors

########################################### NEW PITCH TYPE

unique(statcast_stuff_df$pitch_type_name)

#split df into different pitch types for modeling
sweeper_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Sweeper')

sample_sweeper_stuff_df <- sweeper_stuff_df %>%
  sample_frac(.1)

#sample the large df for features using boruta
boruta_st <- Boruta(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                      release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                      release_pos_y + spin_axis,
                    data = sample_sweeper_stuff_df %>%
                      dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

#release speed not important for splitter
boruta_st

#fir a random forest with the fastball data
fit_rf_st <- randomForest::randomForest(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                                          release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                          release_pos_y + spin_axis, data = sweeper_stuff_df %>%
                                          dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
fit_rf_st

#save the best model
saveRDS(fit_rf_st, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_st_stuff.rds'))

#load model 
fit_rf_st <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_st_stuff.rds'))

fit_rf_st$importance

names(fit_rf_st)
fit_rf_st$importance
randomForest::varImpPlot(fit_rf_st) #release speed and vertical movement are biggest predictors

########################################### NEW PITCH TYPE

unique(statcast_stuff_df$pitch_type_name)

#split df into different pitch types for modeling
kc_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Knuckle Curve')

sample_kc_stuff_df <- kc_stuff_df %>%
  sample_frac(.1)

#sample the large df for features using boruta
boruta_kc <- Boruta(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                      release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                      release_pos_y + spin_axis,
                    data = sample_kc_stuff_df %>%
                      dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

#release speed not important for kc
boruta_kc

#fir a random forest with the fastball data
fit_rf_kc <- randomForest::randomForest(linear_weights_above_average ~ pfx_x_adj + pfx_z + release_speed + 
                                          release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                          release_pos_y + spin_axis, data = kc_stuff_df %>%
                                          dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
fit_rf_kc

#save the best model
saveRDS(fit_rf_kc, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_kc_stuff.rds'))

#load model 
fit_rf_kc <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_kc_stuff.rds'))

fit_rf_kc$importance

names(fit_rf_kc)
fit_rf_kc$importance
randomForest::varImpPlot(fit_rf_kc) #release speed and vertical movement are biggest predictors

plot(fit_rf_kc)
which.min(fit_rf_kc$mse)

#test data is all games from july20-aug1st format the data and predict on the new pitches for each type

test_df <- read.csv('/Users/sam/Desktop/personal/NickWanBootcamp/test_data_july20_aug2.csv')

outs <- c(unique(test_df %>% dplyr::filter(!events %in% non_outs) %>%
                   dplyr::select(events) %>%
                   filter(events != '')))

outs <- c(outs$events)

test_df <- test_df %>%
  dplyr::mutate(events = ifelse(is.na(events) | events == "", type, events)) %>%
  dplyr::mutate(events = ifelse(events == "B", "ball",
                                ifelse(events == "S", "strikes", events)))


test_df <- test_df %>%
  mutate(pitcher_events = case_when(
    events %in% outs ~ 'outs',
    events == 'ball' ~ 'ball',
    events == 'strikes' ~ 'strikes',
    events == 'home_run' ~ 'home_run',
    events == 'triple' ~ 'triple',
    events == 'double' ~ 'double',
    events == 'single' ~ 'single',
    events == 'walk' ~ 'walk',
    events == 'hit_by_pitch' ~ 'hit_by_pitch',
    TRUE ~ 'non pitcher event'
  ),
  pitcher_events = ifelse(events == 'strikeout','strikes', pitcher_events), #if the out event is a strikeout change event to strikes for the pitcher
  pitcher_events = ifelse(events == 'walk','ball', pitcher_events)) #if the out event is a walk change event to ball for the pitcher


#make sure that all pitcher events are listed
non_pitcher_events <- test_df %>%
  filter(pitcher_events == 'non pitcher event')

#add the values from the linear weights to the df
test_df_ <- test_df %>%
  left_join(statcast_re24_table, by = c('pitcher_events' = 'events')) %>%
  filter(pitcher_events != 'non pitcher event')

hits <- c('single','double','triple','home_run')

test_df_ <- test_df_ %>%
  rename(
    batterid = batter,
    pitcherid = pitcher,
    event_type = events) %>%
  mutate(
    # create binary handedness indicators
    is_lhb = ifelse(stand == "L", "1", "0"),
    is_lhp = ifelse(p_throws == "L", "1", "0"),
    # binary inning half indicator
    is_bottom = ifelse(inning_topbot == "Bot", "1", "0"),
    # add spray angle 
    spray_angle = round(atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75, 1),
    # pitch information
    is_bip = ifelse(type == "X", 1, 0),
    is_stk = ifelse(type == "S", 1, 0),
    location_x = hc_x - 125.42,
    location_y = 198.27 - hc_y,
    location_x_ft = 2.5 * (hc_x - 125.42),
    location_y_ft = 2.5 * (198.27 - hc_y),
    hit = ifelse(event_type %in% hits,1,0),
    pfx_x_inches = pfx_x *12,
    pfx_z_inches = pfx_z *12)

test_df_ <- test_df_ %>%
  mutate(batter_name = mlb_players$full_name[match(batterid, mlb_players$key_mlbam)],
         pitcher_name = mlb_players$full_name[match(pitcherid, mlb_players$key_mlbam)])

#change statcast names to english characters
test_df_$pitcher_name <- stri_trans_general(test_df_$pitcher_name, "Latin-ASCII")
test_df_$batter_name <- stri_trans_general(test_df_$batter_name, "Latin-ASCII")

#get a list of all mlb players by getting unique batters and pitchers by id
unique_ids <- unique(c(test_df_$batterid, test_df_$pitcherid))
unique_ids_df <- data.frame(ids = unique_ids)
unique_ids_df <- unique_ids_df %>%
  mutate(full_name = mlb_players$full_name[match(ids, mlb_players$key_mlbam)])
unique_ids_df$full_name <- stri_trans_general(unique_ids_df$full_name, "Latin-ASCII")

test_df_ <- test_df_ %>%
  mutate(pitch_type_name = case_when(
    pitch_type == 'CH' ~ 'Changeup',
    pitch_type == 'CU' ~ 'Curveball',
    pitch_type == 'FC' ~ 'Cutter',
    pitch_type == 'FF' ~ 'Four-Seam Fastball',
    pitch_type == 'FS' ~ 'Splitter',
    pitch_type == 'KC' ~ 'Knuckle Curve',
    pitch_type == 'SL' ~ 'Slider',
    pitch_type == 'SI' ~ 'Sinker',
    pitch_type == 'ST' ~ 'Sweeper',
    TRUE ~ 'Other'
  ))

test_df_clean <- test_df_ %>%
  mutate(release_pos_x_adj = ifelse(p_throws == "R", release_pos_x, -release_pos_x),
         pfx_x_adj = ifelse(p_throws == "R", pfx_x, -pfx_x)) %>%
  dplyr::select(pfx_x_adj, pfx_z, plate_x, plate_z, release_speed, pitch_type_name,
                release_spin_rate, release_pos_x_adj, release_pos_z,
                release_extension, release_pos_y, spin_axis, linear_weights_above_average,
                pitcher_name, pitcherid, pitcher_events) %>%
  na.omit()


test_df_clean

#filter and test fastball model
test_fb_df <- test_df_clean %>%
  filter(pitch_type_name == 'Four-Seam Fastball')

fb_preds <- predict(fit_rf_fb, newdata = test_fb_df %>%
          dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

test_fb_df$preds <- fb_preds

compare_results_fb <- test_fb_df %>%
  dplyr::select(pitcher_name,pfx_x_adj, pfx_z, plate_x, plate_z, release_speed, pitch_type_name,
                release_spin_rate, release_pos_x_adj, release_pos_z,
                release_extension, release_pos_y, spin_axis, pitcher_events,
                linear_weights_above_average, preds)

qop_pitchers <- compare_results_fb %>%
  group_by(pitcher_name) %>%
  group_by(pitcher_name) %>%
  summarise(pitches = n(),
            xrv = 100*sum(preds)/pitches,
            rv = 100*sum(linear_weights_above_average)/pitches) %>%
  filter(pitches > 100) %>%
  arrange(desc(xrv))

#filter and test fastball model
test_ch_df <- test_df_clean %>%
  filter(pitch_type_name == 'Changeup')

ch_preds <- predict(fit_rf_ch, newdata = test_ch_df %>%
                      dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

test_ch_df$preds <- ch_preds

compare_results_ch <- test_ch_df %>%
  dplyr::select(pitcher_name,pfx_x_adj, pfx_z, plate_x, plate_z, release_speed, pitch_type_name,
                release_spin_rate, release_pos_x_adj, release_pos_z,
                release_extension, release_pos_y, spin_axis, pitcher_events,
                linear_weights_above_average, preds)

qop_pitchers_ch <- compare_results_ch %>%
  group_by(pitcher_name) %>%
  summarise(pitches = n(),
    xrv = 100*sum(preds)/pitches,
    rv = 100*sum(linear_weights_above_average)/pitches) %>%
  filter(pitches > 10) %>%
  arrange(desc(xrv))


#view results of full season data fastballs only
full_season_data_ <- four_seam_fastball_stuff_df %>%
  mutate(preds = fit_rf_fb$predicted)

full_season_data <- rbind(full_season_data_,test_fb_df)

qop_pitchers_fb <- full_season_data %>%
  mutate(league_avg = mean(preds),
         pct_dif = (preds - league_avg) / league_avg * 100) %>%
  group_by(pitcher_name) %>%
  summarise(pitches = n(),
            xrv = 100*sum(preds)/pitches,
            rv = 100*sum(linear_weights_above_average)/pitches) %>%
  ungroup() %>%
  mutate(rv_plus = (xrv / mean(xrv))*100) %>%
  filter(pitches > 300) %>%
  arrange(desc(xrv))

four_seam_fastball_stuff_df %>%
  group_by(pitcher_name) %>%
  summarise(pitches = n(),
            lin_weight = sum(linear_weights_above_average)) %>%
  filter(pitches > 300) %>%
  arrange(desc(lin_weight))



re_24_manual_table <- statcast_df_re24__ %>%
  group_by(pitcher_events) %>%
  filter(!is.na(re24)) %>%
  summarise(linear_weights_above_average_manual = round(mean(re24),2))

#remake model with my own linear weights to see if output is better

statcast_df_re24_manual <- statcast_df_re24__ %>%
  left_join(re_24_manual_table, by = c('pitcher_events')) %>%
  mutate(release_pos_x_adj = ifelse(p_throws == "R", release_pos_x, -release_pos_x),
         pfx_x_adj = ifelse(p_throws == "R", pfx_x, -pfx_x)) %>%
  dplyr::select(pfx_x_adj, pfx_z, plate_x, plate_z, release_speed, pitch_type_name,
                release_spin_rate, release_pos_x_adj, release_pos_z,
                release_extension, release_pos_y, spin_axis,linear_weights_above_average, linear_weights_above_average_manual = linear_weights_above_average_manual.x,
                pitcher_name, pitcherid, pitcher_events) %>%
  na.omit()

four_seam_fastball_stuff_df_manual <- statcast_df_re24_manual %>%
  filter(pitch_type_name == 'Four-Seam Fastball')

#fit a random forest with the fastball data
fit_rf_fb_manual <- randomForest::randomForest(linear_weights_above_average_manual ~ pfx_x_adj + pfx_z + release_speed + 
                                          release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                          release_pos_y + spin_axis, data = four_seam_fastball_stuff_df_manual %>%
                                          dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
min(fit_rf_fb_manual$mse)

#save the best model
saveRDS(fit_rf_fb_manual, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_fb_manual_stuff.rds'))

four_seam_fastball_stuff_df_manual_results <- four_seam_fastball_stuff_df_manual %>%
  mutate(preds = fit_rf_fb_manual$predicted)

qop_pitchers_fb_manual <- four_seam_fastball_stuff_df_manual_results %>%
  mutate(league_avg = mean(preds),
         pct_dif = (preds - league_avg) / league_avg * 100) %>%
  group_by(pitcher_name) %>%
  summarise(pitches = n(),
            xrv = 100*sum(preds)/pitches,
            rv = 100*sum(linear_weights_above_average)/pitches) %>%
  ungroup() %>%
  mutate(rv_plus = (xrv / mean(xrv))*100) %>%
  filter(pitches > 300) %>%
  arrange(desc(xrv))

four_seam_fastball_stuff_df_manual_results_ <- four_seam_fastball_stuff_df_manual_results %>%
  dplyr::select(pitcher_name,pitcher_events,linear_weights_above_average_manual,preds, pfx_x_adj, pfx_z,
                release_speed, release_spin_rate,release_extension,release_pos_x_adj,release_pos_z,
                  release_pos_y,spin_axis) %>%
  arrange((preds))
  

four_seam_fastball_stuff_df_manual_results_ %>%
  group_by(pitcher_events) %>%
  summarise(cnt = n())

sample_statcast_df <- sample_frac(statcast_df,0.1)

sample_statcast_df <- sample_statcast_df %>%
  arrange(desc(delta_run_exp)) %>%
  dplyr::select(pfx_x, pfx_z, plate_x, plate_z, release_speed, pitch_type_name,
                release_spin_rate, release_pos_x, release_pos_z,
                release_extension, release_pos_y, spin_axis, delta_run_exp) %>%
  na.omit()

corrplot::corrplot(cor(sample_statcast_df %>%
                         dplyr::select(-c(pitch_type_name))))


common_pitch_types <- c('Four-Seam Fastball','Cutter','Sinker','Changeup','Splitter','Curveball','Knuckle Curve','Slider','Sweeper')

statcast_stuff_df <- statcast_df %>%
  filter(pitch_type_name %in% common_pitch_types)



statcast_stuff_df <- statcast_df %>%
  mutate(stuff_model_pitch_name = case_when(
    pitch_type_name %in% fastballs ~ 'Fastballs',
    pitch_type_name %in% offspeed ~ 'Offspeed',
    pitch_type_name %in% breakingball ~ 'Breakingball',
    TRUE ~ 'No valid pitch'
  )) %>%
  filter(stuff_model_pitch_name != 'No valid pitch')

#clustering for pitch types to view how the model should be made - between fastball/offspeed models OR fastball/offspeed/breakingball

#Hierarchical clustering to explore data and make sure i have the right clusters 

#take all columns that might be useful for classifying a pitch 
statcast_stuff_df <- statcast_stuff_df %>%
  dplyr::select(pfx_x, pfx_z, plate_x, plate_z, release_speed, pitch_type_name,
                release_spin_rate, release_pos_x, release_pos_z,
                release_extension, release_pos_y, spin_axis, delta_run_exp, pitcher_name, pitcherid) %>%
  na.omit()

statcast_stuff_df %>%
  group_by(pitcherid) %>%
  summarise(run_val = sum(delta_run_exp)) %>%
  ungroup() %>%
  merge(statcast_stuff_df %>%
          dplyr::select(pitcherid,pitcher_name), by = c('pitcherid')) %>%
  distinct() %>%
  arrange((run_val))

statcast_stuff_df %>%
  group_by(pitch_type_name) %>%
  summarise(league_avg_dre = mean(delta_run_exp),
            count = n())

#check for na values across the columns
statcast_stuff_df_filtered %>%
  summarize(across(.cols = everything(),
                   .fns = ~sum(is.na(.x)))) %>%
  t()

#remove NA values and filter for just movement and spin of pitch
statcast_stuff_df_filtered <- statcast_stuff_df %>%
  dplyr::select(pitch_type_name,release_speed,release_spin_rate,pfx_x, pfx_z,spin_axis)

#take the median of each pitch type
pitch_type_summary <- statcast_stuff_df_filtered %>%
  group_by(pitch_type_name) %>%
  summarize(across(everything(),
                   .fns = mean))

#store the pitch types in a vector
pitches <- pitch_type_summary$pitch_type_name

hclustering_stuff_df <- pitch_type_summary %>%
  dplyr::select(-c(pitch_type_name)) %>%
  as.data.frame()

rownames(hclustering_stuff_df) <- pitches

#scale the data to where the mean is 0 and the sd is 1 then take the distance between each type of pitch to create a dissimilarity matrix for the hclust function to cluster 
pitch_clust <- dist(scale(hclustering_stuff_df))

#bottom up approach method of clustering
pitch_clust_final <- hclust(pitch_clust, method = 'ward.D')

#plot the clusters - it appears the clusters are best categorized with these features and they make up a division between fastball and offspeed
plot(pitch_clust_final,labels = pitches, main = 'pitch clusters')
rect.hclust(pitch_clust_final, k=3, border='red')

#i think the model will perform best with 3 splits based on these clusters

#assign the clusters back to the df by using cutree function to cut the dataset into the clusters outlined in red on the dendrogram
clust_assignment <- cutree(pitch_clust_final, k=3)

#add back the pitch type, custered dataframe and the divided cluster assignments into one combined dataframe
pitch_clust_df <- bind_cols(pitch_type = pitches,
                            hclustering_stuff_df, 
                            cluster_num = clust_assignment)

#apply this clustering to the entire df using case when since clustering takes up too much memory
clust_assignment_df <- clust_assignment %>%
  as.data.frame() %>%
  rownames_to_column("pitch_type_name") %>%
  rename("cluster_num" = ".")

statcast_stuff_df <- statcast_stuff_df %>%
  merge(clust_assignment_df, by = c('pitch_type_name'))


#now that the cluster have been created, divide the dataset into 3 different dfs each with training and testing, also use boruta for feature selection/reduction

#could just be enough data to make a model for all 7 pitch types 

statcast_stuff_df_clust_1 <- statcast_stuff_df %>%
  filter(cluster_num == 1)

statcast_stuff_df_clust_2 <- statcast_stuff_df %>%
  filter(cluster_num == 2)

statcast_stuff_df_clust_3 <- statcast_stuff_df %>%
  filter(cluster_num == 3)

#boruta feature selection 

#store pitch types in a vector to join back later 
cluster1_pitches <- statcast_stuff_df_clust_1$pitch_type_name

#remove the cluster and the pitch name from the df to keep only the target and features
statcast_stuff_df_clust_1_numeric <- statcast_stuff_df_clust_1 %>%
  dplyr::select(-c('pitch_type_name','cluster_num'))


Boruta_cluster1 <- Boruta(delta_run_exp ~ ., data = statcast_stuff_df_clust_1_numeric)

#also test boruta on individual pitches for the model using all 7 pitch types 

#plot correlation between delta run expectancy and pitch features
corrplot::corrplot(cor(statcast_stuff_df_clust_1_numeric))


stuff_lm_test <- lm(delta_run_exp ~ ., data = statcast_stuff_df_clust_3 %>%
                      dplyr::select(-c('pitch_type_name','cluster_num')))

summary(stuff_lm_test)

#### redoing stuff model using whiff to test against linear weights ####

swings <- c('hit_into_play','swinging_strike_blocked','foul','swinging_strike','foul_tip')
misses <- c('swinging_strike','swinging_strike_blocked','foul_tip')

#create whiff column by taking the swings and misses 
statcast_df_ <- statcast_df %>%
  mutate(swing = ifelse(description %in% swings,1,0),
         miss = ifelse(description %in% misses,1,0),
         whiff = case_when(
           swing == 1 & miss == 1 ~ 1,
           swing == 1 & miss == 0 ~ 0,
           TRUE ~ 2
         )
  ) %>%
  filter(whiff != 2)

# statcast_df_ %>%
#   summarise(cnt = n())

#filter to just features and target data
statcast_stuff_df <- statcast_df_ %>%
  mutate(release_pos_x_adj = ifelse(p_throws == "R", release_pos_x, -release_pos_x),
         pfx_x_adj = ifelse(p_throws == "R", pfx_x, -pfx_x)) %>%
  dplyr::select(pfx_x_adj, pfx_z, plate_x, plate_z, release_speed, pitch_type_name,
                release_spin_rate, release_pos_x_adj, release_pos_z,
                release_extension, release_pos_y, spin_axis, whiff,
                pitcher_name, pitcherid) %>%
  na.omit()

#train model on swing data for each pitch type 
four_seam_fastball_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Four-Seam Fastball')

sample_four_seam_fastball_stuff_df <- four_seam_fastball_stuff_df %>%
  sample_frac(.1)

#sample the large df for features using boruta
boruta_fb <- Boruta(whiff ~ pfx_x_adj + pfx_z + release_speed + 
                      release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                      release_pos_y + spin_axis,
                    data = sample_four_seam_fastball_stuff_df %>%
                      dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

#all 9 features were important for fastball
boruta_fb

#fit a random forest with the fastball data
fit_rf_fb <- randomForest::randomForest(as.factor(whiff) ~ pfx_x_adj + pfx_z + release_speed + 
                                          release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                          release_pos_y + spin_axis, data = four_seam_fastball_stuff_df %>%
                                          dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))

summary(fit_rf_fb)
min(fit_rf_fb$mse)

four_seam_fastball_stuff_df_  <- four_seam_fastball_stuff_df %>%
  mutate(preds = fit_rf_fb$predicted)

#get the percentile that each prediction fits compared to the rest of the predictions
four_seam_fastball_stuff_df_$ranking <- (rank(four_seam_fastball_stuff_df_$preds) - 1) / (length(four_seam_fastball_stuff_df_$preds) - 1) * 100


#get league average predicted whiff and then scale the predictions to make stuff plus
league_avg_fastball <- mean(four_seam_fastball_stuff_df_$preds)

four_seam_fastball_stuff_df_$stuff_plus_fb <- (four_seam_fastball_stuff_df_$preds / league_avg_fastball)*100

four_seam_fastball_stuff_df_$stuff_plus_fb_percentile <- (four_seam_fastball_stuff_df_$ranking / 50)*100

#group by pitcher and see average stuff
fastball_pitcher_stuff <- four_seam_fastball_stuff_df_ %>%
  group_by(pitcherid) %>%
  summarise(
    cnt = n(),
    stuff = mean(stuff_plus_fb)) %>%
  filter(cnt > 10) %>%
  arrange(desc(stuff)) %>%
  left_join(four_seam_fastball_stuff_df_ %>%
              dplyr::select(pitcherid,pitcher_name), by = c('pitcherid')) %>%
  distinct()

#save the model
saveRDS(fit_rf_fb, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/rf_fb_stuff.rds'))

#load model 
fit_rf_fb <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/first_rf_fb_stuff.rds'))

#create a function to build the other stuff models by inputting a list of pitch types and the df - filter the df for the given pitch type and build the model

stuff_model_creation <- function(df, pitch_type) {
  
  #train model on swing data for each pitch type 
  pitch_type_df <- df %>%
    filter(pitch_type_name == pitch_type)
  
  sample_pitch_type_df <- pitch_type_df %>%
    sample_frac(.1)
  
  #sample the large df for features using boruta
  boruta_pitch_type <- Boruta(whiff ~ pfx_x_adj + pfx_z + release_speed + 
                        release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                        release_pos_y + spin_axis,
                      data = sample_pitch_type_df %>%
                        dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
  
  #all 9 features were important for fastball
  boruta_pitch_type
  
  #fit a random forest with the fastball data
  fit_rf_pitch_type <- randomForest::randomForest(as.factor(whiff) ~ pfx_x_adj + pfx_z + release_speed + 
                                            release_spin_rate + release_extension + release_pos_x_adj + release_pos_z +
                                            release_pos_y + spin_axis, data = pitch_type_df %>%
                                            dplyr::select(-c('pitch_type_name','pitcher_name','pitcherid')))
  
  
  saveRDS(fit_rf_pitch_type, file = file.path('/Users/sam/Desktop/personal/NickWanBootcamp/', paste0('fit_rf_',pitch_type,'.rds')))
  
  
}

#test building the models within the function
stuff_model_creation(statcast_stuff_df, 'Slider')

#load model 
fit_rf_slider <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/fit_rf_Slider.rds'))

prediction_probs <- predict(fit_rf_slider, type = "prob")

# Extract the probability of the positive class (class 1)
positive_class_prob <- prediction_probs[, "1"]

league_avg_slider <- mean(positive_class_prob)

slider_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Slider') %>%
  mutate(preds = positive_class_prob,
         stuff_plus_sl = (preds/league_avg_slider)*100)
  

slider_pitcher_stuff <- slider_stuff_df %>%
  group_by(pitcherid) %>%
  summarise(
    cnt = n(),
    stuff = mean(stuff_plus_sl)) %>%
  filter(cnt > 10) %>%
  arrange(desc(stuff)) %>%
  left_join(slider_stuff_df %>%
              dplyr::select(pitcherid,pitcher_name), by = c('pitcherid')) %>%
  distinct()

#loop through each pitch type and make the stuff model 

statcast_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name != 'Other')

for (i in unique(statcast_stuff_df$pitch_type_name)){
  
  stuff_model_creation(statcast_stuff_df, i)
  print(i)
  
}

#load model 
fit_rf_changeup <- readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/fit_rf_Changeup.rds'))

prediction_probs <- predict(fit_rf_changeup, type = "prob")

# Extract the probability of the positive class (class 1)
positive_class_prob <- prediction_probs[, "1"]

league_avg_changeup <- mean(positive_class_prob)

changeup_stuff_df <- statcast_stuff_df %>%
  filter(pitch_type_name == 'Changeup') %>%
  mutate(preds = positive_class_prob,
         stuff_plus_ch = (preds/league_avg_changeup)*100)


changeup_pitcher_stuff <- changeup_stuff_df %>%
  group_by(pitcherid) %>%
  summarise(
    cnt = n(),
    stuff = mean(stuff_plus_ch)) %>%
  filter(cnt > 10) %>%
  arrange(desc(stuff)) %>%
  left_join(changeup_stuff_df %>%
              dplyr::select(pitcherid,pitcher_name), by = c('pitcherid')) %>%
  distinct()

#now all the models have been saved can make a tab for stuff models and then also a pitch builder 

stuff_predictions <- function(df, pitch_type) {
  
  rf_model <-readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/',paste0('fit_rf_',pitch_type,'.rds')))
  
  prediction_probs <- predict(rf_model, type = "prob")
  
  # Extract the probability of the positive class (class 1)
  positive_class_prob <- prediction_probs[, "1"]
  
  league_avg_pitch_types <- mean(positive_class_prob)
  
  stuff_df <- df %>%
    filter(pitch_type_name == pitch_type) %>%
    mutate(preds = positive_class_prob,
           stuff_plus = (preds/league_avg_pitch_types)*100)
  
  return(stuff_df)
  
}

# Create an empty list to store each df
stuff_dfs <- list()

# Loop through each pitch type
for (i in unique(statcast_stuff_df$pitch_type_name)){
  stuff_df <- stuff_predictions(statcast_stuff_df, i)
  stuff_dfs <- append(stuff_dfs, list(stuff_df))
}

# Bind the modified dataframes together
statcast_stuff_df_complete <- do.call(rbind, stuff_dfs)



























new_pitch <- data.frame(
  pfx_x_adj = 1,
  pfx_z = 1,
  release_speed = 1,
  release_spin_rate = 1,
  release_pos_x_adj = 1,
  release_pos_z = 1,
  release_extension = 1,
  release_pos_y = 1,
  spin_axis = 1
)




























#### shiny app #####

#build shiny app
ui <- fluidPage(
  theme = shinytheme('cosmo'),
                
                #page header
  headerPanel('Statcast Visualizations'),
                #navigation panel to different visualization tools
  navlistPanel(
    "Visuals",
    widths = c(2,10),
    tabPanel(
      "Pitch Highlighter",
      fluidRow(
        column(4,
               uiOutput('pitch_counter')
               ),
        column(4,
               h3('Pitch Highlighter'),
               selectInput(inputId = 'player_select',
                           label = 'Look Up A Player',
                           choices = unique(unique_ids_df$full_name),
                           selected = NULL),
               selectInput(inputId = 'player_type',
                           label = 'Player Type',
                           choices = c('Pitcher', 'Batter'),
                           selected = '')),
        column(4,
               uiOutput('filtering_plot') #this uioutput loads more selective filtering when the player is selected
               )
        ),
      fluidRow(
        column(12,
               tableOutput("test_table"), #used for testing data
               plotOutput("plate_loc_plot") #plate location plot
               ), 
        column(12,
               plotOutput("spray_chart") #spray chart plot
               ),
        column(12,
               plotOutput("radial_plot") #radial plot 
               ),
        column(12,
               tableOutput("player_summary_table"))
        )
    ),
    #this next tab will be for pitch arsenal
    tabPanel(
      "Pitch Arsenal",
      column(4,
             h1("Pitch Arsenal")
             ),
      column(12,
             p('An application to view pitch-type signatures, by frequency, speed, and break. Only showing pitchers with at least 100 pitches thrown.')
             ),
      column(4,
             #pitcher lookup > 100 pitches
             selectInput(inputId = 'pitcher_lu',
                         label = 'Select A Pitcher',
                         choices = frequent_pitchers %>%
                           dplyr::select(pitcher_name) %>%
                           unique(),
                         selected = NULL),
             br(),
             uiOutput('pitcher_selected_handedness'),
             br()
             ),
      column(12,
             plotOutput('pitch_arsenal_cowplot')
      ),
      column(12,
             checkboxInput(value = FALSE,
                    inputId = 'league_avg',
                    label = 'League Avg')
             ),
      column(12,
             plotOutput('pitch_movement_plot'),
             )
      ),
    tabPanel(
      "Pitcher Stuff",
      column(4,
             h1("Pitcher Stuff Table")
      ),
      column(12,
             p('This data is trained on swings and misses from the 2023 season'),
             splitLayout(
               cellWidths = c('50%','50%'),
               column(6,
                      selectInput(inputId = 'pitch_type_stuff',
                                  label = 'Select A Pitch Type',
                           choices = unique(statcast_stuff_df_complete$pitch_type_name),
                           selected = 'Four-Seam Fastball'),
               selectInput(inputId = 'total_pitches',
                           label = 'Minimum Pitches',
                           choices = c(1,10,25,50,100,150,200,250,300,350,400,450,500),
                           selected = 100),
               tableOutput('stuff_table')
               ),
              column(12,
                selectInput(inputId = 'pitcher_select_stuff',
                            label = 'Select A Pitcher',
                            choices = unique(statcast_stuff_df_complete$pitcher_name),
                            selected = NULL),
                br(),
                tableOutput('stuff_table_individual'),
                plotOutput('pitcher_individual_stuff_plot')
              ),
               )
             )
      ),
    tabPanel(
      "Create A Pitch",
      column(4,
             h1("Create A Pitch")
      ),
      column(12,
             p('Create your own pitch using the stuff models'),
             fluidRow(
             column(4,
                    selectInput(inputId = 'create_pitch_type_stuff',
                                label = 'Select A Pitch Type',
                                choices = unique(statcast_stuff_df_complete$pitch_type_name),
                                selected = 'Four-Seam Fastball')
                    ),
             column(4,
                    actionButton(inputId = 'create_new_pitch', 'Create New Pitch')
                    )
             ),
            column(12,
                   uiOutput('create_pitch_slider'),
                   tableOutput('create_pitch_stuff')
            ),
        )
      )
    )
  )


server <- function(input, output, session) {
  
  #reactive filter for player selection 
  selected_player_statcast_df <- reactive({
    statcast_df %>%
      filter((batter_name == input$player_select) | (pitcher_name == input$player_select),
             !is.na(release_pos_x), !is.na(release_pos_y))
  })

  observeEvent(input$player_type, {
        if (nrow(selected_player_statcast_df()) > 0) {
          output$filtering_plot <- renderUI({
            tagList(
              selectInput(inputId = 'pitcher_handedness',
                          label = 'Pitcher Throws',
                          choices = c('All','LHP','RHP'),
                          selected = 'All'),
              selectInput(inputId = 'batter_stands',
                          label = 'Batter Stands',
                          choices = c('All','LHB','RHB'),
                          selected = 'All'),
              selectInput(inputId = 'pitch_type_thrown',
                          label = 'Pitch Types',
                          choices = c('All', unique(statcast_df$pitch_type_name)),
                          selected = 'All'),
              selectInput(inputId = 'zone_location', #this will come later
                          label = 'Select Zone',
                          choices = c('All',statcast_df %>%
                                        filter(zone != 'No Zone Listed') %>%
                                        dplyr::select(zone) %>%
                                        unique(), statcast_df %>%
                                        filter(attack_region != 'No Region Listed') %>%
                                        dplyr::select(attack_region) %>%
                                        unique(),'In Zone', 'Out of Zone'),
                          selected = 'All')
            )
          })
        } else {
          print('There is no data on this Player!')
        }
      })

  #reactive filtering for additional inputs once the player has been selected
  selected_player_type <- reactive({
    req(input$player_type, input$pitcher_handedness, input$batter_stands,input$pitch_type_thrown)
    input_filtered_df <- selected_player_statcast_df()

    if (input$player_type == 'Pitcher') {
      input_filtered_df <- input_filtered_df %>%
        filter(pitcher_name == input$player_select)
    } else if (input$player_type == 'Batter') {
      input_filtered_df <- input_filtered_df %>%
        filter(batter_name == input$player_select)
    }

    if (input$pitcher_handedness == 'All') {
      input_filtered_df } else if (input$pitcher_handedness != 'All') {
        input_filtered_df <- input_filtered_df %>%
          filter(p_throws == ifelse(input$pitcher_handedness == 'LHP', 'L', 'R'))
      }

    if (input$batter_stands == 'All') {
      input_filtered_df } else if (input$batter_stands != 'All') {
        input_filtered_df <- input_filtered_df %>%
          filter(stand == ifelse(input$batter_stands == 'LHB', 'L', 'R'))
      }

    if (input$pitch_type_thrown == 'All') {
      input_filtered_df } else if (input$pitch_type_thrown != 'All') {
        input_filtered_df <- input_filtered_df %>%
        filter(pitch_type_name == input$pitch_type_thrown)
      }
    if (input$zone_location == 'All') {
      input_filtered_df } else if (input$zone_location == 'In Zone') {
        input_filtered_df <- input_filtered_df %>%
          filter(zone %in% 1:9) } else if (input$zone_location == 'Out of Zone') {
            input_filtered_df <- input_filtered_df %>%
              filter(zone %in% 10:14) } else if (input$zone_location %in% 1:14 ) {
                input_filtered_df <- input_filtered_df %>%
                  filter(zone == input$zone_location) } else if (input$zone_location == 'Heart' ) {
                    input_filtered_df <- input_filtered_df %>%
                      filter(attack_region == 'Heart') } else if (input$zone_location == 'Shadow' ) {
                        input_filtered_df <- input_filtered_df %>%
                          filter(attack_region == 'Shadow') } else if (input$zone_location == 'Chase' ) {
                            input_filtered_df <- input_filtered_df %>%
                              filter(attack_region == 'Chase') } else if (input$zone_location == 'Waste' ) {
                                input_filtered_df <- input_filtered_df %>%
                                  filter(attack_region == 'Waste') }
          })


  #create the pitch counter for the top of the page
  output$pitch_counter <- renderText({ paste(HTML(paste(input$player_select, '<br>',
                     nrow(selected_player_type()), 'of', nrow(selected_player_statcast_df()), 'Total Pitches')))
  })

      # #first visualization will be for pitch highlighter
      output$plate_loc_plot <- renderPlot(
        ggplot(data = selected_player_type() %>%
                 filter(complete.cases(plate_x, plate_z))) +
          coord_equal() +
          geom_point(aes(x = plate_x, y = plate_z, color = as.factor(pitch_type_name))) +
          geom_segment(aes(x = -.9, y = 1.5, xend = -.9, yend = 3.5), linewidth = 0.5, color = "black") +
          geom_segment(aes(x = -.9, y = 3.5, xend = .9, yend = 3.5), linewidth = 0.5, color = "black") +
          geom_segment(aes(x =  .9, y = 3.5, xend = .9, yend = 1.5), linewidth = 0.5, color = "black") +
          geom_segment(aes(x = .9, y = 1.5, xend = -.9, yend = 1.5), linewidth = 0.5, color = "black") +
          geom_segment(aes(x = -.9, y = 1.5, xend = -.9, yend = 1.5), linewidth = 0.5, color = "black") +
          # geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", fill = "transparent",size=1.1) + #another way to do the sz
          geom_segment(aes(x = 0, y = -.1, xend = -.9, yend = .1), linewidth = 0.5, color = "black") +
          geom_segment(aes(x = -.9, y = .1, xend = -.7, yend = .4), linewidth = 0.5, color = "black") +
          geom_segment(aes(x =  -.7, y = .4, xend = .7, yend = 0.4), linewidth = 0.5, color = "black") +
          geom_segment(aes(x = .7, y = 0.4, xend = .9, yend = .1), linewidth = 0.5, color = "black") +
          geom_segment(aes(x = .9, y = .1, xend = 0, yend = -.1), linewidth = 0.5, color = "black") +
          scale_color_manual(values = pitch_palette, name = 'Pitch Type') +
          labs(title=paste("Pitch Location For", input$player_select, 'As', input$player_type),
             x ="Plate Side", y = "Plate Height",
             fill = 'Pitch Type Breakdown') +
        theme_classic()
      )


      #results of balls put into play
      output$spray_chart <- renderPlot(
        ggplot(data = selected_player_type() %>%
                 filter(is_bip == 1)) +
          geom_mlb_stadium(stadium_ids = "mets",stadium_transform_coords = TRUE,stadium_segments = "all") +
          geom_point(aes(x = location_x_ft, y = location_y_ft, color = as.factor(event_type))) +
          scale_color_manual(values = result_palette, name = 'Results') +
          labs(fill = 'Results') +
          coord_equal() +
          theme_void() +
          annotate(geom = "text", x = 0, y = 425, label = '425', color = "black") +
          annotate(geom = "text", x = -220, y = 300, label = '330', color = "black") +
          annotate(geom = "text", x = 230, y = 290, label = '330', color = "black")
          # labs(fill = 'Results')
      )

      # define boundaries of polygons
      th <- seq(- pi / 2, pi / 2, length.out = 200)
      df_new <- data.frame(x = cos(th), y = sin(th))
      df_add <- data.frame(x = 0, y = sin(- pi / 2))
      df_new2 <- rbind(df_new, df_add)

      #radial plot
      output$radial_plot <- renderPlot(
        ggplot() +
        coord_equal() +
        geom_polygon(data = df_new2, aes(x, y), fill = "lightblue") +
        geom_polygon(data = df_new2, aes(x / 2, y / 2), fill = "lightyellow") +
        geom_segment(aes(x = 0, y = 0, xend = cos(pi / 4), yend = sin(pi / 4)),
                     color = "grey") +
        geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0),
                     color = "grey") +
        geom_point(data = selected_player_type() %>%
                     filter(complete.cases(launch_speed_angle,as.numeric(Xcoord),
                                           as.numeric(Ycoord),contact_type) & is_bip == 1),
                   aes(Xcoord, Ycoord, color = contact_type), size = 1) +
          scale_color_manual(values = contact_palette, name = 'Contact') +
          labs(fill = 'Contact') +
          annotate(geom = "text", x = 0.75, y = 0.75, label = '45°', color = "black") +
          annotate(geom = "text", x = 1.05, y = 0, label = '0°', color = "black") +
          annotate(geom = "text", x = 0.05, y = 1.05, label = '90°', color = "black") +
          annotate(geom = "text", x = 0.05, y = -1.05, label = '-90°', color = "black") +
          annotate(geom = "text", x = 0.57, y = 0.91, label = "120mph", color = "black") +
          annotate(geom = "text", x = 0.1, y = 0.45, label = "60mph", color = "black") +
          theme_void() +
          theme(panel.grid = element_blank(),axis.title = element_blank(),
                axis.text = element_blank(),axis.ticks = element_blank(),
                panel.background = element_blank()))

      # output$test_table <- renderTable(selected_player_type() %>% head())

      #create the table with the summary info for the player
      player_summary_table <- reactive({
          selected_player_type() %>%
          summarize(name = input$player_select,
                    pitches = n(),
                    total_pitches = nrow(selected_player_statcast_df()),
                    pct_pitches = pitches/total_pitches,
                    hits = sum(hit),
                    single = sum(event_type == 'single'),
                    double = sum(event_type == 'double'),
                    triple = sum(event_type == 'triple'),
                    hr = sum(event_type == 'home_run'),
                    pa = hits + sum(event_type == 'walk' | event_type == 'strikeout' | event_type == 'hit_by_pitch' |
                                      event_type == 'sac_fly' | event_type == 'sac_fly_double_play' | event_type == 'sac_bunt' |
                                      event_type == 'grounded_into_double_play'| event_type == 'field_out'|
                                      event_type == 'field_error'| event_type == 'fielders_choice_out'|
                                      event_type == 'fielders_choice'| event_type == 'strikeout_double_play'| event_type == 'catcher_interf' |
                                      event_type == 'force_out' |event_type == 'other_out' | event_type == 'double_play'),
                    ab = hits + sum(event_type == 'fielders_choice' | event_type == 'field_error' | event_type == 'fielders_choice_out' |
                                      event_type == 'grounded_into_double_play' | event_type == 'force_out' | event_type == 'field_out' |
                                      event_type == 'strikeout' | event_type == 'strikeout_double_play' | event_type == 'double_play'),
                    ba = sprintf("%.3f", hits/ab),
                    so = sum(event_type == 'strikeout'),
                    ev = mean(launch_speed[is_bip==1 & !is.na(launch_speed)]),
                    spin_rate = mean(release_spin_rate[!is.na(release_spin_rate)]),
                    swing = sum(description == 'hit_into_play'|description == 'swinging_strike_blocked'|description == 'foul'|
                                  description == 'swinging_strike'|description == 'foul_tip'),
                    misses = sum(description == 'swinging_strike' | description == 'swinging_strike_blocked' | description == 'foul_tip'),
                    sw_pct = (swing/pitches) * 100,
                    whiff_pct = (misses/swing) * 100,
                    k_pct = (so/pa) * 100,
                    woba = sprintf("%.3f",mean(woba_value[!is.na(woba_value)])))
      })

      output$player_summary_table <- renderTable(player_summary_table() %>%
                                                   dplyr::select(Name = name, `Total Pitches` = total_pitches, Pitches = pitches,
                                                          `Pitch %` = pct_pitches, PA = pa, AB = ab, H = hits,
                                                          `1B` = single, `2B` = double, `3B` = triple, HR = hr,
                                                          BA = ba, SO = so, Swings = swing, Misses = misses,
                                                          `SW%` = sw_pct, `Whiff%` = whiff_pct, `K%` = k_pct, wOBA = woba,
                                                          EV = ev, `Spin Rate` = spin_rate ))

      #this next part will be for pitch arsenal visualizations

  #create dynamic filtering for pitcher plots
  selected_pitcher_statcast_df <- reactive({
    frequent_pitchers %>%
      filter((pitcher_name == input$pitcher_lu),
             !is.na(release_pos_x), !is.na(release_pos_y), !is.na(pitch_type),
             pitch_name != 'Pitch Out', pitch_name != '')
  })

  filter_pitcher_handedness <- reactive({
    selected_pitcher_statcast_df() %>%
      dplyr::select(p_throws) %>%
      unique()
  })

  #create the pitch counter for the top of the page
  output$pitcher_selected_handedness <- renderText({
    paste(input$pitcher_lu, selected_pitcher_statcast_df() %>%
            mutate(pitcher_handedness = case_when(
              p_throws == 'L' ~ 'LHP',
              p_throws == 'R' ~ 'RHP',
              TRUE ~ 'No Hands')) %>%
            dplyr::select(pitcher_handedness) %>%
            distinct())
    })


  pitcher_freq_dist <- reactive({
    selected_pitcher_statcast_df() %>%
    group_by(pitch_name) %>%
    summarize(
      pitch_type_cnt = n(),
      total_pitches = nrow(selected_pitcher_statcast_df()),
      pitch_type_freq = round((pitch_type_cnt / total_pitches) *100,2),
      pitch_velo = mean(release_speed[!is.na(release_speed)])
    ) %>%
    ungroup()
  })

  # # Create the bar plot
  pitch_frequency_bar_plot <- reactive({
    ggplot(pitcher_freq_dist(), aes(x = pitch_type_freq, y = reorder(pitch_name,pitch_type_freq))) +
    geom_bar(stat = "identity",aes(fill = pitch_name),width = 0.5) +
    geom_text(aes(label=pitch_name, x = 8), position = position_nudge(y = 0.33), size = 4.5) +
    geom_text(aes(label = paste0(pitch_type_freq,'%')), hjust = 1.1) +
    scale_fill_manual(values = pitch_arsenal_palette, name = 'Pitch Type') +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "gray85",linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("Pitch Type Frequency") +
    theme(legend.position = "none") +
    scale_x_continuous(expand = c(0, 0), limits = c(0,max(pitcher_freq_dist()$pitch_type_freq) + 10),position = "top") +
    coord_trans(x = "reverse")
  })

  # Create the scatter plot
  pitch_speed_scatter_plot <- reactive({
    ggplot(pitcher_freq_dist(), aes(x = pitch_velo, y = reorder(pitch_name,pitch_type_freq))) +
    geom_point(aes(color = pitch_name, size = 10)) +
    geom_text(aes(label = paste(round(pitch_velo,1),'mph')), vjust = -1) +
    scale_color_manual(values = pitch_arsenal_palette, name = 'Pitch Type') +
    xlab("Velocity (mph)") +
    ylab(NULL) +
    theme(legend.position = "none",
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "gray92",linetype = "dashed"),
          panel.grid.major.y = element_line(color = "gray85"),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(limits = c(65, 105))
  })

  #add a rug plot at the bottom of the scatter plot to display the density of pitch speeds

  # Arrange the two plots together
  output$pitch_arsenal_cowplot <- renderPlot(plot_grid(pitch_frequency_bar_plot() + theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
                             axis.ticks.y = element_blank(),plot.margin = margin(0, 0, 0, 0)),
            pitch_speed_scatter_plot() + theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
                                 axis.line.y = element_line(color = "black", size = 1),
                                 axis.ticks.y = element_blank(),plot.margin = margin(0, 0, 0, 0)),
            align = 'h')
            )

  #create a summary table for the selected pitcher
  pitcher_movement_summary_table <- reactive({
    selected_pitcher_statcast_df() %>%
    group_by(pitch_name) %>%
    summarize(
      avg_horz_break = mean(pfx_x_inches[!is.na(pfx_x_inches)]),
      avg_ivb = mean(pfx_z_inches[!is.na(pfx_z_inches)]),
      horz_break_sd = sd(pfx_x_inches[!is.na(pfx_x_inches)]),
      ivb_sd = sd(pfx_z_inches[!is.na(pfx_z_inches)]),
    ) %>%
    ungroup() %>%
    na.omit()
  })

  #filter the league average dataframe to match the handedness of the selected player
  league_avg_filter_handedness <- reactive({
    league_avg_by_handedness %>%
      filter(p_throws == filter_pitcher_handedness()$p_throws, pitch_name %in% selected_pitcher_statcast_df()$pitch_name)
  })


  #league average plot
  output$pitch_movement_plot <- renderPlot(

    if (input$league_avg == FALSE) {

      ggplot(pitcher_movement_summary_table(), aes(avg_horz_break, avg_ivb)) +
        coord_equal() +
        geom_vline(xintercept = 0, color = "gray75") +
        geom_hline(yintercept = 0, color = "gray75") +
        geom_point(aes(color = pitch_name),size = 1) +
        geom_ellipse(aes(x0 = avg_horz_break, y0 = avg_ivb, a = horz_break_sd, b = ivb_sd, angle = 0, fill = pitch_name), alpha = 0.55) +
        scale_color_manual(values = pitch_arsenal_palette, name = 'Pitch Type') +
        scale_fill_manual(values = pitch_arsenal_palette, name = 'Pitch Type') +
        theme_minimal() +
        labs(x = "Horizontal Break (in)\nCatcher's POV", y = "Induced Vertical Break (in)") +
        annotate("text", x = -28, y = -28, label = "3B Side") +
        annotate("text", x = 28, y = -28, label = "1B Side") +
        ggtitle(paste(input$pitcher_lu, "by Pitch Type")) +
        xlim(-30,30) +
        ylim(-30,30) }

    else if (input$league_avg == TRUE) {

      ggplot() +
        coord_equal() +
        geom_vline(xintercept = 0, color = "gray75") +
        geom_hline(yintercept = 0, color = "gray75") +
        geom_point(data = pitcher_movement_summary_table(), aes(x = avg_horz_break, y = avg_ivb, color = pitch_name), size = 1) +
        geom_ellipse(data = pitcher_movement_summary_table(), aes(x0 = avg_horz_break, y0 = avg_ivb, a = horz_break_sd, b = ivb_sd, angle = 0, fill = pitch_name), alpha = 0.55) +
        geom_point(data = league_avg_filter_handedness(), aes(x = la_horz_break, y = la_ivb, color = pitch_name),size = 1) +
        geom_ellipse(data = league_avg_filter_handedness(), aes(x0 = la_horz_break, y0 = la_ivb,
                                                          a = league_horz_break_sd, b = league_ivb_sd, angle = 0, fill = pitch_name),
                     alpha = 0.1, linetype = 'dashed') +
        scale_color_manual(values = pitch_arsenal_palette, name = 'Pitch Type') +
        scale_fill_manual(values = pitch_arsenal_palette, name = 'Pitch Type') +
        theme_minimal() +
        labs(x = "Horizontal Break (in)\nCatcher's POV", y = "Induced Vertical Break (in)") +
        annotate("text", x = -28, y = -28, label = "3B Side") +
        annotate("text", x = 28, y = -28, label = "1B Side") +
        ggtitle(paste(input$pitcher_lu, "by Pitch Type")) +
        xlim(-30,30) +
        ylim(-30,30) }
    else {
      print('there is no data to display!')
    }
  )

  # rendering table for stuff tab
  statcast_stuff_df_filtered <- reactive({
    statcast_stuff_df_complete %>%
      filter(pitch_type_name == input$pitch_type_stuff)
  })

  stuff_summary_table <- reactive({
    statcast_stuff_df_filtered() %>%
    group_by(pitcherid) %>%
    summarise(
      pitches = n(),
      stuff = mean(stuff_plus)) %>%
    arrange(desc(stuff)) %>%
    left_join(statcast_stuff_df_filtered() %>%
                dplyr::select(pitcherid,pitcher_name), by = c('pitcherid')) %>%
    distinct() %>%
    filter(pitches >= as.numeric(input$total_pitches)) %>%
    dplyr::select(-c('pitcherid')) %>%
    dplyr::select(Pitcher = pitcher_name, Pitches = pitches, `Stuff+` = stuff)
  })

  output$stuff_table <- renderTable(
    stuff_summary_table()
  )

  #individual pitcher stuff
  statcast_stuff_df_filtered_individual <- reactive({
    statcast_stuff_df_complete %>%
      filter(pitcher_name == input$pitcher_select_stuff)
  })

  stuff_individual_summary_table <- reactive({
    statcast_stuff_df_filtered_individual() %>%
      group_by(pitch_type_name) %>%
      summarise(
        pitches = n(),
        stuff = mean(stuff_plus)) %>%
      arrange(desc(stuff)) %>%
      dplyr::select(`Pitch Type` = pitch_type_name, Pitches = pitches, `Stuff+` = stuff)
  })

  output$stuff_table_individual <- renderTable(
    stuff_individual_summary_table()
  )

  #filter for all pitches the pitcher throws for the graph to display the correct pitch types
  all_pitches_individual_throws <- reactive({
    statcast_stuff_df_complete %>%
      filter(pitch_type_name %in% statcast_stuff_df_filtered_individual()$pitch_type_name) %>%
      rename(`Pitch Type` = pitch_type_name)

  })

  avg_stuff_pitches_individual_throws <- reactive({
    statcast_stuff_df_complete %>%
      filter(pitch_type_name %in% statcast_stuff_df_filtered_individual()$pitch_type_name) %>%
      summarise(avg_stuff_plus = mean(stuff_plus))

  })

  #make the vline for the given player

  output$pitcher_individual_stuff_plot <- renderPlot({

      ggplot(all_pitches_individual_throws()) +
        geom_histogram(data = all_pitches_individual_throws(), aes(x = stuff_plus, fill = `Pitch Type`)) +
        scale_fill_manual(values = pitch_palette, name = 'Pitch Type') +
        geom_vline(data = avg_stuff_pitches_individual_throws(), aes(xintercept = avg_stuff_plus),
                   linetype = 'dashed', alpha = 0.85) +
        facet_wrap(~`Pitch Type` , scales = 'free') +
        geom_vline(data = stuff_individual_summary_table(), aes(xintercept = `Stuff+`)) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title="Stuff By Pitch Type",
             x ="Stuff+", y = "Count")

  })


  statcast_stuff_df_create_pitch <- reactive({
    statcast_stuff_df_complete %>%
      filter(pitch_type_name == input$create_pitch_type_stuff)
  })

  #creating the create a pitch sliders using an observe event on what pitch type is selected
  observeEvent(input$create_pitch_type_stuff, {
        if (nrow(statcast_stuff_df_create_pitch()) > 0) {
          output$create_pitch_slider <- renderUI({
            column(12,
            fluidRow(
              column(4, sliderInput(inputId = "rel_speed_slider",
                                    label = "Release Speed",
                                    min = min(statcast_stuff_df_create_pitch()$release_speed),
                                    max = max(statcast_stuff_df_create_pitch()$release_speed),
                                    value = mean(statcast_stuff_df_create_pitch()$release_speed))),
              column(4, sliderInput(inputId = "rel_spin_slider",
                                    label = "Release Spin",
                                    min = min(statcast_stuff_df_create_pitch()$release_spin_rate),
                                    max = max(statcast_stuff_df_create_pitch()$release_spin_rate),
                                    value = mean(statcast_stuff_df_create_pitch()$release_spin_rate))),
              column(4, sliderInput(inputId = "spin_axis_slider",
                                    label = "Spin Axis",
                                    min = min(statcast_stuff_df_create_pitch()$spin_axis),
                                    max = max(statcast_stuff_df_create_pitch()$spin_axis),
                                    value = mean(statcast_stuff_df_create_pitch()$spin_axis))),
            ),
            fluidRow(
              column(4, sliderInput(inputId = "rel_pos_x_slider",
                                    label = "Release Position X",
                                    min = min(statcast_stuff_df_create_pitch()$release_pos_x_adj),
                                    max = max(statcast_stuff_df_create_pitch()$release_pos_x_adj),
                                    value = mean(statcast_stuff_df_create_pitch()$release_pos_x_adj))),
              column(4, sliderInput(inputId = "rel_pos_y_slider",
                                    label = "Release Position Y",
                                    min = min(statcast_stuff_df_create_pitch()$release_pos_y),
                                    max = max(statcast_stuff_df_create_pitch()$release_pos_y),
                                    value = mean(statcast_stuff_df_create_pitch()$release_pos_y))),
              column(4, sliderInput(inputId = "rel_pos_z_slider",
                                    label = "Release Position Z",
                                    min = min(statcast_stuff_df_create_pitch()$release_pos_z),
                                    max = max(statcast_stuff_df_create_pitch()$release_pos_z),
                                    value = mean(statcast_stuff_df_create_pitch()$release_pos_z))),
            ),
            fluidRow(
              column(4, sliderInput(inputId = "pfx_x_slider",
                                    label = "PFX X",
                                    min = min(statcast_stuff_df_create_pitch()$pfx_x_adj),
                                    max = max(statcast_stuff_df_create_pitch()$pfx_x_adj),
                                    value = mean(statcast_stuff_df_create_pitch()$pfx_x_adj))),
              column(4, sliderInput(inputId = "pfx_z_slider",
                                    label = "PFX Z",
                                    min = min(statcast_stuff_df_create_pitch()$pfx_z),
                                    max = max(statcast_stuff_df_create_pitch()$pfx_z),
                                    value = mean(statcast_stuff_df_create_pitch()$pfx_z))),
              column(4, sliderInput(inputId = "rel_ext_slider",
                                    label = "Release Extension",
                                    min = min(statcast_stuff_df_create_pitch()$release_extension),
                                    max = max(statcast_stuff_df_create_pitch()$release_extension),
                                    value = mean(statcast_stuff_df_create_pitch()$release_extension))),
            )
            )

          })
        } else {
          print('There is no data on this pitch type!')
        }
      })

  create_pitch_type_store <- reactiveVal(NULL)
  create_pitch_df_store <- reactiveVal(NULL)
  new_stuff_df <- reactiveVal(NULL)

  #action event for button being pressed
  observeEvent(input$create_new_pitch, {

    pitch_type_name <- input$create_pitch_type_stuff

    if (!any(is.null(list(
      input$pfx_x_slider, input$pfx_z_slider,
      input$rel_speed_slider, input$rel_spin_slider,
      input$rel_pos_x_slider, input$rel_pos_z_slider,
      input$rel_ext_slider, input$rel_pos_y_slider,
      input$spin_axis_slider
    )))) {

      new_pitch <- data.frame(
        pfx_x_adj = input$pfx_x_slider,
        pfx_z = input$pfx_z_slider,
        release_speed = input$rel_speed_slider,
        release_spin_rate = input$rel_spin_slider,
        release_pos_x_adj = input$rel_pos_x_slider,
        release_pos_z = input$rel_pos_z_slider,
        release_extension = input$rel_ext_slider,
        release_pos_y = input$rel_pos_y_slider,
        spin_axis = input$spin_axis_slider
      )
    }

    create_pitch_type_store(pitch_type_name)
    create_pitch_df_store(new_pitch)

    rf_model <-readRDS(file.path('/Users/sam/Desktop/personal/NickWanBootcamp/',paste0('fit_rf_',create_pitch_type_store(),'.rds')))

    prediction_probs <- predict(rf_model, type = "prob")

    prediction_probs_new_pitch <- predict(rf_model, type = "prob", newdata = create_pitch_df_store())

    # Extract the probability of the positive class (class 1)
    positive_class_prob <- prediction_probs[, "1"]

    positive_class_prob_new_pitch <- prediction_probs_new_pitch[,"1"]

    league_avg_pitch_types <- mean(positive_class_prob)


    new_stuff_df(create_pitch_df_store() %>%
                   mutate(preds = positive_class_prob_new_pitch,
                          stuff_plus = (preds/league_avg_pitch_types)*100))


  })

  output$create_pitch_stuff <- renderTable({
    if (!is.null(new_stuff_df())) {
      new_stuff_df() %>%
        select(`Stuff+` = stuff_plus)
    }
  })
  
  
  
  
  
}


shinyApp(ui = ui, server = server)

