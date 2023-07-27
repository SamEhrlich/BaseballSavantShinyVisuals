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

# #load in data from pybaseball request
# saveRDS(statcast_df_small, file = "statcast_df_small.rds")
statcast_df_small <- readRDS("statcast_df_small.rds")

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
    pfx_z_inches = pfx_z *12)

#chadwick mlb player ids
mlb_players <- readRDS("mlb_players.rds")

#create a full name column
mlb_players <- mlb_players %>%
  mutate(full_name = paste0(name_last,', ',name_first)) %>%
  select(full_name, name_last, name_first, key_mlbam)

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

#change the launch angle to radial 
statcast_df <- statcast_df %>% 
  mutate(Xcoord = launch_speed / 120 *
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
         ))

#set color scheme
pitch_palette <- rep(c("Four-Seam Fastball" = "goldenrod2", "Sinker" = "deepskyblue4", "Slider" = "red2", "Changeup" = "forestgreen",
                       "Curveball" = "violetred3", "Cutter" = "purple3", "Knuckle Curve" = "wheat3",
                       "Other" = "grey30"))

result_palette <- rep(c("field_out" = "goldenrod2", "single" = "deepskyblue4", "home_run" = "red2", "force_out" = "forestgreen",
                        "double" = "violetred3", "triple" = "purple3", "grounded_into_double_play" = "wheat3",
                        "sac_fly" = "grey30", 'field_error' = 'darkgreen', 'sac_bunt' = 'lightblue1', 'fielders_choice'= 'lightyellow',
                        'fielders_choice_out' = 'darkorange1', 'double_play' = 'brown','sac_fly_double_play' = 'magenta'))

contact_palette <- rep(c("Weak Contact" = "goldenrod2", "Topped" = "deepskyblue4", "Hit Under" = "red2", "Flare/Burner" = "forestgreen",
                         "Solid Contact" = "violetred3", "Barrels" = "purple3"))


#add a column for full name of pitch 
statcast_df <- statcast_df %>%
  mutate(pitch_type_name = case_when(
    pitch_type == 'CH' ~ 'Curveball',
    pitch_type == 'CU' ~ 'Changeup',
    pitch_type == 'FC' ~ 'Cutter',
    pitch_type == 'FF' ~ 'Four-Seam Fastball',
    pitch_type == 'FS' ~ 'Sinker',
    pitch_type == 'KC' ~ 'Knuckle Curve',
    pitch_type == 'SL' ~ 'Slider',
    pitch_type == 'SI' ~ 'Sinker',
    TRUE ~ 'Other'
  ))

statcast_df <- statcast_df %>%
  mutate(rel_plate_x = plate_x / ((17/2 + 1.456) / 12),
         rel_plate_z = (plate_z - (sz_bot - 1.456/12)) /
           (sz_top - sz_bot + 1.456*2/12) * 2 - 1,
         attack_region = case_when(
           abs(rel_plate_x) < 0.67 & abs(rel_plate_z) < 0.67 ~ "Heart",
           abs(rel_plate_x) < 1.33 & abs(rel_plate_z) < 1.33 ~ "Shadow",
           abs(rel_plate_x) < 2.00 & abs(rel_plate_z) < 2.00 ~ "Chase",
           TRUE ~ "Waste"),
         attack_region = ifelse(is.na(plate_x) | is.na(plate_z), 'No Region Listed', attack_region),
         zone = ifelse(is.na(zone),'No Zone Listed',zone))

pitch_arsenal_palette <- rep(c("4-Seam Fastball" = "goldenrod2", "Sinker" = "deepskyblue4", "Slider" = "red2", "Changeup" = "forestgreen",
                               "Curveball" = "violetred3", "Cutter" = "purple3", "Knuckle Curve" = "wheat3",
                               "Other" = "grey30", 'Split-Finger' = 'darkgreen', 'Eephus' = 'lightblue1', 'Forkball'= 'lightyellow',
                               'Sweeper' = 'darkorange1', 'Screwball' = 'brown','Slurve' = 'magenta', 'Slow Curve' = 'navyblue', 'Knuckleball' = 'orangered' ))

#bar plot, scatter plot for mph and scatterplot for IVB and Horz break
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
                           select(pitcher_name) %>%
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
                                    select(zone) %>%
                                    unique(), statcast_df %>%
                                    filter(attack_region != 'No Region Listed') %>%
                                    select(attack_region) %>%
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
  
  
  # #results of balls put into play
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
  
  # # define boundaries of polygons
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
  # 
  # #create the table with the summary info for the player
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
                                               select(Name = name, `Total Pitches` = total_pitches, Pitches = pitches,
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
      select(p_throws) %>%
      unique()
  })
  
  #create the pitch counter for the top of the page
  output$pitcher_selected_handedness <- renderText({
    paste(input$pitcher_lu, selected_pitcher_statcast_df() %>%
            mutate(pitcher_handedness = case_when(
              p_throws == 'L' ~ 'LHP',
              p_throws == 'R' ~ 'RHP',
              TRUE ~ 'No Hands')) %>%
            select(pitcher_handedness) %>%
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
  
  
  
  
  
  
}


shinyApp(ui = ui, server = server)





















