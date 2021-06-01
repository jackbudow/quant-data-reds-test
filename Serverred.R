library(tidyverse)
library(maps)
library(ggthemes)
library(mapproj)
library(dplyr)
library(stringr)
library(DataExplorer)
library(rgdal)
library(lubridate)
library(GGally)
library(shiny)
library(shinydashboard)

server <- function(input, output, session) {
  
  data = read.csv('pitch_data_one_month.csv')
  
  pitcher_statistics <- data %>%
    select(EVENT_RESULT, PITCHER_ID) %>%
    group_by(PITCHER_ID) %>%
    summarise(
      IP = (sum(EVENT_RESULT == 'field_out', na.rm=TRUE) + sum(EVENT_RESULT == 'strikeout', na.rm=TRUE) + sum(EVENT_RESULT == 'sac_bunt', na.rm=TRUE) +
              sum(EVENT_RESULT == 'fielders_choice', na.rm=TRUE) + sum(EVENT_RESULT == 'force_out', na.rm=TRUE) + (2 * sum(EVENT_RESULT == 'grounded_into_double_play', na.rm=TRUE)) +
              sum(EVENT_RESULT == 'caught_stealing_2b', na.rm=TRUE) + sum(EVENT_RESULT == 'sac_fly', na.rm=TRUE) + (2 * sum(EVENT_RESULT == 'double_play', na.rm=TRUE)) +
              sum(EVENT_RESULT == 'pickoff_caught_stealing_2b', na.rm=TRUE) + sum(EVENT_RESULT == 'fielders_choice_out', na.rm=TRUE) + sum(EVENT_RESULT == 'caught_stealing_3b', na.rm=TRUE) +
              sum(EVENT_RESULT == 'caught_stealing_home', na.rm=TRUE) + (2 * sum(EVENT_RESULT == 'strikeout_double_play', na.rm=TRUE)) + (2 * sum(EVENT_RESULT == 'sac_fly_double_play', na.rm=TRUE)) +
              sum(EVENT_RESULT == 'other_out', na.rm=TRUE) + (2 * sum(EVENT_RESULT == 'sac_bunt_double_play', na.rm=TRUE)) + sum(EVENT_RESULT == 'pickoff_caught_stealing_3b', na.rm=TRUE) + (3 * sum(EVENT_RESULT == 'triple_play', na.rm=TRUE)))/3,
      K = sum(EVENT_RESULT == 'strikeout', na.rm=TRUE),
      BB = sum(EVENT_RESULT == 'walk', na.rm=TRUE),
      HR = sum(EVENT_RESULT == 'home_run', na.rm=TRUE),
      H = sum(EVENT_RESULT == 'single', na.rm=TRUE) + sum(EVENT_RESULT == 'double', na.rm=TRUE) + sum(EVENT_RESULT == 'triple', na.rm=TRUE) + sum(EVENT_RESULT == 'home_run', na.rm=TRUE),
      HBP = sum(EVENT_RESULT == 'hit_by_pitch', na.rm=TRUE),
      IBB = sum(EVENT_RESULT == 'intent_walk', na.rm=TRUE),
      BFP = sum(EVENT_RESULT == 'field_out', na.rm=TRUE) + sum(EVENT_RESULT == 'strikeout', na.rm=TRUE) + sum(EVENT_RESULT == 'sac_bunt', na.rm=TRUE) +
        sum(EVENT_RESULT == 'fielders_choice', na.rm=TRUE) + sum(EVENT_RESULT == 'force_out', na.rm=TRUE) + sum(EVENT_RESULT == 'grounded_into_double_play', na.rm=TRUE) +
        sum(EVENT_RESULT == 'sac_fly', na.rm=TRUE) + sum(EVENT_RESULT == 'double_play', na.rm=TRUE) + sum(EVENT_RESULT == 'triple', na.rm=TRUE) + sum(EVENT_RESULT == 'home_run', na.rm=TRUE) +
        sum(EVENT_RESULT == 'fielders_choice_out', na.rm=TRUE) + sum(EVENT_RESULT == 'single', na.rm=TRUE) + sum(EVENT_RESULT == 'double', na.rm=TRUE) + sum(EVENT_RESULT == 'hit_by_pitch', na.rm=TRUE) +
        sum(EVENT_RESULT == 'strikeout_double_play', na.rm=TRUE) + sum(EVENT_RESULT == 'sac_fly_double_play', na.rm=TRUE) + sum(EVENT_RESULT == 'field_error', na.rm=TRUE) + sum(EVENT_RESULT == 'walk', na.rm=TRUE) +
        sum(EVENT_RESULT == 'other_out', na.rm=TRUE) + sum(EVENT_RESULT == 'sac_bunt_double_play', na.rm=TRUE) + sum(EVENT_RESULT == 'triple_play', na.rm=TRUE),
      K9 = K * 9 / IP,
      BB9 = BB * 9 / IP,
      HR9 = HR * 9 / IP,
      OBA = H / BFP,
      KPerc = K / BFP * 100,
      BBPerc = BB / BFP * 100,
      WHIP = (BB + H) / IP,
      FIP = ((13 * HR) + (3 * (BB + HBP)) - (2 * K)) / IP + 3.24
    )
  
  pitchers = sort(unique(pitcher_statistics$PITCHER_ID))
  
  updateSelectInput(session, "Pitcher", choices = pitchers)
  
  output$kBox <- renderValueBox({
    shinydashboard::valueBox(
      value = paste(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "K"]),
      "K",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  output$bbBox <- renderValueBox({
    shinydashboard::valueBox(
      value = paste(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "BB"]),
      "BB",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  output$hrBox <- renderValueBox({
    shinydashboard::valueBox(
      value = paste(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "HR"]),
      "HR",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  output$k9Box <- renderValueBox({
    shinydashboard::valueBox(
      value = paste(round(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "K9"], 2)),
      "K/9",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  output$bb9Box <- renderValueBox({
    shinydashboard::valueBox(
      value = paste(round(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "BB9"], 2)),
      "BB/9",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  output$whipBox <- renderValueBox({
    shinydashboard::valueBox(
      value = paste(round(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "WHIP"], 2)),
      "WHIP",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  output$kpercBox <- renderValueBox({
    shinydashboard::valueBox(
      value = gsub(" ", "", paste(round(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "KPerc"], 1), "%")),
      "K %",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  output$bbpercBox <- renderValueBox({
    shinydashboard::valueBox(
      value = gsub(" ", "", paste(round(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "BBPerc"], 1), "%")),
      "BB %",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  output$obaBox <- renderValueBox({
    shinydashboard::valueBox(
      value = paste(round(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "OBA"], 3)),
      "OBA",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  output$fipBox <- renderValueBox({
    shinydashboard::valueBox(
      value = paste(round(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "FIP"], 2)),
      "FIP",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  output$hitBox <- renderValueBox({
    shinydashboard::valueBox(
      value = paste(round(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "H"], 2)),
      "Hits",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  output$ipBox <- renderValueBox({
    shinydashboard::valueBox(
      value = paste(round(pitcher_statistics[pitcher_statistics$PITCHER_ID == input$Pitcher, "IP"], 2)),
      "IP",
      icon = NULL,
      color = "red",
      width = NULL
    )
  })
  
  data2 <- data %>%
    filter(PITCH_LOCATION_HEIGHT > 0.0) %>%
    mutate(
      "ADJ_PITCH_HEIGHT" = 2 / (STRIKE_ZONE_TOP - STRIKE_ZONE_BOTTOM) * PITCH_LOCATION_HEIGHT
    )
  
  strikeZoneFrame <- data.frame(
    x=c(-0.95,.95),
    y=c(3.5,1.6)
  )
  
  pitchTypes <- sort(unique(data2$PITCH_TYPE))
  updateRadioButtons(session, "pitchType", choices = pitchTypes)
    
  output$pitchLocationPlot <- renderPlot({
    ggplot(strikeZoneFrame, aes(x, y)) +
      geom_rect(xmin=-0.95,xmax=0.95,ymin=1.6,ymax=3.5,color="black",fill="green3", alpha=.4) +
      geom_point(data=data2 %>% filter(PITCHER_ID == input$Pitcher) %>% filter(PITCH_TYPE == input$pitchType),
                 aes(x=PITCH_LOCATION_SIDE,
                     y=PITCH_LOCATION_HEIGHT),
                 alpha=1,
                 color="black",
                 size=10,
                 shape="circle cross",
                 show.legend = FALSE) +
      xlim(-1.5,1.5) + ylim(0.5,4.5) +
      xlab("") + ylab("") +
      theme(plot.title = element_text(size=20, face="bold", margin = margin(10,0,10,0), hjust = .5),
            axis.ticks = element_blank(), axis.text = element_blank(),
            panel.background = element_rect(fill = 'gray95'),
            panel.grid = element_blank(),
            strip.background = element_rect(color="black", fill="salmon", linetype="blank"),
            strip.text = element_text(size=15, color="black", face="bold")) +
      ggtitle('Pitch Locations by Pitch Type')
  })
  
  data3 <- data2 %>%
    group_by(PITCHER_ID, PITCH_TYPE) %>%
    summarise(
      TOTAL_BALL = sum(PITCH_RESULT == "BallCalled", na.rm = TRUE) + sum(PITCH_RESULT == "HitByPitch", na.rm = TRUE),
      TOTAL_STRIKE = sum(PITCH_RESULT == "StrikeCalled", na.rm = TRUE) + sum(PITCH_RESULT == "StrikeSwinging", na.rm = TRUE) + sum(PITCH_RESULT == "FoulBall", na.rm = TRUE) + sum(PITCH_RESULT == "InPlay", na.rm = TRUE),
      TOTAL_PITCHED = TOTAL_BALL + TOTAL_STRIKE
    ) %>%
    mutate(
      TOTAL = sum(TOTAL_PITCHED)
    )
  
  output$pitchPercPlot <- renderPlot({
    ggplot(data3 %>% filter(PITCHER_ID == input$Pitcher), aes(x="", y=TOTAL_PITCHED, fill=PITCH_TYPE, label = paste(round(TOTAL_PITCHED / TOTAL * 100, 2), "%", sep=""))) +
      geom_bar(stat="identity", width=1) +
      theme_void() +
      coord_flip() +
      geom_text(size = 5, position = position_stack(vjust =0.5)) +
      ggtitle('Pitch Type Frequency') +
      theme(plot.title = element_text(size=20, face="bold", hjust=.06),
            legend.title = element_blank())
  })

}
