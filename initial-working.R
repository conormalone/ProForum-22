library(tidyverse)
library(gganimate)
library(CodaBonito)
#import full games
parsed_1 <-fParseTrackingDataBothTeams('C:/SoccerData',1)
parsed_2 <-fParseTrackingDataBothTeams('C:/SoccerData',2)
#import EPV values from csv,adding colnames
epv_cols <-seq(from = 0,to = 120,length.out = 50)
#get home and away team values (reversing for away)
home_EPV <-read.csv("EPV.csv", col.names = epv_cols)
away_EPV <- rev(home_EPV)
#sense checks that players don't change half (they don't)
#first_half <- parsed_1$dtTrackingData %>% filter(Period ==1)
#second_half <- parsed_1$dtTrackingData %>% filter(Period ==2)
#first_half %>% group_by(Player, Tag) %>%  summarize(X=mean(X), Y = mean(Y)) %>% 
#ggplot( aes(X,Y,col=Tag))+geom_point()



#go through all events and get
#team, game, player, speed, value of change (PV x EPV), attack or def, time/frame

PV_list <-list()
for(i in 1:10){
  #subset tracking to just the "i"th frame
  this_frame_tracking <-parsed_1
  this_frame_tracking$dtTrackingData <- this_frame_tracking$dtTrackingData %>% dplyr::filter(Frame == i)
  #get all player PV for this frame 
  all_PV_output <- fGetPitchControlProbabilities (
    lData = this_frame_tracking,
    viTrackingFrame = i,
    iGridCellsX = 120 / 3
  )
  #get just active players in this frame
  this_frame_player_list <- this_frame_tracking$dtTrackingData %>% dplyr::filter(Tag !="B") %>% 
    dplyr::select("Player")
  #frame details that don't need to be looped
  this_frame <- i
  this_period <-this_frame_tracking$dtTrackingData$Period[1]
  #isolate frame to loop through players and get control for each one
  for(j in 1:nrow(this_frame_player_list)){
    #get player details
    this_player_event <- this_frame_tracking$dtTrackingData %>% dplyr::filter(Player == this_frame_player_list[j]$Player)
    this_player <-this_player_event %>%  dplyr::select("Player")
    this_team <-this_player_event %>% dplyr::select("Tag")
    this_velo <- this_player_event %>% dplyr::select("Velocity")
    #get pitch control without this_player
    #remove from data
    without_this_player_tracking <- this_frame_tracking
    without_this_player_tracking$dtTrackingData <- without_this_player_tracking$dtTrackingData %>% 
      dplyr::filter(Player != this_player$Player[1])
    #do PV calc
    without_this_player_PV <- fGetPitchControlProbabilities (
      lData = without_this_player_tracking,
      viTrackingFrame = i,
      iGridCellsX = 120 / 3
    )
    #get correct EPV
    if(this_team$Tag =="H"){this_EPV <- home_EPV 
    }   else 
      {this_EPV <- away_EPV
      }
    #get diff in PV
    #need to add EPV raster to calc
    added_value <- (matrix(all_PV_output$dtDetails$AttackProbability) - matrix(without_this_player_PV$dtDetails$AttackProbability)) 
    #append to list
    PV_list<- append(PV_list, added_value)
      }
 
}

lPitchControl$dtDetails$AttackProbability

