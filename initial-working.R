library(tidyverse)
library(gganimate)
library(CodaBonito)
library(raster)
#import full games
parsed_1 <-fParseTrackingDataBothTeams('C:/SoccerData',1)
parsed_2 <-fParseTrackingDataBothTeams('C:/SoccerData',2)

#################################
#EPV import and rastering
#import EPV values from csv,adding colnames
#epv_cols <-seq(from = 0,to = 120,length.out = 50)
#get home and away team values (reversing for away)
home_EPV <-read.csv("EPV.csv", header=F)
away_EPV <- rev(home_EPV)

rasterisedhome <-data.frame(matrix(,nrow = 32*50,ncol = 3))
rasterisedaway <-data.frame(matrix(,nrow = 32*50,ncol = 3))
z1 <-1
propercols <-seq(from = -58.5, to = 58.5, length.out = 50)
properrows <-seq(from = -38.51852, to = 38.51852, length.out = 32)
#create raster of start data
for(i in 1:nrow(home_EPV) ){
  for(j in 1:ncol(home_EPV)){
    addtoraster <-c(home_EPV[i,j], propercols[j] , properrows[i]) 
    rasterisedhome[z1,] <-addtoraster
    z1<-z1+1
  }}
rasterisedhome <- rasterisedhome[,c(3,2,1)]
colnames(rasterisedhome)<-c("y", "x","values")
EPV_raster_home <- raster::rasterFromXYZ(rasterisedhome)
z1 <-1
for(i in 1:nrow(away_EPV) ){
  for(j in 1:ncol(away_EPV)){
    addtoraster <-c(away_EPV[i,j], propercols[j] , properrows[i]) 
    rasterisedaway[z1,] <-addtoraster
    z1<-z1+1
  }}
rasterisedaway <- rasterisedaway[,c(3,2,1)]
colnames(rasterisedaway)<-c("y", "x","values")
EPV_raster_away <- raster::rasterFromXYZ(rasterisedaway)
#EPV Finished
#############################################
#sense checks that players don't change half (they don't)
#first_half <- parsed_1$dtTrackingData %>% filter(Period ==1)
#second_half <- parsed_1$dtTrackingData %>% filter(Period ==2)
#first_half %>% group_by(Player, Tag) %>%  summarize(X=mean(X), Y = mean(Y)) %>% 
#ggplot( aes(X,Y,col=Tag))+geom_point()

#put every pass' start and end frame in a list so the player in poss isn't assessed
#(if you have the ball it's assumed you're moving efficiently and you aren't second guessed)
player_in_poss_df <- parsed_1$dtEventsData %>% dplyr::filter(Type == "PASS") %>% 
  dplyr::select(c("StartFrame", "EndFrame", "From"))
player_in_poss_df$player_in_poss_seq <- apply(player_in_poss_df,1, function(x) seq(x["StartFrame"], x["EndFrame"]))
player_in_poss_list <- player_in_poss_df %>% 
  unnest(player_in_poss_seq) %>% 
  dplyr::select("player_in_poss_seq", "From")  %>%  
  distinct(player_in_poss_seq, .keep_all = TRUE)

#go through all events and get
#team, game, player, speed, value of change (PV x EPV), attack or def, time/frame

PV_list <-data.frame(matrix(,,ncol = 7))
colnames(PV_list) <-c("this_frame", "this_period", "Player", "Tag", "Velocity", "Value_Added", "attacking_team")
#set Frame as factor
frame_levels <-levels(as.factor(parsed_1$dtTrackingData$Frame))
for(i in 1:length(frame_levels)){
  this_frame <- frame_levels[i]
  #subset tracking to just the "i"th frame
  this_frame_tracking <-parsed_1
  this_frame_tracking$dtTrackingData <- this_frame_tracking$dtTrackingData %>% dplyr::filter(Frame == this_frame)
  #get all player PV for this frame 
  all_PV_output <- fGetPitchControlProbabilities (
    lData = this_frame_tracking,
    viTrackingFrame = as.integer(this_frame),
    iGridCellsX = 120 / 3
  )
  #put this PV into raster format
  all_PV_raster_format <-all_PV_output$dtDetails %>% 
    dplyr::mutate(y = TargetY, x = TargetX, values = AttackProbability)%>%
    dplyr::select(c("y", "x", "values")) 
  all_PV_raster <- raster::rasterFromXYZ(all_PV_raster_format)
  #isolate the player in possession this frame
  this_player_in_poss <- player_in_poss_list %>% dplyr::filter(player_in_poss_seq == this_frame) %>% dplyr::select("From")
  this_player_in_poss <- ifelse(nrow(this_player_in_poss)==0, 100,this_player_in_poss)
  #get just active players in this frame (leave out ball and player in possession)
  this_frame_player_list <- this_frame_tracking$dtTrackingData %>% dplyr::filter(Tag !="B" && Player != this_player_in_poss$From) %>% 
    dplyr::select("Player")
  #frame details that don't need to be looped
  this_frame <- this_frame
  this_period <-this_frame_tracking$dtTrackingData$Period[1]
  attacking_team <- all_PV_output$dtTrackingSlice$AttackingTeam[1]
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
      viTrackingFrame = as.integer(this_frame),
      iGridCellsX = 120 / 3
    )
    #put PV in raster format then rasterize
    without_this_player_PV_raster_format <-without_this_player_PV$dtDetail %>% 
      dplyr::mutate(y = TargetY, x = TargetX, values = AttackProbability)%>%
      dplyr::select(c("y", "x", "values")) 
    
    without_this_player_PV_raster <- raster::rasterFromXYZ(without_this_player_PV_raster_format)
    
    #get correct EPV
    if(attacking_team =="H"){this_EPV <- EPV_raster_home
    }   else 
      {this_EPV <- EPV_raster_away
      }
    #resample this_EPV raster to get it on same extent as PV rasters
    this_EPV <- raster::resample(this_EPV,all_PV_raster)
    #get diff in PV
    #need to add EPV raster to calc
    PV_with <- raster::overlay(all_PV_raster, this_EPV, fun=function(x,y){return(x*y)})
    PV_without <-raster::overlay(without_this_player_PV_raster, this_EPV, fun=function(x,y){return(x*y)})
    #subtract sum of PVxEPV_without raster from PVxEPV_with raster
    Value_Added<- raster::cellStats(PV_with,sum) - raster::cellStats(PV_without,sum)
    to_add <- cbind(this_frame, this_period, this_player, this_team, this_velo, Value_Added,attacking_team)
    #append to list
    PV_list<- rbind(PV_list, to_add)
      }
 
}

