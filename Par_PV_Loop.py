# -*- coding: utf-8 -*-
import Metrica_IO as mio
import Metrica_Viz as mviz
import Metrica_Velocities as mvel
import myversion_Metrica_PitchControl as mpc
import numpy as np
from joblib import Parallel, delayed
import multiprocessing

# set up initial path to data
DATADIR = "D:/SoccerData/StatsPerform/ProForum22"
game_id = 1 # let's look at sample match 2

# read in the event data
events = mio.read_event_data(DATADIR,game_id)

# read in tracking data
tracking_home = mio.tracking_data(DATADIR,game_id,'Home')
tracking_away = mio.tracking_data(DATADIR,game_id,'Away')

# Convert positions from metrica units to meters (note change in Metrica's coordinate system since the last lesson)
tracking_home = mio.to_metric_coordinates(tracking_home)
tracking_away = mio.to_metric_coordinates(tracking_away)
events = mio.to_metric_coordinates(events)

# reverse direction of play in the second half so that home team is always attacking from right->left
tracking_home,tracking_away,events = mio.to_single_playing_direction(tracking_home,tracking_away,events)

# Calculate player velocities
#tracking_home = mvel.calc_player_velocities(tracking_home,smoothing=True)
#tracking_away = mvel.calc_player_velocities(tracking_away,smoothing=True)
# **** NOTE *****
# if the lines above produce an error (happens for one version of numpy) change them to the lines below:
# ***************
tracking_home = mvel.calc_player_velocities(tracking_home,smoothing=True,filter_='moving_average')
tracking_away = mvel.calc_player_velocities(tracking_away,smoothing=True,filter_='moving_average')

""" **** pitch control for passes leading up to goal 2 **** """
# first get model parameters
params = mpc.default_model_params()

# evaluated pitch control surface for first pass
PPCF,xgrid,ygrid = mpc.generate_pitch_control_for_event(2, events, tracking_home, tracking_away, params, field_dimen = (106.,68.,), n_grid_cells_x = 50)




#def eventiteration(the_limit):
    #time.sleep(5)
#    i = 1
#    while i < the_limit:
#        PPCF,xgrid,ygrid = mpc.generate_pitch_control_for_event(i, events, tracking_home, tracking_away, params, field_dimen = (106.,68.,), n_grid_cells_x = 50)
#        mylist.append(PPCF)
#        i+=1
#    return mylist

#result = eventiteration(146007)


mylist = []
inputs = range(1,71268)
def parPV_loop(i):
    try:
        PPCF,xgrid,ygrid = mpc.generate_pitch_control_for_event(i, events, tracking_home, tracking_away, params, field_dimen = (106.,68.,), n_grid_cells_x = 50)
        np.savetxt(str(i)+'.csv', PPCF, delimiter=',')
    except AssertionError:
        pass
    #mylist.insert(i,PPCF)
    return mylist
num_cores = multiprocessing.cpu_count()
results = Parallel(n_jobs=num_cores)(delayed(parPV_loop)(i) for i in inputs)
