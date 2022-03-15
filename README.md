# ProForum-22
Calculating the efficiency of running by valuing the territory gained as (Pitch Control x Possession Value) and dividing by energy expended (in this case speed) on Metrica Soccer tracking data. 

This was my paper submission to the 2022 StatsPerform ProForum to the AS Monaco Proposal:
"Identifying and evaluating the efficiency of runs made by a team’s players, to distinguish proactive running from reactive running"


Have everything working, unfortunately the Pitch Control function is extremely slow, took about 7 seconds in R to get 1 game, got that down to 3 secs by parallelizing, then to 1 sec by parallelizing in Python but that's still about 20 hours to get PC for 1 game, without looking at individual player contributions. 
I'll come back to this in the future if I think of a way to make it more efficient or I get a fire extinguisher and just let it run and hope.
