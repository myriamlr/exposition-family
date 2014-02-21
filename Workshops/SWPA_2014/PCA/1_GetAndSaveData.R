##Step 1: Let's get the data in and save it in R's format.
##This is good practice for analysis pipelines.


##Which directory are we in?
getwd()
	##Do we want to change our directory? Usually, this is a good idea
	# ?setwd() will help us!

##let's see if we have anything in the workspace:

ls()

##let's clean the workspace

rm(list=ls()) #this gets rid of _everything_.


	##let's discuss what each flag here means.
#ps.sim.data <- read.csv('ps.sim.data.csv',header=TRUE,row.names=1)
#ps.sim.design <- read.csv('ps.sim.design.csv',header=TRUE,row.names=1)

ps.sim.data <- read.csv("http://exposition-family.googlecode.com/svn/Workshops/SWPA_2014/PCA/ps.sim.data.csv",header=TRUE,row.names=1)
ps.sim.design <- read.csv("http://exposition-family.googlecode.com/svn/Workshops/SWPA_2014/PCA/ps.sim.data.csv",header=TRUE,row.names=1)

##it's often good practice to save the data in a natural R format
##Typically, after you've cleaned the data and it's ready to go!

save(ps.sim.data,file='clean.ps.data.rda')
save(ps.sim.design,file='clean.ps.design.rda')