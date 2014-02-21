### This file will be updated periodically between now (whenever you are seeing this)
### and April 2nd (the day before the workshop!)
#####
### If you download this file now, please be sure to check back for updates
### as the conference approaches.


### TO NOTE: Anything preceded by at # is a 'comment'
#   that means it is something ignored by R
#   which means comments are usually something like this --
#   informative statements used for later reference, such as
#   reminding yourself what you did and why!


rm(list=ls()) #clean the workspace

load('clean.ps.data.rda')
load('clean.ps.design.rda')


##let's see what is in our workspace:
ls()

##Let's load up the packages we need:
#library(prettyGraphs)
#library(ExPosition)
#library(InPosition)

##But, if packages have dependences, they will load up together when we load the one with all the dependencies:
library(InPosition) ##InPosition depends on ExPosition which depends on prettyGraphs -- much easier!


##plain ol' PCA
##The age old question: to scale or not to scale?
####scaled
pca.ps.sim.fix <- epPCA(ps.sim.data,DESIGN=ps.sim.design,make_design_nominal=FALSE)

####not scaled
#pca.ps.sim.fix.noscale <- epPCA(ps.sim.data,scale=FALSE,DESIGN=ps.sim.design,make_design_nominal=FALSE)
	### to be discussed in detail during the workshop.


##let's explore output & hierarcy of the output
###This is the plotting data. Just a quick reference of where plotting parameters can be changed.
pca.ps.sim.fix
pca.ps.sim.fix$Plotting.Data
pca.ps.sim.fix$Plotting.Data$fi.col

###The good stuff -- all the PCA results.
pca.ps.sim.fix$ExPosition.Data

##And important specific things like:
###explained variance
pca.ps.sim.fix$ExPosition.Data$t
	##NOT necessarily eigenvalues -- to be discussed in detail at the workshop!
	###and we can visualize this relationship
prettyScree(pca.ps.sim.fix$ExPosition.Data$eigs)	


###factor scores (and normalized loadings)
pca.ps.sim.fix$ExPosition.Data$fi
pca.ps.sim.fix$ExPosition.Data$fj

###contributions to the variance
pca.ps.sim.fix$ExPosition.Data$ci
pca.ps.sim.fix$ExPosition.Data$cj

##interpretting at other components
epGraphs(pca.ps.sim.fix,x_axis=2,y_axis=3)



#### However, PCA in and of itself is just a descriptive model. There is no inference. 
#### Which means we don't know if the results are significant or meaningful yet.
###
#### And if we don't know what is significant... 
#### we may be interpretting null effects (as blasphemous sin in the eyes of Sir Ronald Fisher)!


##PCA with an inference battery -- all the permutation and bootstrap tests are performed at once.
pca.ps.sim.inf <- epPCA.inference.battery(ps.sim.data,DESIGN=ps.sim.design,make_design_nominal=FALSE,test.iters=100)
##It all looks the same -- just colored differently.
	##and with a few extra things written on the pictures.
	## to be discussed in detail at the workshop

##explore output & hierarchy
pca.ps.sim.inf
pca.ps.sim.inf$Fixed.Data # -- same as above!
						  ###whatever we understood before is still here!
						  ###see, I'll prove it!
pca.ps.sim.inf$Fixed.Data$ExPosition.Data$t # -- same thing as up above!						  


##But now we have something new
pca.ps.sim.inf$Inference.Data

###First, permutation. It tells us which components are significant.
pca.ps.sim.inf$Inference.Data$components
pca.ps.sim.inf$Inference.Data$components$p.vals

prettyScree(pca.ps.sim.inf$Fixed.Data$ExPosition.Data$eigs)	##so one component wasn't such a bad guess after all!

prettyScree(pca.ps.sim.inf$Fixed.Data$ExPosition.Data$eigs,
	n.comps=1,
	broken.stick=FALSE,
	kaiser=FALSE,
	perc.exp=pca.ps.sim.inf$Fixed.Data$ExPosition.Data$t[1]/100)	##but let's make it prettier and more accurate!
	
prettyScree(pca.ps.sim.inf$Fixed.Data$ExPosition.Data$eigs,
	n.comps=1,
	broken.stick=TRUE,
	kaiser=TRUE,
	perc.exp=pca.ps.sim.inf$Fixed.Data$ExPosition.Data$t[1]/100)	##prettier with several test criteria


### Now, the bootstrap 
### For fun: (see here for a great video interview by Efron on the bootstrap: http://www.youtube.com/watch?v=6l9V1sINzhE)

pca.ps.sim.inf$Inference.Data$fj.boots

##Bootstrap ratios and tests for Component 1
pca.ps.sim.inf$Inference.Data$fj.boots$tests$sig.boot.ratios[,1]
pca.ps.sim.inf$Inference.Data$fj.boots$tests$boot.ratios[,1]

##Bootstrap ratios and tests for Component 2
pca.ps.sim.inf$Inference.Data$fj.boots$tests$sig.boot.ratios[,2]
pca.ps.sim.inf$Inference.Data$fj.boots$tests$boot.ratios[,2]


###And now other components!
inGraphs(pca.ps.sim.inf,x_axis=2,y_axis=3)