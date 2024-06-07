## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
taf.library(FLCore)
taf.library(stockassessment)
taf.library(FLSAM)

mkdir("data")

### ============================================================================
### Read the input data
### ============================================================================

read.ices.taf <- function(...) {
  read.ices(taf.data.path(...))
}

#  ## Catch-numbers-at-age ##
catage <- read.ices.taf("cn.dat")

#  ## Catch-weight-at-age ##
wcatch <- read.ices.taf("cw.dat")
wdiscards <- read.ices.taf("dw.dat")
wlandings <- read.ices.taf("lw.dat")

#  ## Natural-mortality ##
natmort <- read.ices.taf("nm.dat")

# maturity
maturity <- read.ices.taf("mo.dat")

#  ## Proportion of F before spawning ##
propf <- read.ices.taf("pf.dat")

#  ## Proportion of M before spawning ##
propm <- read.ices.taf("pm.dat")

#  ## Stock-weight-at-age ##
wstock <- read.ices.taf("sw.dat")

# Landing fraction in catch at age
landfrac <- read.ices.taf("lf.dat")

# survey data
surveys <- read.ices.taf("survey.dat")


### ============================================================================
###- Setup FLStock objects with data in there
### ============================================================================
dmns 							<- FLQuant(NA,dimnames=list(age=colnames(catage),year=rownames(catage),unit="unique",season="all",area="unique",iter=1))
ARG								<- FLStock(m=dmns)
ARG@catch.n[] 		<- t(catage)
ARG@landings.n[] 	<- t(catage) 
ARG@discards.n[] 	<- 0
ARG@catch.wt[] 		<- t(wcatch)
ARG@discards.wt[] <- t(wdiscards)
ARG@landings.wt[] <- t(wlandings)
ARG@mat[] 				<- t(maturity)
ARG@m[] 					<- t(natmort)
ARG@harvest.spwn[]<- t(propf)
ARG@m.spwn[] 			<- t(propm)
ARG@stock.wt[] 		<- t(wstock) 
ARG@catch  				<- computeCatch(ARG)
ARG@landings  		<- computeLandings(ARG)
ARG@discards  		<- computeDiscards(ARG)
ARG@catch.n[ARG@catch.n==-1] <- NA
FLIndices(catch=FLIndex(index=ARG@catch.n))

units(ARG)[1:17]    <- as.list(c(rep(c("tonnes","thousands","kg"),4), 
                                   rep("NA",2),"f",rep("NA",2)))
  
#Set object details
ARG@name                              <- paste0("Argentine 27.5b6a_WGDEEP_",range(ARG)["maxyear"]+1) 
range(ARG)[c("minfbar","maxfbar")]    <- c(6,14)


#- Setup FLIndices
ARG.tun <- FLIndices()
for(i in names(surveys)){
	if(colnames(surveys[[i]])[[1]]==-1) {
		ageDims <- "all"
	} else {
		ageDims <- colnames(surveys[[i]])
	}
	ARG.tun[[i]] <- FLIndex(index=FLQuant(NA,dimnames=list(
												age=ageDims,
												year=rownames(surveys[[i]]),
												unit="unique",
												season="all",
												area="unique",
												iter=1)))
	ARG.tun[[i]]@index[]<- t(surveys[[i]])
	if(ageDims[1]=="all"){
		type(ARG.tun[[i]]) 	<- "biomass"
	} else {
		type(ARG.tun[[i]]) <- "number"
	}
	ARG.tun[[i]]@name	<- i
	range(ARG.tun[[i]])[c("startf","endf")] <- attr(surveys[[i]],"time")
}

### ============================================================================
###- Save the input data
### ============================================================================

save(ARG,ARG.tun,file="data/data.RData")