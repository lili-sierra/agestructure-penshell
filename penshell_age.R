###9 september penshell age structure###
##age structure model penshell (A.maura)
##setting working directory

#("C:/Users/lili_/Desktop/fishpath_r")

#time 100 years
nyears<-100

### age information ###
maxage<-10 #made up number
ages<-1:maxage

###stock recruitment parameters###
#write stock recruitment relationship in terms of steepness, how to do it?
R0<-20000 # made up number for unfished recruitment
#fit beverton holt relationship

#s<- we have different survival rates for different ages/stages of penshell
#not sure how to include it.
#SPR0<- spawners per recruit R0
SPR0<-fa*Na
fa<- fecundity at age a
#Na<-abundance at age a
S0<-SPR0*R0

h<- steepness parameter 
alpha<-(1-h/4h*R0)*S0
beta<- 5h-1/4*h*R0
eggs<-find amount for penshell
beverton<-Et/(alpha+beta*Et)


###survival and fecundity at age vectors###
# Survival- and fecundity-at-age vectors
# Max age = 10, Age at maturity = find out for penshell, eggs per gram =find out for penshell
eggs_per_gram <-  #find out for penshell
  survival<-c()#different values for survival at different ages
#until we reach maximum age
fecundity<-c()

###age size parameters###
linf <- #find out for penshell
  k <- #find out for penshell
  t0 <- #use 0?
  
  
  
  ###length and weight parameters###
  lw_a <-  #based on measurement data
  lw_b <-  #based on measurement data
  
  # Calculate length/weight at age
  length_at_age <- linf * (1-exp(-k*ages))
weight_at_age <- lw_a * length_at_age ^ lw_b


##calculate abundance for year 1###
n0age1<- # use R0?
  v<-vulnerability for age 1 fish
u<-exploitation rate of age 1 fish
n1a1<-(1-v*u)*s*R0 #not sure about this equation
n0_all_ages<-how do we get this?
  # how do we apply equation to all ages?
  #assume that exploitation rate and vulnerability are the same?
  
  
  ##getting biomass##
  biomass_all_ages<-n0_all_ages * weight_at_age


##run model through time##
##not sure about this equation##
# Loop through time
# For testing: t <- 2; a <- 1
for(t in 2:(nyears)){
  Eggs[t]=sum(f * n[t-1,])
  n[t,1] <- (Eggs[t]*alpha)/(beta+Eggs[t])
  n[t,2:maxage] <- n[t-1,1:(maxage-1)] * s[1:maxage-1]
  # Convert abundance to biomass and save
  b[t,] <- n[t,] * weight.at.age
  
}













