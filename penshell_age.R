###28 october penshell age structure###
##age structure model penshell (A.maura)
##setting working directory

#("C:/Users/lili_/Desktop/fishpath_r")

setwd("C:/Users/lili_/Desktop/fishpath_r")

#time 30 years
nyears<-30


#lengths<-as.matrix(read.csv("lengths_atrina.csv",header=FALSE, row.names = NULL))
#lengths_num<-as.numeric(lengths)
##convert lengths to ages using von bertanlafy equation
#ages=(-1/0.5)*log((1-lengths_num)/280)

# Age info
maxage <- 6 # based on lit for 27 cm, first report on the occurence of penshell a.pectinata (Sung Yang, 2015)
age_at_maturity <- 2 # age is 2 years for 12 cms according to VBF equation for size maturity is 12 cm
ages <- 1:maxage


###stock recruitment parameters###
#write stock recruitment relationship in terms of steepness, how to do it?
R0<-20000 # made up number for unfished recruitment
#fit beverton holt relationship

###age size parameters###
##found in literature, modelacion del crecimiento de de callo de hacha, noriega 2013##
linf <- 28 #cm #find out for penshell
k <- 0.5 #find out for penshell, growth rate found sealifebase for atrina pectinata
t0 <- 0 #use 0, as used in other studies when fitting VB curve

###length and weight parameters###
##found in literature for Atrina pinnata, using as an approximation
lw_a <- 2.6 #based on lit for a.pinnata  data
lw_b <- 2.44  #based on measurement data

# Calculate length/weight at age
length_at_age <- linf * (1-exp(-k*ages))
weight_at_age <- lw_a * length_at_age ^ lw_b
  

# Survival- and fecundity-at-age vectors
# Max age = 8, Age at maturity = 4, eggs= 9800000 
eggs_a <- 9800000 
s <- c(0.1, 0.1, 0.4, 0.6,0.8,0.8) # found in lit for a.maura 
fa <- c(0, 0, 0, 98000,9800000,9800000)  # based on egg data, fecundity at age
#s<- we have different survival rates for different ages/stages of penshell
#not sure how to include it.

R0<- 20000
n0age1 <-R0
n0allages <- c(n0age1, sapply(1:(maxage-1), function(x) n0age1*prod(s[1:x])))

S0<-sum(fa*n0allages)

h<-0.8 #steepness parameter 
alpha<-(1-h)/(4*h*R0)*S0
beta<- (5*h-1)/(4*h*R0)
beta
#eggs<-1 # potential fecundity, value found in literature for a.maura, analysis of different methods to estimate fecundity in bivalves (Caceres-Puig, 2016) 
#beverton<-Et/(alpha+beta*Et)


##calculate abundance for year 1###
#n0age1<-20000 # used value for R0
 # v<-0.1 #vulnerability for age 1 fish
#u<-0.1 #exploitation rate of age 1 fish
#n1a1<-(1-v*u)*s*R0 #not sure about this equation


# Create matrix to hold abundance over time
n <- matrix(data=NA, nrow=nyears, ncol=maxage)
n[1,] <- n0allages


# Create matrix to hold biomass over time
b <- matrix(data=NA, nrow=nyears, ncol=maxage)
b[1,] <- n0allages * weight_at_age

# Create matrix for eggs
eggs <- vector()

#eploitation rate vector
u<-rep(0.99,30)

#vulnerabilities at age
v<-c(0,1,1,1,1,1)
  
# Loop through time
# For testing: t <- 2
for(t in 2:(nyears)){
  
  # Number of eggs = fecundity times biomass
  eggs[t-1] <- sum(fa * n[t-1,])
  
  #Calculate number of recruits from SRR and # of eggs
  n[t,1] <- eggs[t-1]/(alpha+beta*eggs[t-1])


  # Calculate remaining age classes based on natural mortality
  #check this line
  n[t,2:maxage] <- n[t-1,1:(maxage-1)] * s[1:(maxage-1)]*(1-v[1:(maxage-1)]*u[t-1])
  
  # Convert abundance to biomass and save
  b[t,] <- n[t,] * weight_at_age
  
}

# PLOT RESULTS
##############################################

# Plot population size over time
plot(1:nyears, rowSums(n), type="l", las=1, bty="n", 
     xlim=c(0, nyears), ylim=c(0, max(rowSums(n), na.rm=T)),
     xlab="Year", ylab="N")

# Plot biomass over time
plot(1:nyears, rowSums(b)/sum(b[1,]), type="l", las=1, bty="n", 
     xlim=c(0, nyears), ylim=c(0, 1),
     xlab="Year", ylab="Biomass (kg)")

# Plot stock-recruitment relationship
plot(eggs/1000, n[2:nrow(n),1], type="p", las=1, bty="n", 
     xlab="Thousands of eggs", ylab="Recruits")








