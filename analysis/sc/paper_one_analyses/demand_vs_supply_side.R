# paper analyses

# figure 2 related analyses about demand-side mitigation measures having disproportionate impacts on low-income populations

load("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/processed_and_binned_bill_data_plot3.Rdata")
params <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/parameter_list.csv")

# surcharges if we build no infrasturcture for high and low-income populations
outputfordate <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/0_outputs.csv")

# need: low income populations, no build, historical, long, intense, long-intense droughts
these_params <- params$X[which(params$mitigation_decision=="baseline"&params$build_decision=="none"&params$price_elasticity==0)]

# 0,66,132,198
outputs_bl <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/0_outputs.csv")
outputs_long <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/66_outputs.csv")
outputs_intense <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/132_outputs.csv")
outputs_long_intense <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/198_outputs.csv")

# fixed charges come from outputs
max(unique(outputs_bl$fixedCharge))-10.99

max(unique(outputs_intense$fixedCharge)) -10.99

# calculate the net reductions in bills 
bills_bl <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/0_hh_bills.csv")
bills_long <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/66_hh_bills.csv")
bills_intense <-read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/132_hh_bills.csv")
bills_long_intense <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/198_hh_bills.csv")

# calculate for low and high-income households
#LI: 7500,12500,17500
#HI: 137500, 175000,250000

# make a plot to make sure we know what's going on
#plot(bills$X12500) # this is the resulting change
#lines(bills$raw_12500,col="red") # raw is "imagine there wasn't a drought"
#lines(bills$rateonly_12500,col="blue") # rateonly is same demand, increased rate
#lines(bills$demandonly_12500,col="green") # demandonly is reduced demand, old rate

# bill reduction for LI populations in baseline drought
# this is bills$raw_INCOME - bilsl$demandonly_INCOME
bl_li_raw <- rowMeans(cbind(bills_bl$raw_7500,bills_bl$raw_12500,bills_bl$raw_17500))
bl_li_demandonly <- rowMeans(cbind(bills_bl$demandonly_7500,bills_bl$demandonly_12500,bills_bl$demandonly_17500))

# calculate mean increase for times there was an increase
bl_li_decrease <- bl_li_raw-bl_li_demandonly
mean(bl_li_decrease[bl_li_decrease!=0])

# same for intense....
intense_li_raw <- rowMeans(cbind(bills_intense$raw_7500,bills_intense$raw_12500,bills_intense$raw_17500))
intense_li_demandonly <- rowMeans(cbind(bills_intense$demandonly_7500,bills_intense$demandonly_12500,bills_intense$demandonly_17500))

# calculate mean increase for times there was an increase
intense_li_decrease <- intense_li_raw-intense_li_demandonly
mean(intense_li_decrease[intense_li_decrease!=0])


# same for HI populations
bl_hi_raw <- rowMeans(cbind(bills_bl$raw_137500,bills_bl$raw_175000,bills_bl$raw_250000))
bl_hi_demandonly <- rowMeans(cbind(bills_bl$demandonly_137500,bills_bl$demandonly_175000,bills_bl$demandonly_250000))

# calculate mean increase for times there was an increase
bl_hi_decrease <- bl_hi_raw-bl_hi_demandonly
mean(bl_hi_decrease[bl_hi_decrease!=0])

# same for intense....
intense_hi_raw <- rowMeans(cbind(bills_intense$raw_137500,bills_intense$raw_175000,bills_intense$raw_250000))
intense_hi_demandonly <- rowMeans(cbind(bills_intense$demandonly_137500,bills_intense$demandonly_175000,bills_intense$demandonly_250000))

# calculate mean increase for times there was an increase
intense_hi_decrease <- intense_hi_raw-intense_hi_demandonly
mean(intense_hi_decrease[intense_hi_decrease!=0])

# net bill increase/decresae 
bl_li_result <- rowMeans(cbind(bills_bl$X7500,bills_bl$X12500,bills_bl$X17500))-bl_li_raw
mean(bl_li_result[bl_li_result!=0])

intense_li_result <- rowMeans(cbind(bills_intense$X7500,bills_intense$X12500,bills_intense$X17500))-intense_li_raw
mean(intense_li_result[intense_li_result!=0])

bl_hi_result <- rowMeans(cbind(bills_bl$X137500,bills_bl$X175000,bills_bl$X250000)) - bl_hi_raw
mean(bl_hi_result[bl_hi_result!=0])

intense_hi_result <- rowMeans(cbind(bills_intense$X137500,bills_intense$X175000,bills_intense$X250000))-intense_hi_raw
mean(intense_hi_result[intense_hi_result!=0])

# supply-side infrastructure costs
# this is just increased in fixed charges for each drought as they are all the same

these_params <- params$X[which(params$mitigation_decision!="baseline"&params$build_decision=="none"&params$price_elasticity==0)]
# x=33,99,165
outputs_bl <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/33_outputs.csv")
outputs_long <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/99_outputs.csv")
outputs_intense <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/165_outputs.csv")

# now get the increases
bl_increases <- outputs_bl$fixedCharge-10.99
long_increases <- outputs_long$fixedCharge - 10.99
intense_increases <- outputs_intense$fixedCharge-10.99

summary(bl_increases[bl_increases>0])
summary(long_increases[long_increases>0])
summary(intense_increases[intense_increases>0])

# infrastructure interaction: that is the increase from baseline to intense for none vs low-capacity

#baseline to intense (none): 
these_params <- params$X[which(params$build_decision%in%c("none","grrp-r")&params$price_elasticity==0&params$drought_characteristic%in%c("baseline.csv","intense.csv")&params$mitigation_decision=="market")]
#x = 0,33,123,165
# calculate average increase for LI populations between 0 and 33 
outputs_0 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/0_outputs.csv")
outputs_33 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/33_outputs.csv")
outputs_132 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/132_outputs.csv")
outputs_165 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/165_outputs.csv")

outputs_22 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/22_outputs.csv")
outputs_154 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/154_outputs.csv")

# 0 to 132 (basleine to intense, baselien mitigation and no build)
outputs$deep_poverty_total_avg[133] - outputs$deep_poverty_total_avg[1]

# baseline to intense (build g-rrp, low capacity)
outputs$deep_poverty_total_avg[155] - outputs$deep_poverty_total_avg[23]


# similarly let's do this for the supply-side case (build nothing increase)
outputs$deep_poverty_total_avg[166] - outputs$deep_poverty_total_avg[34]

# baseline to intense (build g-rrp, low capacity)
outputs$deep_poverty_total_avg[188] - outputs$deep_poverty_total_avg[56]


# FIGURING OUT WHAT/WHY
#slide 8 of weekly report 11/08/21
# x = 0,22,1
# baseline, build low, curtailment x = 22
# intense, build none, curtailment x = 132
# intense, build low, curtailment x = 155
outputs_155 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/155_outputs.csv")

# so fixed charge is way lower for 22 (baseline, low) which makes sense
unique(outputs_22$fixedCharge)
unique(outputs_132$fixedCharge)
unique(outputs_155$fixedCharge)

# so the third one raises rates for the baseline but lowers them during curtailment

unique(outputs_22$conserveStatus)
unique(outputs_132$conserveStatus)
unique(outputs_155$conserveStatus)

# one hypothesis: building raises rates during non-drought but lowers them during drought. 
# no build: lower during non-drought, higher during drought. 
# In the intense drought, building 


# let's just pull low-income bills for scenarios 22,132,155 as a first step today.
bills_22 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/22_hh_bills.csv")
bills_0 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/0_hh_bills.csv")
bills_132 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/132_hh_bills.csv")
bills_155 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/155_hh_bills.csv")

use_22 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/22_hh_demand.csv")
use_132 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/132_hh_demand.csv")
use_155 <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/outputs/santa_cruz/experiments/plot3/155_hh_demand.csv")

pp_bills <- data.frame(cbind(bills_22$X17500,bills_132$X17500,bills_155$X17500))
plot(pp_bills[,1],type="l",col="black",ylim=c(43,79))
lines(pp_bills[,2],type="l",col="blue")
lines(pp_bills[,3],type="l",col="red")

# same thing but show the increases and decreases
plot(bills_132$demandonly_17500,type="l",col="red",ylim=c(30,100))
lines(bills_132$rateonly_17500,type="l",col="red")
lines(bills_155$demandonly_17500,type="l",col="blue")
lines(bills_155$rateonly_17500,type="l",col="blue")

#pp_demand <- data.frame(cbind(use_22$X17500,use_132$X17500,use_155$X17500))
#plot(pp_demand[,1],type="l",col="black",ylim=c(0,4120))
#lines(pp_demand[,2],type="l",col="blue")
#lines(pp_demand[,3],type="l",col="red")
# thought: the difference between red and blue will grow with higher elasticity 
# try the same plot above for for PED = like 0.5

library("scales")
# same thing (increases decreases) but for baseline drought
LWD = 2
plot(bills_22$demandonly_17500,type="l",col="blue",ylim=c(0,100),xlim=c(42,85),main="baseline drought",lty=2,ylab="Monthly Water Bill, Low-income",lwd=LWD)
lines(bills_22$rateonly_17500,type="l",col="blue",lty=1,lwd=LWD)
lines(bills_0$demandonly_17500,type="l",col="red",lty=2,lwd=LWD)
lines(bills_0$rateonly_17500,type="l",col="red",lwd=LWD)
legend(50,15,legend=c("Increase from rate increase (build nothing)", "Decrease from curtailment (build nothing)",
                      "increase from rate increase (build low-cap)",
                      "decrease from curtailment (build low-cap"), col=c("red","red","blue","blue"),lty=c(1,2,1,1),lwd=LWD)


LWD = 2
plot(bills_155$demandonly_17500,type="l",col="blue",ylim=c(0,100),xlim=c(42,85),main="intense drought",lty=2,ylab="Monthly Water Bill, Low-income",lwd=LWD)
lines(bills_155$X17500,type="l",col=alpha(rgb(1,0,0),0.5),lwd=LWD)#,alpha=0.5)
lines(bills_155$rateonly_17500,type="l",col="blue",lty=1,lwd=LWD)
lines(bills_132$demandonly_17500,type="l",col="red",lty=2,lwd=LWD)
lines(bills_132$rateonly_17500,type="l",col="red",lwd=LWD)
lines(bills_132$X17500,type="l",col=alpha(rgb(0,0,1),0.5),lwd=LWD)
legend(50,15,legend=c("Increase from rate increase (build nothing)", "Decrease from curtailment (build nothing)",
                      "increase from rate increase (build low-cap)",
                      "decrease from curtailment (build low-cap"), col=c("red","red","blue","blue"),lty=c(1,2,1,1),lwd=LWD)

WD = 2
plot(bills_155$demandonly_175000,type="l",col="blue",ylim=c(0,200),xlim=c(42,85),main="intense drought",lty=2,ylab="Monthly Water Bill, High-income",lwd=LWD)
lines(bills_155$rateonly_175000,type="l",col="blue",lty=1,lwd=LWD)
lines(bills_132$demandonly_175000,type="l",col="red",lty=2,lwd=LWD)
lines(bills_132$rateonly_175000,type="l",col="red",lwd=LWD)
legend(50,25,legend=c("Increase from rate increase (build nothing)", "Decrease from curtailment (build nothing)",
                      "increase from rate increase (build low-cap)",
                      "decrease from curtailment (build low-cap"), col=c("red","red","blue","blue"),lty=c(1,2,1,1),lwd=LWD)


# same for maximums 
# 132 is intense, curtail, build nothing
# 155 is intense, curtail, build low-capacity

summary(bills_132$X17500)
summary(bills_155$X17500)

summary(bills_155$X17500-bills_132$X17500)
