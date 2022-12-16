# 14/12/2022
# By Jordi Rof

# Instructions:
# Open R
# Run this script, keep lists in the global environment
# Make sure mkd list object was properly stored in temporary directory
# Run Rmarkdown script 221214_grid_exercise.Rmd

# Install packages if not installed!
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("rmarkdown")
# install.packages("bookdown")

# Create all lists to store objects
prm <- list() # Parameters to control the exercise
spl <- list() # Supply
pdm <- list() # Demand
sim <- list() # Simulation
rmv <- list() # List with items to be removed
cda <- list() # Chart data
chr <- list() # ggplot chart storage

# Global parameters
prm$pr <- 24*50 # time periods (50 days, each period represents an hour)
prm$machine_storage_MWh <- 20 #MW/h
prm$machine_life_years <- 15 # years of life expectancy
prm$machine_total_cost <- 2000000 # 2 milion Euro
prm$machine_exercise_cost <- prm$machine_total_cost*((prm$pr/24)/(365*prm$machine_life_years)) # Assumption of 100 eur/kwh multiplied by amortization (50 days over 365*15)
prm$total_consumers <- 60000
prm$total_generators <- 4
prm$generator_cost_increase <- 20

# Supply frontier
spl$generators <- data.frame(matrix(NA,prm$total_generators,2))
names(spl$generators) <- c("Capacity","Cost")
spl$generators["Capacity"] <- 70
spl$generators["Cost"] <- seq(40,40+(prm$generator_cost_increase-1)*prm$total_generators,prm$generator_cost_increase)

# Demand profile of the market
# Daily pattern inspiration July energy demand https://www.eia.gov/todayinenergy/detail.php?id=42915
# Power average consumption https://www.eia.gov/tools/faqs/faq.php?id=97&t=3#:~:text=In%202021%2C%20the%20average%20annual,about%20886%20kWh%20per%20month.
pdm$daily_average_Mw <- c(0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.0,1.0,1.0,1.0,1.0,1.0)*600/30/12*0.001
names(pdm$daily_average_Mw) <- 0:23
pdm$MWh <- rep(pdm$daily_average_Mw*prm$total_consumers,prm$pr/24)
names(pdm$MWh) <- 1:length(pdm$MWh)

# Buying and selling opportunities
sim$opt <- list()
sim$opt$price <- sim$opt$supply <- sim$opt$sell_q <- sim$opt$buy_q <- pdm$MWh*NA
# Marginal price
sim$opt$price[pdm$MWh <= sum(spl$generators[1:3,"Capacity"])] <- spl$generators[3,"Cost"]
sim$opt$price[pdm$MWh <= sum(spl$generators[1:2,"Capacity"])] <- spl$generators[2,"Cost"]
sim$opt$price[pdm$MWh <= sum(spl$generators[1:1,"Capacity"])] <- spl$generators[1,"Cost"]
# Loop convenience
# This loops iterates not by period, but by market state depending of number of producers active
sim$opt$filter <-  matrix(NA,nrow(spl$generators),prm$pr)
sim$opt$supply[is.na(sim$opt$supply)] <- spl$generators$Capacity[1] #initial condition
sim$opt$sell_q <- pdm$MWh
for(i in 1:nrow(spl$generators)){
  # Create filter
  sim$opt$filter[i,] <- sim$opt$price == spl$generators$Cost[i]
  # Total available supply (without participation of the machine agents)
  sim$opt$supply[colSums(sim$opt$filter[1:i,,drop=F])==0] <- sim$opt$supply[colSums(sim$opt$filter[1:i,,drop=F])==0] + spl$generators["Capacity"][i,]
  # Selling frontier of possibility
  sim$opt$sell_q[colSums(sim$opt$filter[1:i,,drop=F])==0] <- sim$opt$sell_q[colSums(sim$opt$filter[1:i,,drop=F])==0] - spl$generators["Capacity"][i,]
}
# Energy purchase frontier of possibility
sim$opt$buy_q <-  sim$opt$supply - pdm$MWh
# Energy purchase 
sim$opt$decision <- matrix(0,prm$pr+1,12) #Periods +1 to record initial conditions
colnames(sim$opt$decision) <- c("Period","Supply","Demand","SellQ","BuyQ","Price","Decision","StorageFlow","StorageStock","MaxStorage","MoneyFlow","MoneyStock")
sim$opt$decision[,"Period"] <- 0:prm$pr
sim$opt$decision[-1,"Supply"] <- sim$opt$supply
sim$opt$decision[-1,"Demand"] <- pdm$MWh
sim$opt$decision[-1,"SellQ"] <- sim$opt$sell_q
sim$opt$decision[-1,"BuyQ"] <- sim$opt$buy_q
sim$opt$decision[-1,"Price"] <- sim$opt$price
# Store simulation results
sim$opt$simulartion_results

# Optimize the daily cycle
# Define all the possible sim$options of the parameters to be sim$optimized in the 
sim$vars <- expand.grid("SellP" = unique(sim$opt$price), "BuyP" = unique(sim$opt$price), "Machines" = 1:(sum(sim$opt$supply-pdm$MWh)/prm$machine_storage_MWh), "Revenue" = 0, "Profit" = 0)
sim$vars <- sim$vars[sim$vars[,"SellP"] > sim$vars[,"BuyP"],] # Exclude cases where sell price is lower or equal than buy price
sim$seq <- list()

for(n in 1:nrow(sim$vars)){
  # The loop across "n" ensures that we consider every combination of variables
  print(paste("Computing variable batch",n,"out of",nrow(sim$vars),"..."))
  sim$seq[[n]] <- sim$opt$decision
  sim$seq[[n]][,"MaxStorage"] <- prm$machine_storage_MWh*sim$vars[n,"Machines"]
  for(i in 2:nrow(sim$opt$decision)){
    # The loop acroos i ensures all the periods are taken into account
    # Compute decision
    if(sim$seq[[n]][i,"Price"] <= sim$vars[n,"BuyP"]){
      # Store decision type
      sim$seq[[n]][i,"Decision"] <- 1
      # Update storage
      sim$seq[[n]][i,"StorageStock"] <- min(sim$seq[[n]][i-1,"StorageStock"] + sim$seq[[n]][i,"BuyQ"], sim$seq[[n]][,"MaxStorage"])
      # Money outflow
      sim$seq[[n]][i,"MoneyFlow"] <- (sim$seq[[n]][i-1,"StorageStock"] - sim$seq[[n]][i,"StorageStock"])*sim$seq[[n]][i,"Price"]
    } else if (sim$seq[[1]][i,"Price"] >= sim$vars[n,"SellP"]) {
      # Store decision type
      sim$seq[[n]][i,"Decision"] <- -1
      # Update storage
      sim$seq[[n]][i,"StorageStock"] <- max(sim$seq[[n]][i-1,"StorageStock"] - sim$seq[[n]][i,"SellQ"], 0)
      # Money Inflow
      sim$seq[[n]][i,"MoneyFlow"] <- (sim$seq[[n]][i-1,"StorageStock"] - sim$seq[[n]][i,"StorageStock"])*sim$seq[[n]][i,"Price"]
    } else {
      # Store decision type
      sim$seq[[n]][i,"Decision"] <- 0
      # Update storage
      sim$seq[[n]][i,"StorageStock"] <- sim$seq[[n]][i-1,"StorageStock"]
    }
    # Update storage flow
    sim$seq[[n]][i,"StorageFlow"] <- sim$seq[[n]][i,"StorageStock"] - sim$seq[[n]][i-1,"StorageStock"]
    # Update MoneyStock
    sim$seq[[n]][i,"MoneyStock"] <- sim$seq[[n]][i-1,"MoneyStock"] + sim$seq[[n]][i,"MoneyFlow"]
  }
  # Store the results
  sim$vars[n,"Revenue"] <- sim$seq[[n]][i,"MoneyStock"]
  sim$vars[n,"Profit"] <- sim$vars[n,"Revenue"] - prm$machine_exercise_cost*sim$vars[n,"Machines"]
}

# Optimal strategy
sim$optimal_details <- sim$vars[sim$vars[,"Profit"]==max(sim$vars[,"Profit"]),]
sim$optimal_iter <- as.numeric(rownames(sim$optimal_details))

# Create nice theme for the plots
plotTheme <- function(text_size = 12) {
  ggplot2::theme_void() + ggplot2::theme(
    plot.title = ggplot2::element_text(color = R2::sty_palette("Light black", search_by = "name"), size = text_size + 8, face = "bold", hjust = 0.5, margin = ggplot2::margin(t = 0, r = 0, b = 10, l = 0)),
    axis.title.x.bottom = ggplot2::element_text(color = R2::sty_palette("Light black", search_by = "name"), size = text_size + 2, hjust = 0.5),
    axis.text.x = ggplot2::element_text(color = R2::sty_palette("Light black", search_by = "name"), size = text_size, hjust = 0.5, margin = ggplot2::margin(t = 0, r = 0, b = 10, l = 0)),
    axis.title.y.left = ggplot2::element_text(color = R2::sty_palette("Light black", search_by = "name"), size = text_size + 2, hjust = 0.5, angle = 90),
    axis.text.y = ggplot2::element_text(color = R2::sty_palette("Light black", search_by = "name"), size = text_size, hjust = 0.5, margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 10)),
    legend.text = ggplot2::element_text(size = text_size),
    plot.margin = ggplot2::unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.y = ggplot2::element_line(color = "grey"),
    legend.position = "top"
  )
}

# Plot the profile of the demand
cda$demand <- data.frame( "Pattern" = pdm$daily_average_Mw, "Hour" = 1:length(pdm$daily_average_Mw) )
chr$demand <- ggplot2::ggplot(data= cda$demand, ggplot2::aes(x=Hour, y=Pattern)) +
              ggplot2::geom_bar(stat="identity") +
              ggplot2::labs(title = "Consumption of a consumer in a day", x = "Hour", y = "MWh") +
              plotTheme()

# Plot the optimization results
rmv$S2B1 <- sim$vars[sim$vars[,"SellP"]==spl$generators[2,"Cost"],]
rmv$S2B1 <- rmv$S2B1[rmv$S2B1[,"BuyP"]==spl$generators[1,"Cost"],]
rmv$S3B1 <- sim$vars[sim$vars[,"SellP"]==spl$generators[3,"Cost"],]
rmv$S3B1 <- rmv$S3B1[rmv$S3B1[,"BuyP"]==spl$generators[1,"Cost"],]
rmv$S3B2 <- sim$vars[sim$vars[,"SellP"]==spl$generators[3,"Cost"],]
rmv$S3B2 <- rmv$S3B2[rmv$S3B2[,"BuyP"]==spl$generators[2,"Cost"],]
cda$optimization <- data.frame("Machines"=rmv$S2B1[,"Machines"],
                        "S2B1"=rmv$S2B1[,"Profit"]/1000,
                        "S3B1"=rmv$S3B1[,"Profit"]/1000,
                        "S3B2"=rmv$S3B2[,"Profit"]/1000)
rmv$optimization_labels <- c(paste("Sell ",spl$generators[2,"Cost"],", buy ",spl$generators[1,"Cost"],sep=""),
                           paste("Sell ",spl$generators[3,"Cost"],", buy ",spl$generators[1,"Cost"],sep=""),
                           paste("Sell ",spl$generators[3,"Cost"],", buy ",spl$generators[2,"Cost"],sep=""))

chr$optimization <- ggplot2::ggplot(data = cda$optimization[1:20,], ggplot2::aes(x = Machines, y = S2B1, group = 1)) +
                    ggplot2::geom_line(ggplot2::aes(y = S2B1, colour = "S2B1"), linetype = "solid", size = 1) +
                    ggplot2::geom_line(ggplot2::aes(y = S3B1, colour = "S3B1"), linetype = "solid", size = 1) +
                    ggplot2::geom_line(ggplot2::aes(y = S3B2, colour = "S3B2"), linetype = "solid", size = 1) +
                    ggplot2::labs(title = "Profit by strategy over machine number", x = "Machines", y = "Thousand euro") +
                    ggplot2::scale_colour_discrete(name = "Strategies:", labels = rmv$optimization_labels) +
                    plotTheme()

# Origin of energy demand
cda$energyf <- data.frame( "Hour" = sim$seq[[sim$optimal_iter]][,"Period"],
                          "Demand" = sim$seq[[sim$optimal_iter]][,"Demand"],
                          "Supply" = sim$seq[[sim$optimal_iter]][,"Supply"],
                          "StorageFlow" = sim$seq[[sim$optimal_iter]][,"StorageFlow"],
                          "Storage" = NA,
                          "ConsumpBattery" = NA,
                          "ConsumpGenerator" = NA)
cda$energyf["Storage"] <- cda$energyf["StorageFlow"]
cda$energyf["ConsumpBattery"] <- cda$energyf["StorageFlow"]*-1
cda$energyf[cda$energyf["Storage"] < 0,"Storage"] <- 0
cda$energyf[cda$energyf["ConsumpBattery"] < 0,"ConsumpBattery"] <- 0
cda$energyf["ConsumpGenerator"] <- data.frame(cda$energyf[,"Demand"] - cda$energyf[,"ConsumpBattery"])
cda$energy <- cda$energyf[1:(24*3+1) ,-c(2,3,4)]
cda$energy <- tidyr::gather(cda$energy, key = EnergyUse, value = MWh, c(Storage, ConsumpGenerator, ConsumpBattery), factor_key = T)

rmv$energy_labels <- c("Released by agents",
                       "Stored by agents",
                       "Generated & consumed")

chr$energy <- ggplot2::ggplot(cda$energy, ggplot2::aes(fill=factor(EnergyUse, levels=c("ConsumpBattery","Storage","ConsumpGenerator")), y=MWh, x=Hour)) + 
  ggplot2::geom_bar(position="stack", stat="identity") +
  ggplot2::labs(title = paste("Energy origin/use (optimal strategy)")) +
  ggplot2::scale_fill_discrete(name = "", labels = rmv$energy_labels) +
  plotTheme()

# Evolution of battery storage
cda$storage <- data.frame( "MWh" = sim$seq[[sim$optimal_iter]][,"StorageStock"], "Hour" = sim$seq[[sim$optimal_iter]][,"Period"])
chr$storage <- ggplot2::ggplot(data= cda$storage, ggplot2::aes(x=Hour, y=MWh)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::labs(title = "Storage capacity (optimal strategy)", x = "Hour", y = "MWh") +
  ggplot2::scale_x_continuous(breaks = seq(0, 1200, by = 200)) +
  plotTheme()

# Evolution of cash flow
cda$cashbalance <- data.frame( "Euro" = sim$seq[[sim$optimal_iter]][,"MoneyStock"]/1000, "Hour" = sim$seq[[sim$optimal_iter]][,"Period"])
chr$cashbalance <- ggplot2::ggplot(data= cda$cashbalance, ggplot2::aes(x=Hour, y=Euro)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::labs(title = "Cash balance (optimal strategy)", x = "Hour", y = "Thousand euro") +
  ggplot2::scale_x_continuous(breaks = seq(0, 1200, by = 200)) +
  plotTheme()

# Collect data for Markdown in a convenient way
mkd <- list()
mkd$chr <- chr
mkd$pdm <- pdm
mkd$spl <- spl
mkd$sim <- sim
mkd$prm <- prm

# Save important objects for Markdown in temporary directory
rmv$path <- tempdir()
rmv$pathsplit <- which(strsplit(rmv$path, "")[[1]] == "\\")
rmv$path <- strtrim(rmv$path, rmv$pathsplit[3])
rmv$path <- paste(rmv$path,"markdown_data.Rda",sep="")
save(mkd,file=rmv$path)