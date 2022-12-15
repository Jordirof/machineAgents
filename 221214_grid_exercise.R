# 13/12/2022
# By Jordi Rof

# Instructions:
# Open R
# Run this script, keep lists in the global environment
# Run script XXX

# Install packages if not installed!
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("rmarkdown")
# install.packages("bookdown")

# Create all lists to store objects
prm <- list() # Parameters to control the exercise
spl <- list() # Supply
pdm <- list() # Demand
opt <- list() # Optimization exercise
sim <- list() # Simulation
rmv <- list() # List with items to be removed
cda <- list() # Chart data
chr <- list() # ggplot chart storage

# Global parameters
prm$seed <- 123 # number controlling random components for exercise replicability
prm$pr <- 24*50 # time periods (50 days, each period represents an hour)
prm$transformer_efficiency <- 0.98
prm$dimnames_wind <- list(c("s1","s2","s3"),1:prm$pr)
prm$machine_storage <- 20 #Kwmh
prm$machine_cost <- 500 #Kwmh

# Supply frontier
spl$p4 <- spl$p3 <- spl$p2 <- spl$p1 <- list()
spl$p1$power_Mw <- spl$p2$power_Mw <- spl$p3$power_Mw <- 70
spl$p1$power_p <- 40 #2
spl$p2$power_p <- 60 #4
spl$p3$power_p <- 80 #6
spl$p4$power_p <- 100 #8

# Demand profile of the market
# Daily pattern inspiration July energy demand https://www.eia.gov/todayinenergy/detail.php?id=42915
# Power average consumption https://www.eia.gov/tools/faqs/faq.php?id=97&t=3#:~:text=In%202021%2C%20the%20average%20annual,about%20886%20kWh%20per%20month.
pdm$daily_average_Mw <- c(0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.0,1.0,1.0,1.0,1.0,1.0)*600/30/12*0.001
names(pdm$daily_average_Mw) <- 0:23
pdm$total_consumers <- 60000
pdm$MWh <- rep(pdm$daily_average_Mw*pdm$total_consumers,prm$pr/24)
names(pdm$MWh) <- 1:length(pdm$MWh)

# Buying and selling opportunities
opt$price <- opt$supply <- opt$sell_q <- opt$buy_q <- pdm$MWh*NA
# Marginal price
opt$price[pdm$MWh <= (spl$p1$power_Mw + spl$p2$power_Mw + spl$p2$power_Mw)] <- spl$p3$power_p
opt$price[pdm$MWh <= (spl$p1$power_Mw + spl$p2$power_Mw)] <- spl$p2$power_p
opt$price[pdm$MWh <= spl$p1$power_Mw] <- spl$p1$power_p
# Loop convenience
# This loops iterates not by period, but by market state depending of number of producers active
opt$filter <-  matrix(NA,length(spl),prm$pr)
opt$supply[is.na(opt$supply)] <-  spl[[1]]$power_Mw #initial condition
opt$sell_q <- pdm$MWh
for(i in 1:length(spl)){
  # Create filter
  opt$filter[i,] <- opt$price == spl[[i]]$power_p
  # Total available supply (without participation of the machine agents)
  opt$supply[colSums(opt$filter[1:i,,drop=F])==0] <- opt$supply[colSums(opt$filter[1:i,,drop=F])==0] + spl[[min(i+1,length(spl))]]$power_Mw
  # Selling frontier of possibility
  opt$sell_q[colSums(opt$filter[1:i,,drop=F])==0] <- opt$sell_q[colSums(opt$filter[1:i,,drop=F])==0] - spl[[min(i+1,length(spl))]]$power_Mw
}
# Energy purchase frontier of possibility
opt$buy_q <-  opt$supply - pdm$MWh
# Energy purchase 
opt$decision <- matrix(0,prm$pr+1,12) #Periods +1 to record initial conditions
colnames(opt$decision) <- c("Period","Supply","Demand","SellQ","BuyQ","Price","Decision","StorageFlow","StorageStock","MaxStorage","MoneyFlow","MoneyStock")
opt$decision[,"Period"] <- 0:prm$pr
opt$decision[-1,"Supply"] <- opt$supply
opt$decision[-1,"Demand"] <- pdm$MWh
opt$decision[-1,"SellQ"] <- opt$sell_q
opt$decision[-1,"BuyQ"] <- opt$buy_q
opt$decision[-1,"Price"] <- opt$price
# Store simulation results
opt$simulartion_results

# Optimize the daily cycle
# Define all the possible options of the parameters to be optimized in the 
sim$vars <- expand.grid("SellP" = unique(opt$price), "BuyP" = unique(opt$price), "Machines" = 1:(sum(opt$supply-pdm$MWh)/prm$machine_storage), "Revenue" = 0, "Profit" = 0)
sim$vars <- sim$vars[sim$vars[,"SellP"] > sim$vars[,"BuyP"],] # Exclude cases where sell price is lower or equal than buy price
sim$seq <- list()

for(n in 1:nrow(sim$vars)){
  # The loop across "n" ensures that we consider every combination of variables
  print(paste("Computing variable batch",n,"out of",nrow(sim$vars),"..."))
  sim$seq[[n]] <- opt$decision
  sim$seq[[n]][,"MaxStorage"] <- prm$machine_storage*sim$vars[n,"Machines"]
  for(i in 2:nrow(opt$decision)){
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
  # Store the restults
  sim$vars[n,"Revenue"] <- sim$seq[[n]][i,"MoneyStock"]
  sim$vars[n,"Profit"] <- sim$vars[n,"Revenue"] - prm$machine_cost*sim$vars[n,"Machines"]
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
rmv$S2B1 <- sim$vars[sim$vars[,"SellP"]==spl$p2$power_p,]
rmv$S2B1 <- rmv$S2B1[rmv$S2B1[,"BuyP"]==spl$p1$power_p,]
rmv$S3B1 <- sim$vars[sim$vars[,"SellP"]==spl$p3$power_p,]
rmv$S3B1 <- rmv$S3B1[rmv$S3B1[,"BuyP"]==spl$p1$power_p,]
rmv$S3B2 <- sim$vars[sim$vars[,"SellP"]==spl$p3$power_p,]
rmv$S3B2 <- rmv$S3B2[rmv$S3B2[,"BuyP"]==spl$p2$power_p,]
cda$optimization <- data.frame("Machines"=rmv$S2B1[,"Machines"],
                        "S2B1"=rmv$S2B1[,"Profit"],
                        "S3B1"=rmv$S3B1[,"Profit"],
                        "S3B2"=rmv$S3B2[,"Profit"])
rmv$optimization_labels <- c(paste("Sell ",spl$p2$power_p,", buy ",spl$p1$power_p,sep=""),
                           paste("Sell ",spl$p3$power_p,", buy ",spl$p1$power_p,sep=""),
                           paste("Sell ",spl$p3$power_p,", buy ",spl$p2$power_p,sep=""))

chr$optimization <- ggplot2::ggplot(data = cda$optimization[1:75,], ggplot2::aes(x = Machines, y = S2B1, group = 1)) +
                    ggplot2::geom_line(ggplot2::aes(y = S2B1, colour = "S2B1"), linetype = "solid", size = 1) +
                    ggplot2::geom_line(ggplot2::aes(y = S3B1, colour = "S3B1"), linetype = "solid", size = 1) +
                    ggplot2::geom_line(ggplot2::aes(y = S3B2, colour = "S3B2"), linetype = "solid", size = 1) +
                    ggplot2::labs(title = "Profit by strategy over machine number", x = "Machines", y = "Euro") +
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
  plotTheme()

# Evolution of cash flow
cda$cashbalance <- data.frame( "Euro" = sim$seq[[sim$optimal_iter]][,"MoneyStock"], "Hour" = sim$seq[[sim$optimal_iter]][,"Period"])
chr$cashbalance <- ggplot2::ggplot(data= cda$cashbalance, ggplot2::aes(x=Hour, y=Euro)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::labs(title = "Cash balance (optimal strategy)", x = "Hour", y = "Euro") +
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