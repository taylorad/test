library(lubridate)
library (plyr)
library (dplyr)
library (ggplot2)

# Upload all years and join together

# Add new year as required (make sure columns are consistent)
years <- c(2003,2004,2005,2006,2007,2008,2009,2011,2016,2017,2018,2019) 

# Run loop to bring in all years and merge together
# RSTsum dataframe should contain all files
RSTsum <- NULL
for(i in years) {
  RSTsum_yr <- read.csv(paste0('C:/Users/TaylorAn/Desktop/RSTsummary/RSTsummary_', i, '.csv'))
  RSTsum <- rbind(RSTsum, RSTsum_yr)
}
rm (RSTsum_yr)
head (RSTsum)


# Convert dates to R format
RSTsum$Date <- ymd (RSTsum$Date)
# Pull out year 
RSTsum$Year <- year (RSTsum$Date)

# a row of NA loaded (don't know why)
RSTsum <- RSTsum[!is.na(RSTsum$Year),]

# Summarize by year for smolt estimates 
  # 2005 merges both wheels on west branch for estimate
RST_summary <- ddply (RSTsum,. (Year, Branch), summarize,
                      mark = sum (na.omit(Mark)), 
                      recap = sum (na.omit (Recap)),
                      cap = sum (na.omit (Capture)))

# Can split the wheels and branch (don't think it is necessary/valuable)
#RST_summary <- ddply (RSTsum,. (Year, Branch, Wheel), summarize,
 #                     mark = sum (na.omit(Mark)), 
  #                    recap = sum (na.omit (Recap)))


# Calculate mod peterson estimates 
RST_summary$captured <- RST_summary$mark + RST_summary$recap
RST_summary$estimate <- ((RST_summary$mark + 1) * (RST_summary$captured+1)) / (RST_summary$recap + 1)
RST_summary$variance <- ((RST_summary$mark + 1)*(RST_summary$captured+1)*
                           (RST_summary$mark - RST_summary$recap)*
                           (RST_summary$captured-RST_summary$recap)) / (((RST_summary$recap+1)^2)*
                                                                          (RST_summary$recap+2))
RST_summary$sd <- sqrt (RST_summary$variance)
RST_summary$UCI <- RST_summary$estimate + (1.965*RST_summary$sd)
RST_summary$LCI <- RST_summary$estimate - (1.965*RST_summary$sd)
RST_summary$LCI <- ifelse (RST_summary$LCI < 0, 0, RST_summary$LCI)

# Filter to only show years that mark-recap was conducted! (not just total captures)
RST_summary_true <- RST_summary[RST_summary$captured > 0,]

# plot smolt estimates by year (West Branch)
WB_plot <- ggplot (RST_summary_true[RST_summary_true$Branch == 'WB',], aes (x = Year, y = estimate))+
  geom_bar(stat='identity') +
  geom_errorbar(aes (ymin = LCI,ymax=UCI), width = 0.2) + theme_bw()
WB_plot

# East Branch - very little data
EB_plot <- ggplot (RST_summary_true[RST_summary_true$Branch == 'EB',], aes (x = Year, y = estimate))+
  geom_bar(stat='identity') +
  geom_errorbar(aes (ymin = LCI,ymax=UCI), width = 0.2) + theme_bw()
EB_plot


# Calculate/visualize percent of run by day
totalcatch <- RST_summary[c('Year', 'Branch', 'cap')]
totalcatch <- plyr::rename (totalcatch, c('cap' = 'totalcatch'))

# Merge total run with main df
RSTsum <- merge (RSTsum, totalcatch)

# Create column for proportion of catch per day
RSTsum$prop.catch <- RSTsum$Capture/RSTsum$totalcatch

# Create julian day for plotting all years together
RSTsum$Day <- yday (RSTsum$Date)


# Can look at plots for each year --- update year in temp df
temp <- RSTsum[RSTsum$Year == '2019',]

annual_plot <- ggplot (temp[temp$Branch == 'WB',], aes(x=Date, y=prop.catch)) +
  geom_histogram(stat='identity')
annual_plot


# all years as line plots for peaks 
#runtime_plot <- ggplot (RSTsum[RSTsum$Branch == 'WB',], aes(x=Day, y=prop.catch,colour = as.factor(Year))) +
 # geom_line(stat='identity') + geom_line()
#runtime_plot

## Plotting all years 
runtime_plot <- ggplot (RSTsum[RSTsum$Branch == 'WB',], aes(x=Day, y=prop.catch)) +
  geom_histogram(stat = 'identity') + facet_wrap (~Year)
runtime_plot


## Determine limits of run time
# calculate cumulative total of run caught
# Must turn all NA's in prop.catch to 0 (do in new column to avoid confusion) - this is for calculation purposes
RSTsum$prop.catch.na <- RSTsum$prop.catch
RSTsum$prop.catch.na[is.na(RSTsum$prop.catch.na)] <- 0

RSTsum <- dplyr::mutate (group_by(RSTsum, Year, Branch), cum_prop = cumsum(prop.catch.na))

RSTsum$cat_bounds <- NA
RSTsum$cat_bounds <- ifelse (RSTsum$cum_prop < 0.25, 'start', 
                             ifelse (RSTsum$cum_prop > 0.25 & RSTsum$cum_prop < 0.75, 'peak',
                                     ifelse (RSTsum$cum_prop >0.75, 'end', RSTsum$cat_bounds)))

quartiles <- ddply (RSTsum,. (Year, Branch, cat_bounds), summarize,
                    Q25 = max (Day), Q75 = min(Day))

quartiles_min <- quartiles [quartiles$cat_bounds == 'start',]
quartiles_max <- quartiles [quartiles$cat_bounds == 'end',]
#quartiles <- quartiles [quartiles$cat_bounds == 'peak',]
quartiles_min <- quartiles_min[c('Year','Branch','Q25')]
quartiles_max <- quartiles_max[c('Year','Branch','Q75')]
quartiles_range <- merge (quartiles_min,quartiles_max)


# merge Q25 and Q75 values with main Df
RSTsum <- merge (RSTsum, quartiles_range)

# Add quartile lines to plot to see peak run timing 
## Plotting all years 

runtime_plot <- ggplot (RSTsum_WB, aes(x=Day, y=prop.catch)) +
  geom_histogram(stat = 'identity') + facet_wrap (~Year) +
  geom_vline(xintercept=as.numeric(RSTsum_WB$Q25), colour = "grey40")
runtime_plot

RSTsum_WB <- RSTsum[RSTsum$Branch == 'WB',]

# Create loop for all years; bind together as grided plot after (can't get vlines using facet_wrap)
for (i in years){
temp <- RSTsum_WB[RSTsum_WB$Year == i,]

annual_plot <- ggplot (temp, aes(x=Day, y=prop.catch)) +
  geom_histogram(stat='identity') + geom_vline (xintercept = temp$Q25 - 0.5, colour = 'red') +
  geom_vline (xintercept = temp$Q75 + 0.5, colour = 'red')
jpeg (paste0('propplot',i,'.jpg'), width = 4,height = 4,units = 'in',res=400)
print(annual_plot)
dev.off()

}

