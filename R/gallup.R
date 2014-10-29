library(reshape2)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Take a look at the responses from the most recent poll (top-level summary) ####

all <- read.csv("data/all-crimes-summary.csv", header=TRUE, stringsAs=FALSE, sep=",")
all$Crime <- factor(all$Crime, all$Crime, ordered=TRUE)
all_m <- melt(all, id.vars = c("Crime"))
colnames(all_m) <- c("Crime", "Response", "Value")


gg <- ggplot(all_m, aes(x=Crime, y=Value, fill=Response))
gg <- gg + geom_bar(position="fill", stat="identity")
gg <- gg + scale_y_continuous(labels = percent_format(), expand=c(0,0))
gg <- gg + scale_fill_brewer(palette="BrBG", type="div")
gg <- gg + coord_flip()
gg <- gg + labs(x=NULL, y=NULL, title="How often do you, yourself, worry about the following things?")
gg <- gg + theme_bw()
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg

# Look at responses from previous polls ####

responses_by_crime_by_year <- read.csv("data/responses-by-crime-by-year.csv", header=TRUE, stringsAs=FALSE, sep=",")
responses_by_crime_by_year$Survey_year <- factor(responses_by_crime_by_year$Survey_year)
responses_by_crime_by_year$mid <- 0.50
resp_m <- melt(responses_by_crime_by_year, id.vars = c("Crime", "Survey_year", "mid"))
colnames(resp_m) <- c("Crime", "Survey Year", "mid", "Response", "Value")

# the line chart isn't very helpful ####

gg <- ggplot(resp_m, aes(x=`Survey Year`, y=Value, group=Response))
gg <- gg + geom_line(aes(color=Response))
gg <- gg + facet_wrap(~Crime)
gg <- gg + scale_color_manual(values=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#a65628"))
gg <- gg + theme(axis.text.y = element_text(angle = 90, hjust = 1))
gg <- gg + theme_bw()
gg

# the facets are, tho; this one is Year by Crime ####

colnames(resp_m) <- c("Crime", "Survey Year", "mid", "Response", "Value")
gg <- ggplot(resp_m, aes(x=`Survey Year`, y=Value, fill=Response))
gg <- gg + geom_bar(position="fill", stat="identity")
gg <- gg + geom_hline(aes(yintercept=mid))
gg <- gg + scale_y_continuous(labels = percent_format(), expand=c(0,0))
gg <- gg + scale_fill_brewer(palette="BrBG", type="div")
gg <- gg + labs(x=NULL, y=NULL, title="Survey Responses YoY by Crime")
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~Crime)
gg <- gg + theme_bw()
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg

# this one is Crime by Year - easier to see the Terrorism & Cyber similarities this way ####

colnames(resp_m) <- c("Crime", "Survey_Year", "mid", "Response", "Value")
gg <- ggplot(resp_m, aes(x=Crime, y=Value, fill=Response))
gg <- gg + geom_bar(position="fill", stat="identity")
gg <- gg + geom_hline(aes(yintercept=mid))
gg <- gg + scale_y_continuous(labels = percent_format(), expand=c(0,0))
gg <- gg + scale_fill_brewer(palette="BrBG", type="div")
gg <- gg + labs(x=NULL, y=NULL, title="Survey Responses Crime by YoY")
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~Survey_Year)
gg <- gg + theme_bw()
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg

# just show those two ####
gg <- ggplot(resp_m[resp_m$Survey_Year %in% c(2000, 2001, 2013, 2014),], aes(x=Crime, y=Value, fill=Response))
gg <- gg + geom_bar(position="fill", stat="identity")
gg <- gg + geom_hline(aes(yintercept=mid))
gg <- gg + scale_y_continuous(labels = percent_format(), expand=c(0,0))
gg <- gg + scale_fill_brewer(palette="BrBG", type="div")
gg <- gg + labs(x=NULL, y=NULL, title="Survey Responses Crime by YoY")
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~Survey_Year, ncol=2)
gg <- gg + theme_bw()
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg











