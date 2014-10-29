library(reshape2)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(dplyr)

# Take a look at the responses from the most recent poll (top-level summary) ####

all <- read.csv("data/all-crimes-summary.csv", header=TRUE, stringsAs=FALSE, sep=",")
all$Crime <- factor(all$Crime, rev(all$Crime), ordered=TRUE)
all_m <- melt(all, id.vars = c("Crime"))
colnames(all_m) <- c("Crime", "Response", "Value")

gg <- ggplot(all_m, aes(x=Crime, y=Value, fill=Response))
gg <- gg + geom_bar(position="fill", stat="identity")
gg <- gg + geom_hline(yintercept=0.5, alpha=0.33)
gg <- gg + scale_y_continuous(labels = percent_format(), expand=c(0,0))
gg <- gg + scale_fill_brewer(palette="BrBG", type="div", labels=gsub("_", "\n", levels(all_m$Response)))
gg <- gg + coord_flip()
gg <- gg + labs(x=NULL, y=NULL, title="How often do you, yourself,\nworry about the following things?")
gg <- gg + theme_bw()
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.position="bottom")
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
gg <- gg + geom_hline(aes(yintercept=mid), alpha=0.25)
gg <- gg + scale_y_continuous(labels = percent_format(), expand=c(0,0))
gg <- gg + scale_fill_brewer(palette="BrBG", type="div", labels=gsub("_", "\n", levels(resp_m$Response)))
gg <- gg + labs(x=NULL, y=NULL, title="Survey Responses Crime by YoY")
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~Survey_Year, ncol=2)
gg <- gg + theme_bw()
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(strip.background=element_blank())
gg <- gg + theme(legend.position="bottom")
gg

# Make a rank-order parallel coordinate chart ####

# use ntile so we can fit them into 1-10

resp_rank <- responses_by_crime_by_year %>%
  group_by(Survey_year) %>%
  mutate(fo=Frequently+Frequently, fo_rank=ntile(fo, 10)) %>%
  ungroup %>% select(Crime, Survey_year, fo, fo_rank)

resp_rank$Survey_year <- factor(resp_rank$Survey_year, seq(2000, 2016, 1))

# there's prbly a dyply way to do this. meh

last_year <- resp_rank[resp_rank$Survey_year==2014,]
last_year <- data.table::rbindlist(lapply(split(last_year, last_year$fo_rank), function(x) {
  if (nrow(x) == 1) {
    data.frame(Survey_year=x$Survey_year, fo_rank=x$fo_rank, Crime=x$Crime)
  } else {
    data.frame(Survey_year=x$Survey_year[1], fo_rank=x$fo_rank[1], Crime=paste(x$Crime, sep="", collapse="\n"))
  }
}))

gg <- ggplot(resp_rank, aes(x=Survey_year, y=fo_rank, group=Crime))
gg <- gg + geom_line(aes(color=Crime), size=0.3)
gg <- gg + geom_text(data=last_year,
                     aes(label=Crime), color="black", hjust=-0.09, size=4)
gg <- gg + scale_x_discrete(drop=FALSE, labels=c(sprintf("%02d", seq(00, 14, 1)), "", ""))
gg <- gg + scale_y_continuous(breaks=seq(1, 10, 1), labels=NULL)
gg <- gg + scale_color_manual(values=c("#000000", "#9D9D9D", "#BE2633", "#E06F8B", "#493C2B",
                                       "#A46422", "#EB8931", "#F7E26B", "#2F484E", "#44891A",
                                       "#A3CE27", "#1B2632", "#005784", "#31A2F2", "#B2DCEF"))
gg <- gg + geom_point(aes(color=Crime), size=2.5)
gg <- gg + labs(y=NULL, x=NULL)
gg <- gg + theme_bw()
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.position="none")
gg <- gg + theme(axis.ticks.x=element_blank())
gg <- gg + theme(axis.ticks.y=element_blank())
gg

# Just "Car stolen" ####

resp_rank <- responses_by_crime_by_year %>%
  group_by(Survey_year) %>%
  mutate(fo=Frequently+Frequently, fo_rank=ntile(fo, 10)) %>%
  ungroup %>% select(Crime, Survey_year, fo, fo_rank)

gg <- ggplot(resp_rank[resp_rank$Crime=="Car stolen not present",], aes(x=Survey_year, y=fo_rank, group=Crime))
gg <- gg + geom_line(aes(color=Crime), size=0.3)
gg <- gg + scale_x_discrete(skip=FALSE, labels=sprintf("%02d", seq(00, 14, 1)))
gg <- gg + scale_y_continuous(limits=c(1,10), breaks=seq(1, 10, 1), labels=NULL)
gg <- gg + scale_color_manual(values=c("#BE2633"))
gg <- gg + geom_point(aes(color=Crime), size=2.5)
gg <- gg + labs(y=NULL, x=NULL)
gg <- gg + theme_bw()
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.position="none")
gg <- gg + theme(axis.ticks.x=element_blank())
gg <- gg + theme(axis.ticks.y=element_blank())
gg








