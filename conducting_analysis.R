## Bodo Winter
## Created March 3, 2015; Edited April 10, 2015
## Analysis of conducting gesture data, production only

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

options(stringsAsFactors = F)

## Load in data:

setwd('/Users/teeniematlock/Desktop/research/conducting/')
con <- read.csv('beethoven_conducting_gestures_amanda_coded_20_06_2016.csv')

## Load in packages:

library(dplyr)
library(ggplot2)
library(reshape2)

## Check:

nrow(con)		# 105 videos

# # ## Extract those where critical measues are visible:

# con.backup <- con
# con <- filter(con, Critical.Measures.Visible == 'yes')
# nrow(con)		# 42
# nrow(con) / nrow(con.backup)

## Baton left vs. baton right:

con$BatonHand <- 'right'
con[con$Baton == 'Yes (LH!)', ]$BatonHand <- 'left'
con[con$Baton == 'No', ]$BatonHand <- 'No Baton'

## Aggregate of this:

table(con$BatonHand)

## How many had batons?

con[con$Baton == 'Yes (LH!)', ]$Baton <- 'Yes'
table(con$Baton) / nrow(con)

## Exclude the three who had the baton in their left hand and those 9 that did not have the baton:

con.backup2 <- con
con <- filter(con, BatonHand == 'right')
nrow(con)

## For first two measures versus piano part and left hand:
## Extract relevant handshape (e.g., take endpoint of "fist>open" to be "open"):

con[which(con$L.1.2.HandShape == 'fist>open'), ]$L.1.2.HandShape <- 'open'
con[which(con$L.1.2.HandShape == 'open>fist'), ]$L.1.2.HandShape <- 'fist'
con[which(con$L.1.2.HandShape == 'open>point'), ]$L.1.2.HandShape <- 'point'
con[which(con$L.1.2.HandShape == 'NA>open'), ]$L.1.2.HandShape <- 'open'
con[which(con$L.1.2.HandShape == 'open>NA'), ]$L.1.2.HandShape <- NA
con[which(con$L.1.2.HandShape == ''), ]$L.1.2.HandShape <- NA

con[which(con$L.3.5.HandShape == 'fist>open'), ]$L.3.5.HandShape <- 'open'
# con[which(con$L.3.5.HandShape == 'fist>precision grip\n'), ]$L.3.5.HandShape <- 'precision grip'
con[which(con$L.3.5.HandShape == 'NA>open'), ]$L.3.5.HandShape <- 'open'
con[which(con$L.3.5.HandShape == 'open>fist'), ]$L.3.5.HandShape <- 'fist'

con[which(con$L.6.8.HandShape == 'open>point'), ]$L.6.8.HandShape <- 'point'
con[which(con$L.6.8.HandShape == 'precision grip>open'), ]$L.6.8.HandShape <- 'open'
con[which(con$L.6.8.HandShape == ''), ]$L.6.8.HandShape <- NA

## For first two measures versus piano part and left hand:
## Extract relevant palm orientation (e.g., take endpoint of "fist>open" to be "open"):

con[which(con$L.1.2.PalmOrientation == 'down>inward'), ]$L.1.2.PalmOrientation <- 'inward'
con[which(con$L.1.2.PalmOrientation == 'down>NA'), ]$L.1.2.PalmOrientation <- NA
con[which(con$L.1.2.PalmOrientation == 'down>up'), ]$L.1.2.PalmOrientation <- 'up'
con[which(con$L.1.2.PalmOrientation == 'inward>down'), ]$L.1.2.PalmOrientation <- 'down'
con[which(con$L.1.2.PalmOrientation == 'inward>NA'), ]$L.1.2.PalmOrientation <- NA
con[which(con$L.1.2.PalmOrientation == 'inward>up'), ]$L.1.2.PalmOrientation <- 'up'
con[which(con$L.1.2.PalmOrientation == 'NA\n'), ]$L.1.2.PalmOrientation <- NA
con[which(con$L.1.2.PalmOrientation == 'NA>down'), ]$L.1.2.PalmOrientation <- 'down'
con[which(con$L.1.2.PalmOrientation == 'NA>inward'), ]$L.1.2.PalmOrientation <- 'inward'
con[which(con$L.1.2.PalmOrientation == 'NA>up'), ]$L.1.2.PalmOrientation <- 'up'
con[which(con$L.1.2.PalmOrientation == 'up>NA'), ]$L.1.2.PalmOrientation <- NA
con[which(con$L.1.2.PalmOrientation == ''), ]$L.1.2.PalmOrientation <- NA

# con[which(con$L.3.5.PalmOrientation == 'down>NA'), ]$L.3.5.PalmOrientation <- NA
con[which(con$L.3.5.PalmOrientation == 'down>up'), ]$L.3.5.PalmOrientation <- 'up'
con[which(con$L.3.5.PalmOrientation == 'down>up\n'), ]$L.3.5.PalmOrientation <- 'up'
con[which(con$L.3.5.PalmOrientation == 'inward>down'), ]$L.3.5.PalmOrientation <- 'down'
con[which(con$L.3.5.PalmOrientation == 'inward>NA'), ]$L.3.5.PalmOrientation <- NA
con[which(con$L.3.5.PalmOrientation == 'inward>up'), ]$L.3.5.PalmOrientation <- 'up'
con[which(con$L.3.5.PalmOrientation == 'NA>down'), ]$L.3.5.PalmOrientation <- 'down'
con[which(con$L.3.5.PalmOrientation == 'NA>inward'), ]$L.3.5.PalmOrientation <- 'inward'
con[which(con$L.3.5.PalmOrientation == 'NA>up'), ]$L.3.5.PalmOrientation <- 'up'
con[which(con$L.3.5.PalmOrientation == 'up>NA'), ]$L.3.5.PalmOrientation <- NA

con[which(con$L.6.8.PalmOrientation == 'down\n'), ]$L.6.8.PalmOrientation <- 'down'
con[which(con$L.6.8.PalmOrientation == 'down>NA'), ]$L.6.8.PalmOrientation <- NA
con[which(con$L.6.8.PalmOrientation == 'down>inward'), ]$L.6.8.PalmOrientation <- 'inward'
con[which(con$L.6.8.PalmOrientation == 'NA>down'), ]$L.6.8.PalmOrientation <- 'down'

# con[which(con$R.1.2.HandShape == 'open>fist'), ]$R.1.2.HandShape <- 'fist'

## Get rid of NAs for cut-off gestures:

con[which(con$X1.2.Cutoff == ''), ] <- NA
con[which(con$X3.5.Cutoff == ''), ] <- NA



##------------------------------------------------------------------
## Analysis and plot of hand shape:
##------------------------------------------------------------------

## Hand shape:

xtab1 <- table(con$L.1.2.HandShape)
xtab2 <- table(con$L.3.5.HandShape)
xtab3 <- table(con$L.6.8.HandShape)

xtab1 <- xtab1[-3]	# get rid of points
xtab2 <- xtab2[-3]	# get rid of points
xtab3 <- xtab3[-2]	# get rid of points

xtab1.prop <- xtab1 / sum(xtab1)
xtab2.prop <- xtab2 / sum(xtab2)
xtab3.prop <- xtab3 / sum(xtab3)

## Relevant chi-square tests:

binom.test(c(xtab1[1], 0))		# fist forte ( = 15) vs. fist piano (= 0)
binom.test(c(xtab3[2], 0))		# precision grip piano ( = 12) vs. pgrip forte (= 0)
binom.test(c(xtab1[2], xtab3[1]))	# not significantly more open's

## Are those that have the fist also those that are the prip:

con[, c('L.1.2.HandShape', 'L.6.8.HandShape')]
(M <- with(con, table(L.1.2.HandShape, L.6.8.HandShape)))
(M <- M[1:2, c(1, 3)])
fisher.test(M)

## Plotting parameters:

yfac <- 0.04
xfac <- 0.2
xshift <- 1.2

## Set colors depending on whether it's for powerpoint or not:

set_to_powerpoint <- function(powerpoint = T) {
	if (powerpoint == T) {
		col1 <<- 'darkred'
		col2 <<- 'goldenrod3'
		} else {
			col1 <<- rgb(0, 0, 0, 0.7)
			col2 <<- rgb(0, 0, 0, 0.2)
			}	
	}
powerpoint <- F
set_to_powerpoint(powerpoint = F)
show <- 3

## Make a plot of the hand shapes:

quartz('', 12, 6)
par(mai = c(1.5, 1.5, 0.5, 0.5))
plot(1, 1, xlim = c(0, 30), ylim = c(0, 1),
	type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', yaxs = 'i')
## All axes:
mtext(side = 2, font = 2, cex = 2.2, line = 4.3, text = 'Proportion')
axis(side = 2, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), '%'),
	font = 2, las = 2, lwd.ticks = 2, cex.axis = 1.25)
axis(side = 1, at = (c(2, 5, 8) - xshift), labels = c('fist', 'open', 'grip'),
	font = 2, lwd.ticks = 2, cex.axis = 1.5)
if (show >= 2) {
	axis(side = 1, at = c(12, 15, 18), labels = c('fist', 'open', 'grip'),
		font = 2, lwd.ticks = 2, cex.axis = 1.5)
		}
if (show >= 3) {
	axis(side = 1, at = (c(22, 25, 28) + xshift), labels = c('fist', 'open', 'grip'),
		font = 2, lwd.ticks = 2, cex.axis = 1.5)
		}
## Measures bar:
if (show == 1) xselect <- 1
if (show == 2) xselect <- 1:2
if (show == 3) xselect <- 1:3
axis(side = 1, at = c(5 - xshift, 15, 25 + xshift)[xselect],
	line = 2.35, font = 2, cex.axis = 1.75,
	tick = F,
	labels = c('Measures 1-3', 'Measures 3-5', 'Measures 6-8')[xselect])
axis(side = 1, at = c(5 - xshift, 15, 25 + xshift)[xselect],
	line = 3.75, font = 2, cex.axis = 1.25,
	tick = F,
	labels = c('(Fortissimo)', '(Fortissimo)', '(Piano)')[xselect])
## Rectangles for measures 1-3:
rect(xleft = 1 - xfac - xshift, xright = 3 + xfac - xshift,
	ybottom = 0, ytop = xtab1.prop[1], col = col1, border = 'black')
rect(xleft = 4 - xfac - xshift, xright = 6 + xfac - xshift,
	ybottom = 0, ytop = xtab1.prop[2], col = col2, border = 'black')
if (!powerpoint) {
	rect(xleft = 7 - xfac - xshift, xright = 9 + xfac - xshift,
		ybottom = 0, ytop = 0.01, col = 'black',
		density = 10, border = 'black')
		} else {
			rect(xleft = 7 - xfac - xshift, xright = 9 + xfac - xshift,
				ybottom = 0, ytop = 0.01, col = 'steelblue', border = 'black')	
			}
## Rectangles for measures 3-5:
if (show >= 2) {
	rect(xleft = 11 - xfac, xright = 13 + xfac,
		ybottom = 0, ytop = xtab2.prop[1], col = col1, border = 'black')
	rect(xleft = 14 - xfac, xright = 16 + xfac,
		ybottom = 0, ytop = xtab2.prop[2], col = col2, border = 'black')
	if (!powerpoint) {
		rect(xleft = 17 - xfac, xright = 19 + xfac,
			ybottom = 0, ytop = 0.01, col = 'black',
			density = 10, border = 'black')
			} else {
				rect(xleft = 17 - xfac, xright = 19 + xfac,
					ybottom = 0, ytop = 0.01, col = 'steelblue', border = 'black')
				}
			}
## Rectangles for measures 6-8:
if (show == 3) {
	rect(xleft = 21 - xfac + xshift, xright = 23 + xfac + xshift,
		ybottom = 0, ytop = 0.01, col = col1, border = 'black')
	rect(xleft = 24 - xfac + xshift, xright = 26 + xfac + xshift,
		ybottom = 0, ytop = xtab3.prop[1], col = col2, border = 'black')
	if (!powerpoint) {
		rect(xleft = 27 - xfac + xshift, xright = 29 + xfac + xshift,
			ybottom = 0, ytop = xtab3.prop[2], col = 'black',
			density = 10, border = 'black')
			} else {
				rect(xleft = 27 - xfac + xshift, xright = 29 + xfac + xshift,
					ybottom = 0, ytop = xtab3.prop[2], col = 'steelblue', border = 'black')
				}
			}
## Text on rectangles:
if (show == 1) xselect <- 1:3
if (show == 2) xselect <- 1:6
if (show == 3) xselect <- 1:9
text(x = c(2 - xshift, 5 - xshift, 8 - xshift, 12, 15, 18, 22 + xshift, 25 + xshift, 28 + xshift)[xselect],
	y = (c(xtab1.prop[1], xtab1.prop[2], 0.01,
		xtab2.prop[1], xtab2.prop[2], 0.01,
		0.01, xtab3.prop[1], xtab3.prop[2]) + yfac)[xselect],
	labels = c(xtab1[1], xtab1[2], 0,
		xtab2[1], xtab2[2], 0,
		0, xtab3[1], xtab3[2])[xselect],
		font = 2, cex = 1.5)
## Box:
box(lwd = 2)


##------------------------------------------------------------------
## Analysis and plot of hand orientation:
##------------------------------------------------------------------

# Palm Orientation:

xtab1 <- table(con$L.1.2.PalmOrientation)
xtab2 <- table(con$L.3.5.PalmOrientation)
xtab3 <- table(con$L.6.8.PalmOrientation)

xtab1.prop <- xtab1 / sum(xtab1)
xtab2.prop <- xtab2 / sum(xtab2)
xtab3.prop <- xtab3 / sum(xtab3)

## Relevant inferential stats:

(M <- matrix(c(xtab1[c(1, 3)],
	xtab3[c(1, 3)]), nrow = 2, byrow = T))
fisher.test(M)
binom.test(M[, 2])	# separate analysis, up gestures more in fortissimo
binom.test(M[, 1])	# separate analysis, down gestures more in piano

## Set plotting parameters:

powerpoint <- F
set_to_powerpoint(powerpoint = F)
show <- 3

## Make a plot of hand orientation:

quartz('', 11.5, 6)
par(mai = c(1.5, 1.5, 0.5, 0.5))
plot(1, 1, xlim = c(0.5, 29.5), ylim = c(0, 1),
	type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', yaxs = 'i')
## All axes:
mtext(side = 2, font = 2, cex = 2.2, line = 4.3, text = 'Proportion')
axis(side = 2, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), '%'),
	font = 2, las = 2, lwd.ticks = 2, cex.axis = 1.25)
axis(side = 1, at = c(2 - xshift, 5 - xshift, 8 - xshift), labels = c('up', 'inward', 'down'),
	font = 2, lwd.ticks = 2, cex.axis = 1.4)
if (show >= 2) {
	axis(side = 1, at = c(12, 15, 18), labels = c('up', 'inward', 'down'),
		font = 2, lwd.ticks = 2, cex.axis = 1.4)
	}
if (show == 3) {
	axis(side = 1, at = c(22 + xshift, 25 + xshift, 28 + xshift), labels = c('up', 'inward', 'down'),
		font = 2, lwd.ticks = 2, cex.axis = 1.4)
	}
## Measures bar:
if (show == 1) xselect <- 1
if (show == 2) xselect <- 1:2
if (show == 3) xselect <- 1:3
axis(side = 1, at = c(5 - xshift, 15, 25 + xshift)[xselect],
	line = 2.35, font = 2, cex.axis = 1.75,
	tick = F,
	labels = c('Measures 1-3', 'Measures 3-5', 'Measures 6-8')[xselect])
axis(side = 1, at = c(5 - xshift, 15, 25 + xshift)[xselect],
	line = 3.75, font = 2, cex.axis = 1.25,
	tick = F,
	labels = c('(Fortissimo)', '(Fortissimo)', '(Piano)')[xselect])
## Rectangles for measures 1-3:
rect(xleft = 1 - xfac - xshift, xright = 3 + xfac - xshift,
	ybottom = 0, ytop = xtab1.prop[3], col = col1, border = 'black')
rect(xleft = 4 - xfac - xshift, xright = 6 + xfac - xshift,
	ybottom = 0, ytop = xtab1.prop[2], col = col2, border = 'black')
if (!powerpoint) {
	rect(xleft = 7 - xfac - xshift, xright = 9 + xfac - xshift,
		ybottom = 0, ytop = xtab1.prop[1],
		col = 'black', border = 'black', density = 10)
		} else {
			rect(xleft = 7 - xfac - xshift, xright = 9 + xfac - xshift,
				ybottom = 0, ytop = xtab1.prop[1],
				col = 'steelblue', border = 'black')			
			}
## Rectangles for measures 3-5:
if (show >= 2) {
	rect(xleft = 11 - xfac, xright = 13 + xfac,
		ybottom = 0, ytop = xtab2.prop[3], col = col1, border = 'black')
	rect(xleft = 14 - xfac, xright = 16 + xfac,
		ybottom = 0, ytop = xtab2.prop[2], col = col2, border = 'black')
	if (!powerpoint) {
		rect(xleft = 17 - xfac, xright = 19 + xfac,
			ybottom = 0, ytop = xtab2.prop[1],
			col = 'black', border = 'black', density = 10)
			} else {
				rect(xleft = 17 - xfac, xright = 19 + xfac,
					ybottom = 0, ytop = xtab2.prop[1],
					col = 'steelblue', border = 'black')				
				}
	}
## Rectangles for measures 6-8:
if (show == 3) {
	rect(xleft = 21 - xfac + xshift, xright = 23 + xfac + xshift,
		ybottom = 0, ytop = xtab3.prop[3], col = col1, border = 'black')
	rect(xleft = 24 - xfac + xshift, xright = 26 + xfac + xshift,
		ybottom = 0, ytop = xtab3.prop[2], col = col2, border = 'black')
	if (!powerpoint) {
		rect(xleft = 27 - xfac + xshift, xright = 29 + xfac + xshift,
			ybottom = 0, ytop = xtab3.prop[1], col = 'black',
			density = 10, border = 'black')
			} else {
				rect(xleft = 27 - xfac + xshift, xright = 29 + xfac + xshift,
					ybottom = 0, ytop = xtab3.prop[1], col = 'steelblue', border = 'black')				
				}
	}
## Text on rectangles:
if (show == 1) xselect <- 1:3
if (show == 2) xselect <- 1:6
if (show == 3) xselect <- 1:9
text(x = c(2 - xshift, 5 - xshift, 8 - xshift, 12, 15, 18, 22 + xshift, 25 + xshift, 28 + xshift)[xselect],
	y = (c(as.numeric(xtab1.prop)[c(3, 2, 1)],
		as.numeric(xtab2.prop)[c(3, 2, 1)],
		as.numeric(xtab3.prop)[c(3, 2, 1)]) + yfac)[xselect],
	labels = c(as.numeric(xtab1)[c(3, 2, 1)],
		as.numeric(xtab2)[c(3, 2, 1)],
		as.numeric(xtab3)[c(3, 2, 1)])[xselect],
		font = 2, cex = 1.5)
## Box:
box(lwd = 2)




##------------------------------------------------------------------
## Analysis and plot of cut-off gestures:
##------------------------------------------------------------------

## Closing gesture:

xtab1 <- table(con$X1.2.Cutoff)
xtab2 <- table(con$X3.5.Cutoff)
xtab1.prop <- xtab1 / sum(xtab1)
xtab2.prop <- xtab2 / sum(xtab2)

## Inferential stats of this:

fisher.test(rbind(xtab1, xtab2))
binom.test(xtab1)	# separate analysis
binom.test(xtab2)	# separate analysis

## Plot of these proportions:

xshift <- 0.6
quartz('', 9, 5.5)
par(mai = c(1.75, 1.5, 0.5, 0.5))
plot(1, 1, xlim = c(-0.5, 14.5), ylim = c(0, 1),
	type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', yaxs = 'i')
## All axes:
mtext(side = 2, font = 2, cex = 2.2, line = 4.3, text = 'Proportion')
axis(side = 2, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), '%'),
	font = 2, las = 2, lwd.ticks = 2, cex.axis = 1.25)
axis(side = 1, at = c(2 - xshift, 5 - xshift), labels = c('No cut-off', 'Cut-off'),
	font = 2, lwd.ticks = 2, cex.axis = 1.4)
axis(side = 1, at = c(9 + xshift, 12 + xshift), labels = c('No cut-off', 'Cut-off'),
	font = 2, lwd.ticks = 2, cex.axis = 1.4)
axis(at = c(3.5 - xshift, 10.5 + xshift), side = 1,
	tick = F, labels = c('First transition', 'Second transition'),
	line = 2.35, font = 2, cex.axis = 1.75)
axis(side = 1, at = c(3.5 - xshift, 10.5 + xshift),
	line = 3.75, font = 2, cex.axis = 1.25,
	tick = F,
	labels = c('(Fortissimo to fortissimo)', '(Fortissimo to piano)'))
# Fortissimo to fortissimo
rect(xleft = 1 - xfac - xshift, xright = 3 + xfac - xshift,
	ybottom = 0, ytop = xtab1.prop[2], col = 'goldenrod3', border = 'black')
text(x = 2 - xshift, y = xtab1.prop[2] + yfac,
	labels = xtab1[2],
	font = 2, cex = 1.5)
rect(xleft = 4 - xfac - xshift, xright = 6 + xfac - xshift,
	ybottom = 0, ytop = xtab1.prop[1], col = 'steelblue', border = 'black')
text(x = 5 - xshift, y = xtab1.prop[1] + yfac,
	labels = xtab1[1],
	font = 2, cex = 1.5)
# Fortissimo to piano
rect(xleft = 8 - xfac + xshift, xright = 10 + xfac + xshift,
	ybottom = 0, ytop = xtab2.prop[2], col = 'goldenrod3', border = 'black')
text(x = 9 + xshift, y = xtab2.prop[2] + yfac,
	labels = xtab2[2],
	font = 2, cex = 1.5)
rect(xleft = 11 - xfac + xshift, xright = 13 + xfac + xshift,
	ybottom = 0, ytop = xtab2.prop[1], col = 'steelblue', border = 'black')
text(x = 12 + xshift, y = xtab2.prop[1] + yfac,
	labels = xtab2[1],
	font = 2, cex = 1.5)
box(lwd = 2)

## Is the presence of cut-off gestures related to hand shape previously?

M1 <- table(con$X1.2.Cutoff, con$L.3.5.HandShape)[, 1:2]
M2 <- table(con$X3.5.Cutoff, con$L.6.8.HandShape)[, -2]
fisher.test(M1)
fisher.test(M2)




##------------------------------------------------------------------
## Incorporating duration:
##------------------------------------------------------------------

## Load in durations:

dur <- read.csv('durations.csv')

## Merge into data frame:

dur$Forte1HandShape <- con[match(dur$ID, con$ID), ]$L.1.2.HandShape
dur$Forte2HandShape <- con[match(dur$ID, con$ID), ]$L.3.5.HandShape
dur$PianoHandShape <- con[match(dur$ID, con$ID), ]$L.6.8.HandShape
dur$Forte1PalmOrientation <- con[match(dur$ID, con$ID), ]$L.1.2.PalmOrientation
dur$Forte2PalmOrientation <- con[match(dur$ID, con$ID), ]$L.3.5.PalmOrientation
dur$FirstCutoff <- con[match(dur$ID, con$ID), ]$X1.2.Cutoff
dur$SecondCutoff <- con[match(dur$ID, con$ID), ]$X3.5.Cutoff

## Make a palm-up vs. fist variable:

dur$Forte1PalmVsFist <- NA
ind <- which(dur$Forte1HandShape == 'open' & dur$Forte1PalmOrientation == 'up')
dur[ind, ]$Forte1PalmVsFist <- 'palm-up'
dur[which(dur$Forte1HandShape == 'fist'),]$Forte1PalmVsFist <- 'fist'

dur$Forte2PalmVsFist <- NA
ind <- which(dur$Forte2HandShape == 'open' & dur$Forte2PalmOrientation == 'up')
dur[ind, ]$Forte2PalmVsFist <- 'palm-up'
dur[which(dur$Forte2HandShape == 'fist'),]$Forte2PalmVsFist <- 'fist'

## Speed-adjusted fermatas:

dur$fermata1.rel <- dur$fermata1 / dur$forte1
dur$fermata2.rel <- dur$fermata2 / dur$forte2

## Full phrase:

dur$FOR1 <- dur$forte1 + dur$fermata1
dur$FOR2 <- dur$forte2 + dur$fermata2

## Correlate pause duration with presence of cut-off gestures:

t.test(pause1 ~ FirstCutoff, dur, var.equal = T)
t.test(pause2 ~ SecondCutoff, dur, var.equal = T)

## Correlate pause duration with hand shape:

t.test(fermata1 ~ Forte1HandShape,
	dur[dur$Forte1HandShape %in% c('fist', 'open'), ])
t.test(fermata2 ~ Forte2HandShape,
	dur[dur$Forte2HandShape %in% c('fist', 'open'), ])
t.test(fermata1.rel ~ Forte1HandShape,
	dur[dur$Forte1HandShape %in% c('fist', 'open'), ])
t.test(fermata2.rel ~ Forte2HandShape,
	dur[dur$Forte2HandShape %in% c('fist', 'open'), ])
t.test(FOR1 ~ Forte1HandShape,
	dur[dur$Forte1HandShape %in% c('fist', 'open'), ])
t.test(FOR2 ~ Forte2HandShape,
	dur[dur$Forte2HandShape %in% c('fist', 'open'), ])	

## Correlate duration with hand shape, fist vs. palm-up:

t.test(fermata1 ~ Forte1PalmVsFist, dur)
t.test(fermata2 ~ Forte2PalmVsFist, dur)
t.test(fermata1.rel ~ Forte1PalmVsFist, dur)
t.test(fermata2.rel ~ Forte2PalmVsFist, dur)
t.test(FOR1 ~ Forte1PalmVsFist, dur)
t.test(FOR2 ~ Forte2PalmVsFist, dur)

## Model for plotting duration:

xmdl <- lm(pause2 ~ SecondCutoff, data = dur)
xpred <- data.frame(SecondCutoff = na.omit(unique(dur$SecondCutoff)))
xpred <- cbind(xpred,
	as.data.frame(predict(xmdl, newdata = xpred, se.fit = T)[1:2]))
xpred$se.fit <- xpred$se.fit * 1.96

## Graph for plotting duration:

quartz('', 9, 5.5)
par(mai = c(1.75, 1.5, 0.5, 0.5))
plot(1, 1, xlim = c(0, 800), ylim = c(0, 3.5),
	type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', yaxs = 'i', bty = 'n', xaxs = 'i')
## Rectangles:
rect(xleft = 0, xright = xpred[2, ]$fit * 1000, ybottom = 0.5,
	ytop = 1.5, col = 'goldenrod3', border = 'black')
rect(xleft = 0, xright = xpred[1, ]$fit * 1000, ybottom = 2,
	ytop = 3, col = 'steelblue', border = 'black')
## Error bars:
arrows(y0 = 1, x0 = xpred[2, ]$fit * 1000 - xpred[2, ]$se.fit * 1000,
	x1 = xpred[2, ]$fit * 1000 + xpred[2, ]$se.fit * 1000, lwd = 2, angle = 90, code = 3, length = 0.15)
arrows(y0 = 2.5, x0 = xpred[1, ]$fit * 1000 - xpred[1, ]$se.fit * 1000,
	x1 = xpred[1, ]$fit * 1000 + xpred[1, ]$se.fit * 1000, lwd = 2, angle = 90, code = 3, length = 0.15)
## All axes:
# axis(side = 2, at = c(1, 2.5), labels = c('No Cut-off', 'Cut-off'),
	# font = 2, cex.axis = 1.5, lwd.ticks = 2, las = 2, lwd = 2)
segments(x0 = 0, x1 = 0, y0 = 0, y1 = 3.5, lwd = 2, xpd = NA)
axis(side = 1, at = seq(0, 800, 200), labels = seq(0, 800, 200),
	font = 2, cex.axis = 1.5, lwd = 2, lwd.ticks = 2)
mtext(side = 1, font = 2, cex = 2.2, line = 3.75, text = 'Duration (ms)')



##------------------------------------------------------------------
## Incorporating amplitude analysis:
##------------------------------------------------------------------

## Load in durations:

ins <- read.csv('intensity.csv')

## Normalize amplitudes:

for (i in 1:nrow(ins)) {
	these_intensities <- as.numeric(ins[i, -1])
	these_intensities <- (these_intensities - mean(these_intensities)) / sd(these_intensities)
	ins[i, -1] <- these_intensities
	}

## Merge into data frame:

ins$Forte1HandShape <- con[match(ins$ID, con$ID), ]$L.1.2.HandShape
ins$Forte2HandShape <- con[match(ins$ID, con$ID), ]$L.3.5.HandShape
ins$PianoHandShape <- con[match(ins$ID, con$ID), ]$L.6.8.HandShape
ins$Forte1PalmOrientation <- con[match(ins$ID, con$ID), ]$L.1.2.PalmOrientation
ins$Forte2PalmOrientation <- con[match(ins$ID, con$ID), ]$L.3.5.PalmOrientation
ins$PianoPalmOrientation <- con[match(ins$ID, con$ID), ]$L.6.8.PalmOrientation

## Make a palm-up vs. fist variable:

ins$Forte1PalmVsFist <- NA
ind <- which(ins$Forte1HandShape == 'open' & ins$Forte1PalmOrientation == 'down')
ins[ind, ]$Forte1PalmVsFist <- 'palm-down'
ins[which(ins$Forte1HandShape == 'fist'),]$Forte1PalmVsFist <- 'fist'

ins$Forte2PalmVsFist <- NA
ind <- which(ins$Forte2HandShape == 'open' & ins$Forte2PalmOrientation == 'down')
ins[ind, ]$Forte2PalmVsFist <- 'palm-down'
ins[which(ins$Forte2HandShape == 'fist'),]$Forte2PalmVsFist <- 'fist'

## Make a palm-up vs. palm-down variable:

ins$Forte1PalmVsFist <- NA
ind <- which(ins$Forte1HandShape == 'open' & ins$Forte1PalmOrientation == 'down')
ins[ind, ]$Forte1PalmVsFist <- 'palm-down'
ins[which(ins$Forte1HandShape == 'fist'),]$Forte1PalmVsFist <- 'fist'

## Correlate pause intensity with hand shape:

t.test(forte1 ~ Forte1HandShape,
	ins[ins$Forte1HandShape %in% c('fist', 'open'), ])
t.test(forte2 ~ Forte2HandShape,
	ins[ins$Forte2HandShape %in% c('fist', 'open'), ])
t.test(fermata1 ~ Forte1HandShape,
	ins[ins$Forte1HandShape %in% c('fist', 'open'), ])
t.test(fermata2 ~ Forte2HandShape,
	ins[ins$Forte2HandShape %in% c('fist', 'open'), ])
t.test(forte1 ~ Forte1PalmOrientation,
	ins[ins$Forte1PalmOrientation %in% c('up', 'down'), ])
t.test(forte2 ~ Forte2PalmOrientation,
	ins[ins$Forte2PalmOrientation %in% c('up', 'down'), ])
t.test(piano ~ PianoHandShape,
	ins[!(ins$PianoHandShape %in% 'point'), ])

## Correlate pause intensity with hand shape, relative measure:

t.test(forte1 / piano ~ Forte1HandShape,
	ins[ins$Forte1HandShape %in% c('fist', 'open'), ])
t.test(forte2 / piano ~ Forte2HandShape,
	ins[ins$Forte2HandShape %in% c('fist', 'open'), ])
t.test(fermata1 / piano ~ Forte1HandShape,
	ins[ins$Forte1HandShape %in% c('fist', 'open'), ])
t.test(fermata2 / piano ~ Forte2HandShape,
	ins[ins$Forte2HandShape %in% c('fist', 'open'), ])
t.test(forte1 / piano ~ Forte1PalmOrientation,
	ins[ins$Forte1PalmOrientation %in% c('up', 'down'), ])
t.test(forte2 / piano ~ Forte2PalmOrientation,
	ins[ins$Forte2PalmOrientation %in% c('up', 'down'), ])
t.test(piano / forte2 ~ PianoHandShape,
	ins[!(ins$PianoHandShape %in% 'point'), ])

## Correlate intensity with hand shape, fist vs. palm-up:

t.test(fermata1 ~ Forte1PalmVsFist, ins)
t.test(fermata2 ~ Forte2PalmVsFist, ins)
t.test(forte1 ~ Forte1PalmVsFist, ins)
t.test(forte2 ~ Forte2PalmVsFist, ins)

## Correlate intensity with hand shape, fist vs. palm-up, relative measure:

t.test(fermata1 / piano ~ Forte1PalmVsFist, ins)
t.test(fermata2 / piano ~ Forte2PalmVsFist, ins)
t.test(forte1 / piano ~ Forte1PalmVsFist, ins)
t.test(forte2 / piano ~ Forte2PalmVsFist, ins)



