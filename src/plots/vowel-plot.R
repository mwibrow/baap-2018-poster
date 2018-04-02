list.of.packages <- c("ggplot2", "showtext", "reshape2", "plyr", "stringr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (package in list.of.packages) {
	library(package, character.only=TRUE)
}

lobanov <- function(df, f1='f1', f2='f2', vowel='vowel', group=c(), reduce=TRUE) {
  ddply(df, group, function(df.grp) {
	  f1.grp <- df.grp[,f1]
	  f2.grp <- df.grp[,f2]
	  mn.f1.grp <- mean(f1.grp, na.rm=TRUE)
	  mn.f2.grp <- mean(f2.grp, na.rm=TRUE)
	  sd.f1.grp <- sd(f1.grp, na.rm=TRUE)
	  sd.f2.grp <- sd(f2.grp, na.rm=TRUE)
	  ddply(df.grp, vowel, function(df.vwl) {
		  f1.vwl <- df.vwl[,f1]
		  f2.vwl <- df.vwl[,f2]
		  f1.vwl.nrm <- (f1.vwl - mn.f1.grp) / sd.f1.grp
		  f2.vwl.nrm <- (f2.vwl - mn.f2.grp) / sd.f2.grp
		  if (reduce) {
			  f1.vwl.nrm <- mean(f1.vwl.nrm)
			  f2.vwl.nrm <- mean(f2.vwl.nrm)
		  }
		  data.frame(f1=f1.vwl.nrm, f2=f2.vwl.nrm)
	  })
  })
}

monophthongs <- c()
ipa <- {}

map <- "heed /i:/ hid /ɪ/ head /e/ heard /ɜ:/ had /æ/ hud /ʌ/ hard /ɑ:/ hod /ɒ/ hoard /ɔ:/ whod /u:/ hood /ʊ/"
matches <- as.data.frame(str_match_all(map, "([a-z]+)\\s+/([^/]{1,2})/")[[1]])
invisible(apply(matches, 1, FUN=function(row){
	ipa[row["V2"]] <<- row["V3"]
	monophthongs <<- c(monophthongs, row["V2"])
}))



sse.df <- data.frame(
  vowel=c("heed", "hid", "head", "had", "hard", "hod", "hoard", "hood", "whod", "hud", "heard"),
  f1=c(273, 386, 527, 751, 655, 552, 452, 397, 291, 623, 527),
  f2=c(2289, 2038, 1801, 1558, 1044, 986, 793, 1550, 1672, 1370, 1528))

sse.lob.df <- lobanov(sse.df)
sse.lob.df$ipa <- ipa[as.character(sse.lob.df$vowel)]

sse.lob.df <- rbind(
    ddply(sse.lob.df, names(sse.lob.df), function(x) data.frame(group='LV')),
    ddply(sse.lob.df, names(sse.lob.df), function(x) data.frame(group='HV')))

sse.lob.df$angle <- 0
sse.lob.df$dist <- 0.25

vwl.df <- rbind(
	read.csv("vowels-pre-LV.csv"),
	read.csv("vowels-post-LV.csv"),
	read.csv("vowels-pre-HV.csv"),
	read.csv("vowels-post-HV.csv"))
vwl.df <- vwl.df[vwl.df$vowel %in% monophthongs,]
vwl.df$speaker <- sapply(vwl.df$participant, FUN=function(x) sub("(C[0-9]+).*?([LH]V)", "\\1\\2", x))



lob.df <- lobanov(vwl.df, f1="f1.50", f2="f2.50", group=c("group", "test", "speaker"))
write.csv(lob.df, "vowels-lob.csv", row.names=FALSE, quote=FALSE)



lob.mn.df <- ddply(lob.df, c("group", "test", "vowel"), function(subset) {
	data.frame(f1=mean(subset$f1), f2=mean(subset$f2))
})

# Do plot

# Set up fonts
font_add("Cabin", "../fonts/Cabin/Cabin-Regular.ttf")
font_add("DejaVuSans", "../fonts/dejavu-fonts-ttf-2.37/ttf/DejaVuSans.ttf")
showtext_auto()


df.melt <- melt(lob.mn.df,  id.var = c("vowel", "group", "test"))
df <- dcast(df.melt, group+vowel~variable+test)

# Roughtly Position labels
df$angle <- apply(df, 1, FUN=function(d) {
	atan2(as.numeric(d["f1_post"]) - as.numeric(d["f1_pre"]),
		  as.numeric(d["f2_post"]) - as.numeric(d["f2_pre"])) / pi * 180 })
df$angle <- floor(df$angle / 90) * 90 + 90
df$angle[df$vowel == "hud" & df$group == "LV"] <- 180
df$angle[df$vowel == "hard" & df$group == "LV"] <- 45
df$angle[df$vowel == "hoard" & df$group == "LV"] <- 135

df$angle[df$vowel == "heed" & df$group == "HV"] <- 45
df$angle[df$vowel == "hud" & df$group == "HV"] <- 45
df$angle[df$vowel == "hard" & df$group == "HV"] <- 0
df$angle[df$vowel == "heard" & df$group == "HV"] <- 180

sse.lob.df$angle[sse.lob.df$group == "HV"] <- 90
s <- 0.1875
df$lab.x <- df$f2_pre - cos(df$angle / 180 * pi) * s
df$lab.y <- df$f1_pre - sin(df$angle / 180 * pi) * s
df$ipa <- ipa[as.character(df$vowel)]

# Define colors
colors <- {}
colors$pre <-  "#F8BBD0"
colors$post <- "#E91E63"
colors$arrow <- "#444444"
colors$ipa <- "#176FC1" # "#0288D1"
colors$sse <- "#cccccc"
colors$panel.background <- "#eeeeee"
colors$panel.grid <- "#ffffff"

dpi = 1200
fontSize <- dpi * 80 / 600
rad <- function(a) a / 180 * pi
width = 7
height = 4.5

# Start plot and set some theme stuff
p <- ggplot(data=) + theme(
	text=element_text(family="Cabin", size=fontSize),
	panel.background=element_rect(fill=colors$panel.background),
	panel.grid.major=element_line(
		color=colors$panel.grid,
		linetype="13",
		lineend="round"),
	axis.ticks=element_blank(),
	legend.position="bottom",
	legend.key=element_rect(
	  fill="transparent",
	  colour="transparent"))
# Reverse scales
p <- p + scale_y_reverse(
	expand=c(0.02,0.02),
	minor_breaks=c(),
	breaks=seq(-2,2,1),
	labels=seq(-2,2,1))
p <- p + scale_x_reverse(
	expand=c(0.02,0.02),
	minor_breaks=c(),
	breaks=seq(-2,2,1),
	labels=seq(-2,2,1))
# Add limits here to prevent filtering of data
p <- p + coord_cartesian(xlim=c(2,-2), ylim=c(2, -2))
# Add SSBE points...
p <- p + geom_point(
	data=sse.lob.df,
	aes(x=f2, y=f1, color='SSBE'),
	size=3)
# ...and SSBE labels
p <- p + geom_text(
	data=sse.lob.df,
	aes(x=f2 - cos(rad(angle))*dist, y=f1 - sin(rad(angle))*dist, label=ipa),
	color=colors$sse,
	family="DejaVuSans",
	size=fontSize*0.4)
# Draw lines between pre and post
p <- p + geom_segment(
	data=df,
	aes(x=f2_pre, xend=f2_post, y=f1_pre, yend=f1_post),
	# arrow=arrow(type="closed", length=unit(0.075, "in")),
	inherit.aes=FALSE,
	colour=colors$arrow)
# Draw pre and post points
p <- p + geom_point(
	data=lob.mn.df,
	aes(x=f2, y=f1, color=test),
	size=3)
p <- p + scale_fill_manual(
  values=c(colors$pre, colors$post, colors$sse),
  name="test")
p <- p + scale_color_manual(
  values=c(colors$pre, colors$post, colors$sse),
  name="test")
p <- p + geom_text(
	data=df,
	aes(x=lab.x, y=lab.y, label=ipa),
	colour=colors$ipa,
	family="DejaVuSans",
	size=fontSize*0.4)
p <- p +  ylab("F1 (Lobanov)")
p <- p +  xlab("F2 (Lobanov)")
p <- p + facet_grid(.~group)

options(repr.plot.width=width, repr.plot.height=height)
ggsave("vowels-plot.png", width=width, height=height, units="in", dpi=dpi)

