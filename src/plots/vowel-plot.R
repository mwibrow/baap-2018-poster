list.of.packages <- c("ggplot2", "showtext", "reshape2", "plyr", "stringr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (package in list.of.packages) {
  library(package, character.only=TRUE)
}

dataDir <- "data"

rad <- function(a) a / 180 * pi

lobanov <- function(df, f1="f1", f2="f2", vowel="vowel", group=c(), reduce=TRUE) {
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


ssbe.df <- read.csv(file.path(dataDir, "ssbe.csv"))
ssbe.df <- subset(ssbe.df, vowel %in% monophthongs)
ssbe.df <- data.frame(
  vowel=c("heed", "hid", "head", "had", "hard", "hod", "hoard", "hood", "whod", "hud", "heard"),
  f1=c(273, 386, 527, 751, 655, 552, 452, 397, 291, 623, 527),
  f2=c(2289, 2038, 1801, 1558, 1044, 986, 793, 1550, 1672, 1370, 1528))

ssbe.lob.df <- lobanov(ssbe.df)
ssbe.lob.df$label <- ipa[as.character(ssbe.lob.df$vowel)]

ssbe.lob.df <- rbind(
    ddply(ssbe.lob.df, names(ssbe.lob.df), function(x) data.frame(group="LV")),
    ddply(ssbe.lob.df, names(ssbe.lob.df), function(x) data.frame(group="HV")))

ssbe.lob.df$angle <- 0
ssbe.lob.df$dist <- 0.25

lob.file <- file.path(dataDir, "lobanov.csv")
if (!file.exists(lob.file)) {
  cat("Calculating Lobanov formats.\n")
  vwl.df <- rbind(
    read.csv(file.path(dataDir, "pre-LV.csv")),
    read.csv(file.path(dataDir, "post-LV.csv")),
    read.csv(file.path(dataDir, "pre-HV.csv")),
    read.csv(file.path(dataDir, "post-HV.csv")))
  vwl.df <- vwl.df[vwl.df$vowel %in% monophthongs,]
  vwl.df$speaker <- sapply(vwl.df$participant, FUN=function(x) sub("(C[0-9]+).*?([LH]V)", "\\1\\2", x))



  lob.df <- lobanov(vwl.df, f1="f1", f2="f2", group=c("group", "test", "speaker"))
  write.csv(lob.df, file.path(dataDir, "lobanov-calc.csv"), row.names=FALSE, quote=FALSE)
} else {
  cat("Reading precalculated Lobanov formats\n")
  lob.df <- read.csv(lob.file)
  lob.df$test <- as.factor(lob.df$test)
  lob.df$test <- relevel(lob.df$test, ref="pre")
}

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
df$ipa <- ipa[as.character(df$vowel)]


lab.df <- with(df, data.frame(label=ipa, group, f1=f1_pre, f2=f2_pre))




label.transform <- function(
  df, x, y,
  label="label",
  transforms=NULL,
  groups=c(),
  default=list(angle=0, r=0.25)) {
  return(ddply(df, c(groups, label), function(d) {
    group <- as.character(d$group)
    label <- as.character(d$label)
    if (is.null(transforms)) {
      pos = NULL
    } else {
      pos <- transforms[[group]][[label]]
    }
    if (is.null(pos)) {
      pos <- default
    } else {
      if (is.numeric(pos)) {
        pos <- list(angle=pos, r=default$r)
      }
    }
    data.frame(
        x=d[,x] - cos(rad(pos$angle)) * pos$r,
        y=d[,y] - sin(rad(pos$angle)) * pos$r)
  }))
}

dict <- function(content=NULL) {
  dct <- new.env(hash=TRUE)
  if (!is.null(content)) {
    expressions <- parse(text = deparse(substitute(content)))[[1]]
    for (i in seq(2, length(expressions))) {
      expression <- expressions[[i]]
      name <- as.character(expression[[2]])
      value <- expression[[3]]
      dct[[name]] <- eval(value)
    }
  }
  return (dct)
}

transforms <- dict({
  LV <- dict({
    "ɒ" <- 180
    "i:" <- 0
    "ʊ" <- 180
    "ɪ" <- 180
    "ɜ:" <- 180
    "ʌ" <- 180
    "æ" <- 180
    "e" <- 180
    "ɔ:" <- 90
    "ɑ:" <- 270
  })
  HV <- dict({
    "i:" <- 90
    "æ" <- 180
    "ɪ" <- 270
    "ɜ:" <- 180
    "e" <- 90
    "ɑ:" <- 0
    "ʌ" <- 45
    "u:" <- 90
    "ɒ" <- 270
  })
})

transforms.ssbe <- dict({
  LV <- dict({
    "ɒ" <- 180
    "i:" <- 0
    "ʊ" <- 180
    "ɪ" <- 270
    "ɜ:" <- 180
    "ʌ" <- 0
    "æ" <- 180
    "e" <- 180
    "ɔ:" <- 90
  })
  HV <- dict({
    "i:" <- 90
    "æ" <- 180
    "ɪ" <- 180
    "ɜ:" <- 180
    "e" <- 180
    "ɑ:" <- 0
    "ʌ" <- 270
    "u:" <- 90
    "ɒ" <- 270
  })
})

lab.df <- label.transform(
  lab.df, x="f2", y="f1",
  groups=c("group"),
  transforms=transforms,
  default=list(angle=0, r=0.2))

ssbe.lab.df <- ssbe.lob.df[c("label", "group", "f1", "f2")]
ssbe.lab.df <- label.transform(
  ssbe.lab.df, x="f2", y="f1",
  groups=c("group"),
  transforms=transforms.ssbe,
  default=list(angle=90, r=0.19))

# Define colors
colors <- {}
colors <- new.env()
with(colors, {
  pre <-  "#F8BBD0"
  post <- "#E91E63"
  arrow <- "#444444"
  ipa <- "#176FC1" # "#0288D1"
  ssbe <- "#cccccc"
  ssbe.label <- "#cccccc"
  panel.background <- "#eeeeee"
  panel.grid <- "#ffffff"
})

dpi = 1200

if (Sys.info()["sysname"] == "Darwin") {
  fontSize <- 12
} else {
  fontSize <- 12
}

showtext_opts(dpi=dpi)
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
    colour="transparent"),
  axis.title.x = element_text(margin = margin(t = 10, r = 0, b = -10, l = 0)))
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
  data=ssbe.lob.df,
  aes(x=f2, y=f1, fill="SSBE"),
  color=colors$ssbe,
  size=3)
# ...and SSBE labels
p <- p + geom_text(
  data=ssbe.lab.df,
  aes(x, y, label=label),
  color=colors$ssbe.label,
  family="DejaVuSans",
  vjust=0.4,
  size=fontSize*0.333)
# Add an extra legend entry
p <- p + scale_fill_manual("", breaks = "SSBE", values=colors$ssbe)

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
p <- p + scale_color_manual(
  values=c(colors$pre, colors$post),
  name="Test")

# Add labels to pre points
p <- p + geom_text(
  data=lab.df,
  aes(x, y, label=label),
  colour=colors$ipa,
  family="DejaVuSans",
  vjust=0.4,
  size=fontSize*0.4)

p <- p +  ylab("F1 (Lobanov)")
p <- p +  xlab("F2 (Lobanov)")
p <- p + facet_grid(.~group)

options(repr.plot.width=width, repr.plot.height=height)
ggsave("vowel-plot.png", width=width, height=height, units="in", dpi=dpi)


