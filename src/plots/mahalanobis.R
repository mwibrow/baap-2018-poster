
source(file.path("R", "settings.R"))

fmt.df <- rbind(
  read.csv(file.path(dataDir, "pre-LV.csv")),
  read.csv(file.path(dataDir, "post-LV.csv")),
  read.csv(file.path(dataDir, "pre-HV.csv")),
  read.csv(file.path(dataDir, "post-HV.csv")))

fmt.df$speaker <- apply(fmt.df, 1, function(d) {
  paste(c(
  sub("(C[0-9]+).*", "\\1", d['participant']),
      d['group']), collapse='-')
})

lob.df <- lobanov(fmt.df, group=c('group', 'test', 'speaker'))
lob.df <- subset(lob.df, vowel %in% monophthongs)

out.df <- ddply(lob.df, c("group", "test", "vowel"), function(d){
  f1.z <- scale(d$f1, center=TRUE, scale=TRUE)
  f2.z <- scale(d$f2, center=TRUE, scale=TRUE)
  f1.mad <- median(abs(d$f1 - median(d$f1)))
  f2.mad <- median(abs(d$f2 - median(d$f2)))
  f1.mad.dev <- abs(d$f1 - median(d$f1)) / f1.mad
  f2.mad.dev <- abs(d$f2 - median(d$f2)) / f1.mad
  cbind(d,
    data.frame(
    f1.z,
    f2.z,
    f1.z.out=abs(f1.z) > 2,
    f2.z.out=abs(f2.z) > 2,
    f1.mad.out=f1.mad.dev > 3,
    f2.mad.out=f2.mad.dev > 3
    )
  )
})
in.df <- subset(
  out.df,
  !f1.z.out & !f2.z.out,
  c("group", "test", "vowel", "speaker", "f1", "f2"))


hm.df <- read.csv(file.path(dataDir, "hawkins_midgely_2005.csv"))

hm.lob.df <- lobanov(hm.df, group=c("speaker"))
hm.lob.df$group <- substr(hm.lob.df$speaker, 1, 2)
hm.lob.mn <- ddply(hm.lob.df, c("vowel"), function(subset) {
  data.frame(f1=mean(subset$f1), f2=mean(subset$f2))
})


cov2d <- function(x, y) {
    cov.xy <- cov(x, y)
    return (matrix(c(var(x), cov.xy, cov.xy, var(y)), ncol=2, nrow=2))
}

mahal.dist <- function(x, y) {
    mn.x <- rowMeans(x)
    mn.y <- rowMeans(y)
    sgm <- cov2d(y[1,], y[2,])
    mat <- t(sgm) %*% sgm
    mhl <- sqrt(t(mn.x - mn.y) %*% solve(mat) %*% (mn.x - mn.y))
    mhl <- mahalanobis(t(mn.x), t(mn.y), mat)
    return (mhl)
}


mahalanobisFile <- file.path(inDir, "mahalanobis.csv")

if (file.exists(mahalanobisFile)) {

cnf.df <- read.csv(mahalanobisFile)

} else {
cnf.df <- adply(in.df,1, function(d) {
    vowel <- as.character(d$vowel)
    cls.df <- ddply(subset(hm.lob.df, group=="S4"), c("vowel"), function(e) {
        mal <- mahal.dist(matrix(rbind(d$f1,d$f2), nrow=2),
            matrix(rbind(e$f1, e$f2), nrow=2))
        data.frame(
            vowel=vowel,
            cls=unique(e$vowel), mal)
    })
    data.frame(vowel.class=cls.df$cls, mahalanobis.distance=cls.df$mal, correct=cls.df$mal == min(cls.df$mal))
})
write.csv(cnf.df, file.path(inDir, "mahalanobis.csv"), row.names=FALSE, quote=FALSE)
}


cnf.df$vowel <- factor(cnf.df$vowel, levels=monophthongs)
cnf.df$vowel.class <- with(cnf.df,
    factor(vowel.class, levels=levels(vowel)))

crt.cnf.df <- ddply(subset(cnf.df, correct==TRUE), c("group", "test", "speaker", "vowel"), function(d) {
    data.frame(correct=as.character(d$vowel)==as.character(d$vowel.class))
})

acc.cnf.df <- ddply(crt.cnf.df, c("group", "test", "speaker"), function(d) {
    data.frame(accuracy=sum(d$correct) / length(d$correct))
})

font_add("Cabin", "../fonts/Cabin/Cabin-Regular.ttf")
font_add("Cabin-Italic", "../fonts/Cabin/Cabin-Italic.ttf")
font_add("DejaVuSans", "../fonts/dejavu-fonts-ttf-2.37/ttf/DejaVuSans.ttf")

showtext_auto()


width <- 4
height <- 5
options(repr.plot.width=width, repr.plot.height=height)



acc.cnf.df$test <- relevel(factor(acc.cnf.df$test), ref="pre")
acc.cnf.df$group <- factor(acc.cnf.df$group, levels=c("LV", "HV"))

dpi <- DPI

showtext_opts(dpi=dpi)
fontSize <- 12
width <- 4
height <- 2.75
options(repr.plot.width=width, repr.plot.height=height)

p <- ggplot() + theme(
    legend.position="none",
    legend.key=element_rect(
      fill="transparent",
      colour="transparent"),
    panel.background=element_rect(fill="#eeeeee"),
    panel.grid.major=element_line(color="#ffffff", linetype="13", lineend="round"),
    axis.ticks = element_blank(),
    text=element_text(family="Cabin", size=fontSize),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) +
    xlab("Test") +
    ylab("SSBE class accuracy")

p <- p + geom_boxplot(data=acc.cnf.df, aes(x=test, y=accuracy, color=test, fill=test))
p <- p + scale_y_continuous(
  expand=c(0.02,0.02),
  minor_breaks=c(),
  breaks=seq(0,0.8,.2),
  labels=seq(0,0.8,.2))

# Add limits here to prevent filtering of data
p <- p + coord_cartesian(ylim=c(0,.8))
p <- p + scale_color_manual(
  values=c(pre=colors$pre, post=colors$post),
  name="Test")
p <- p + scale_fill_manual(
  values=c(pre=colors$pre, post=colors$post),
  name="Test")
p <- p + facet_grid(.~group)

dat <- ggplot_build(p)$data[[1]]
dat$test <- rep(levels(acc.cnf.df$test), times=nrow(dat) / 2)
dat$group <- rep(levels(acc.cnf.df$group), each=nrow(dat) / 2)

dat$group <- factor(dat$group, levels=c("LV", "HV"))
p <- p + geom_segment(
  data=dat,
  aes(x=xmin, xend=xmax, y=middle, yend=middle), lineend="square", inherit.aes=FALSE, colour="white")


ggsave(file.path(outDir, "mahalanobis-plot.png"), width=width, height=height, units="in", dpi=dpi)


