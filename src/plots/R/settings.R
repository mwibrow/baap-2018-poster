list.of.packages <- c("ggplot2", "showtext", "reshape2", "plyr", "stringr")

ensure.packages <- function(packages=list.of.packages) {
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  for (package in packages) {
    library(package, character.only=TRUE)
  }
}

ensure.packages()

bvd_raw = "
beat   i:   ɪ   bit
poot   u:  ʊ    put
bot    ɒ   ɑ:   bart
bot    ɒ   ʌ    but
bert   ɜ:  ɑ:   bart
but    ʌ   ɑ:   bart
bird   ɜ:  eə   bared
boat   əʊ   aʊ  bout
bait   eɪ  e    bet
bait   eɪ  aɪ   bite
bite   aɪ  ɪ    bit
bat    æ   ʌ    but
beard  ɪə  eə   bared
buoyed  ɔɪ  ɔ:   board
"
matches <- as.data.frame(
  str_match_all(bvd_raw, "([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)")[[1]])
bvd <- {}
invisible(apply(matches, 1, FUN=function(row){
  bvd[row["V2"]] <<- row["V3"]
  bvd[row["V5"]] <<- row["V4"]
}))


hvd_raw <- "heed /i:/ hid /ɪ/ head /e/ heard /ɜ:/ had /æ/ hud /ʌ/ hard /ɑ:/ hod /ɒ/ hoard /ɔ:/ whod /u:/ hood /ʊ/"
matches <- as.data.frame(
  str_match_all(hvd_raw, "([a-z]+)\\s+/([^/]{1,2})/")[[1]])

hvd <- {}
monophthongs <- c()
invisible(apply(matches, 1, FUN=function(row){
  hvd[row["V2"]] <<- row["V3"]
  monophthongs <<- c(monophthongs, row["V2"])
}))

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

# Set up fonts
font_add("Cabin", "../fonts/Cabin/Cabin-Regular.ttf")
font_add("Cabin-Italic", "../fonts/Cabin/Cabin-Italic.ttf")
font_add("DejaVuSans", "../fonts/dejavu-fonts-ttf-2.37/ttf/DejaVuSans.ttf")
showtext_auto()


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
        f1.vwl.nrm <- median(f1.vwl.nrm)
        f2.vwl.nrm <- median(f2.vwl.nrm)
      }
      data.frame(f1=f1.vwl.nrm, f2=f2.vwl.nrm)
    })
  })
}

colors <- new.env()
with(colors, {
  pre <-  "#F8BBD0"
  post <- "#E91E63"
  arrow <- "#444444"
  hvd <- "#176FC1" # "#0288D1"
  ssbe <- "#cccccc"
  ssbe.label <- "#cccccc"
  panel.background <- "#eeeeee"
  panel.grid <- "#ffffff"
})


outDir <- "images"
inDir <- "data"
dataDir <- inDir

DPI <- as.numeric(Sys.getenv("DPI"))
if (is.na(DPI)) {
  DPI <- 300
}
cat(sprintf("Using %d dpi\n", DPI))
