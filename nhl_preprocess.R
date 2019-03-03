## Reading the data and getting the desired features
inputPath <- file.choose()
dfRaw <- read.csv(inputPath, header=TRUE, stringsAsFactors=FALSE)
cols <- c("Rk", "Tm", "GP", "G","A", "X...", "PIM", "S",
            "ATOI", "BLK", "HIT", "FOL", "FOW", "Pos")
df <- dfRaw[, cols]
colnames(df)[6] <- "PM"

## Unify Pos (position) feature to LW, C, RW and D only
df$Pos <- gsub("/.*", "", df$Pos)

## Replacing NAs in BLK with 0
df$BLK[is.na(df$BLK)] <- 0

## Convert ATOI (average time on ice) to seconds
time2sec <- function(x) {
  x <- as.numeric(unlist(strsplit(x, ":")))
  return(x[1] * 60 + x[2])
}

df$ATOI <- as.numeric(lapply(df$ATOI, FUN=time2sec))

## Calculating the total stats of players with multiple teams
df <- subset(df, df$Tm != "TOT")
df <- within(df, rm("Tm", "Pos"))
df <- aggregate(. ~ Rk, df, FUN = sum)

## Compute FO (face-off-%) from FOW (face-off wins) and FOL (face-off losses)
df$FO <- round(df$FOW / (df$FOW + df$FOL) * 100, digits = 1)
df$FO[is.nan(df$FO)] <- 0
df <- within(df, rm("FOW", "FOL"))

## Give each player back their position
df$Pos <- ""
for (i in 1:nrow(df)) {
  for (j in 1:nrow(dfRaw)) {
  if (df$Rk[i] == dfRaw$Rk[j])
  df$Pos[i] <- dfRaw$Pos[j]
  }
}

## Combine LW and RW to just wingers (W)
df$Pos[df$Pos == "RW" | df$Pos == "LW"] <- "W"

## Removing players with GP (games played) less than 20
df <- subset(df, df$GP >= 20)

## Removing unnecessary variables
df <- within(df, rm("Rk", "GP"))

## Writing data to csv
filename <- "nhlseason1718_processed.csv"
outputPath <- paste(dirname(inputPath), filename, sep="/") 
write.csv(df, file=outputPath, row.names = FALSE)
