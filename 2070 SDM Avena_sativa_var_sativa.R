#!/usr/bin/env Rscript
options(java.parameters = "-Xmx64g")
options(repos = c(CRAN = "https://cloud.r-project.org"))
library(dismo)
library(raster)
library(maptools)
library(rJava)
library(viridis)

#— paths —#
input_pts_dir   <- "/home/suhailak/kantar_koastore/suhaila/cover_crop_project/Inputs/cc_sdm"
bioclim_dir     <- "/home/suhailak/kantar_koastore/anna/SDM/CMIP6_2061_2080_ssp585"
out_sdm_dir     <- "/home/suhailak/kantar_koastore/suhaila/cover_crop_project/Outputs/SDMs"
out_var_dir     <- "/home/suhailak/kantar_koastore/suhaila/cover_crop_project/Outputs/SDMs New/Variable Contributions"
out_auc_dir     <- "/home/suhailak/kantar_koastore/suhaila/cover_crop_project/Outputs/SDMs New/AUCs"

dir.create(out_sdm_dir, recursive=TRUE, showWarnings=FALSE)
dir.create(out_var_dir, recursive=TRUE, showWarnings=FALSE)
dir.create(out_auc_dir, recursive=TRUE, showWarnings=FALSE)

# Load world map for plotting
data(wrld_simpl)

# Load all bioclim predictors
bio_files <- list.files(bioclim_dir, pattern="tif$", full.names=TRUE)
predictors <- stack(bio_files)
if ("biome" %in% names(predictors)) predictors <- dropLayer(predictors, "biome")

# Color ramp for plotting
cols <- viridis(100)

# Species to run
sp <- "Avena_sativa_var_sativa"
f <- file.path(input_pts_dir, paste0(sp, ".csv"))

# Output updates
cat("=== Processing", sp, "===\n")
start_time <- Sys.time()

# Read & clean occurrences
df <- read.csv(f, header=TRUE, stringsAsFactors=FALSE)
df <- df[complete.cases(df$longitude, df$latitude), c("longitude", "latitude")]
df <- df[!duplicated(df), ]

# Split presence into train/test
kf_p <- kfold(df, 3)
pres_train <- df[kf_p != 1, ]
pres_test  <- df[kf_p == 1, ]

# Background points
backg_all <- randomPoints(predictors, 1000, extf=1.25)
colnames(backg_all) <- c("longitude", "latitude")
kf_b <- kfold(backg_all, 3)
backg_train <- backg_all[kf_b != 1, ]
backg_test  <- backg_all[kf_b == 1, ]

# Fit Maxent
maxent_jar <- system.file("java/maxent.jar", package="dismo")
if (!file.exists(maxent_jar)) {
  stop("Maxent JAR not found in dismo package. Install it first.")
}
xm <- maxent(predictors, pres_train)

# Evaluate model
ev <- evaluate(pres_test, backg_test, xm, predictors)
auc_val <- ev@auc

# Predict to raster
px <- predict(predictors, xm, progress='')

# Write raw raster
out_ras <- file.path(out_sdm_dir, paste0(sp, "Future2070.tif"))
writeRaster(px, out_ras, overwrite=TRUE, options=c('TFW=YES'))

#— Map SDM (high-res + grid + Antarctica colored, ocean white) —#
png(
  filename = file.path(out_sdm_dir, paste0(sp, "_map.png")),
  width    = 1600,
  height   = 1000,
  res      = 300,
  bg       = "white"
)
par(mar = c(4, 4, 3, 8))

# 1) Plot base map with transparent land outlines
plot(
  wrld_simpl,
  col    = "transparent",
  border = "lightgrey",
  bg     = "white",
  xlab   = "Longitude",
  ylab   = "Latitude",
  main   = "Future 2061-2080"
)

# 2) Add a light grey grid (adjust nx/ny for more/less spacing)
grid(
  nx  = 10,
  ny  = 5,
  col = "lightgrey",
  lwd = 0.5
)

# 3) Plot raster (with NA as white = ocean will stay white)
plot(
  px,
  add    = TRUE,
  col    = cols,
  legend = FALSE,
  colNA  = "white"
)

# 4) Manually fill Antarctica using the first viridis color
plot(
  wrld_simpl[wrld_simpl$NAME=="Antarctica", ],
  add    = TRUE,
  col    = cols[1],
  border = NA
)

# 5) Draw land outlines again on top
plot(
  wrld_simpl,
  add    = TRUE,
  border = "lightgrey",
  lwd    = 0.5
)

# 6) Add legend
plot(
  px,
  legend.only     = TRUE,
  col             = cols,
  legend.width    = 1.5,
  legend.shrink   = 0.6,
  axis.args       = list(
    at     = seq(0, 1, by = 0.25),
    labels = seq(0, 1, by = 0.25),
    cex.axis = 0.8
  ),
  legend.args     = list(
    text = "Habitat suitability",
    side = 4,
    font = 2,
    line = 2.5,
    cex  = 0.9
  )
)

dev.off()

# Variable contribution
png(file.path(out_var_dir, paste0(sp, "_varcontrib.png")),
    width=700, height=600, bg="white")
plot(xm, main=paste0(sp, " variable contributions"), cex.main=1.2)
dev.off()

# ROC plot
png(file.path(out_auc_dir, paste0(sp, "_AUC.png")),
    width=700, height=600, bg="white")
plot(ev, 'ROC',
     main=paste0(sp, " ROC (AUC = ", round(auc_val, 3), ")"),
     cex.main=1.2)
dev.off()

# Time report
end_time <- Sys.time()
cat("Time taken for", sp, ":", round(difftime(end_time, start_time, units="mins"), 2), "minutes\n\n")
