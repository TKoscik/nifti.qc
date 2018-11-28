nii.qc.descriptive <- function(
  img.nii, img.vol=1L,
  mask.nii, mask.vol=1L, mask.dir="gt", mask.thresh=0) {

  img <- read.nii.volume(img.nii, img.vol)

  mask <- read.nii.volume(mask.nii, mask.vol)
  mask <- switch(mask.dir,
                 `gt`=(mask > mask.thresh) * 1,
                 `ge`=(mask >= mask.thresh) * 1,
                 `lt`=(mask < mask.thresh) * 1,
                 `le`=(mask <= mask.thresh) * 1,
                 `eq`=(mask == mask.thresh) * 1)
  mask <- which(mask==1, arr.ind = TRUE)

  img <- img[mask]

  summary <- list()
  summary$mean <- mean(img, na.rm=TRUE)
  summary$sd <- sd(img, na.rm=TRUE)
  summary$median <- median(img, na.rm=TRUE)
  summary$mad <- mad(img, na.rm=TRUE)
  summary$skew <- skewness(img, na.rm=TRUE)
  summary$kurtosis <- kurtosis(img, na.rm=TRUE)
  summary$q05 <- quantile(img, 0.05, na.rm=TRUE)
  summary$q95 <- quantile(img, 0.95, na.rm=TRUE)
  df <- data.frame(Intensity=img)
  summary$plot <- ggplot(df, aes(x=Intensity)) +
    theme_bw() +
    geom_density(size=1, fill="#646464", color="transparent") +
    labs(x="Intensity", y="Density") +
    geom_vline(xintercept=summary$mean, linetype="dotted", size=1) +
    annotate(geom = "rect",
             xmin=summary$mean - summary$sd,
             xmax=summary$mean + summary$sd,
             ymin=-Inf, ymax=Inf, alpha=0.15, color="transparent") +
    annotate(geom = "rect", xmin=summary$q05, xmax=summary$q95,
             ymin=-Inf, ymax=Inf, alpha=0.15, color="transparent")

}
