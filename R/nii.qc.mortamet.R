nii.qc.mortamet <- function(
  img.nii, img.vol=1L,
  air.nii, air.vol=1L, air.dir="eq", air.thresh=1,
  art.nii, art.vol=1L, art.dir="eq", art.thresh=1) {

  # load data ------------------------------
  img <- read.nii.volume(img.nii, img.vol)

  air <- read.nii.volume(air.nii, air.vol)
  air <- thresh.apply(air, air.dir, air.thresh, "binary")

  art <- read.nii.volume(art.nii, art.vol)
  art <- thresh.apply(art, art.dir, art.thresh, "binary")

  # calculate qi1 ------------------
  qi1 <- sum(art == 1) / sum(air == 1)

  # calculate qi2 ------------------
  ## remove artefact mask from air mask
  no.art <- ((air - art) > 0)*1

  ## get data in clean air mask
  data <- img[which(no.art==1)]
  data <- data[data > 0]

  ## subsample data for speed
  if (length(data) > 3e5) {
    modelx <- sample(data, 3e5)
  } else {
    modelx <- data
  }

  x_grid <- seq(0, quantile(data,0.99), length.out = 1000)

  ## estimate observed pdf
  kde <- KernSec(x=modelx,
                 xgridsize=100,
                 xbandwidth = 0.05 * quantile(data, 0.98),
                 range.x = x_grid)
  thresh <- max(which(kde$yden > (max(kde$yden)/2)))

  ## Fit Chi^2 distribution
  modelx <- as.numeric(modelx)
  fit.chi <- fitdist(modelx[modelx < quantile(modelx, 0.95)],
                     distr="chisq", method= "mle",
                     start=list(df=32, ncp=0.1), lower=c(0,0))
  exp.pdf <- dchisq(x_grid, fit.chi$estimate[1], fit.chi$estimate[2])

  ## calculate goodness of fit
  gof <- sum(abs(kde$yden[1:thresh]-exp.pdf[1:thresh]))/length(data)

  ## calculate qi2
  qi2 <- qi1 + gof

  # send out Mortamet at al's Quality Indices ----
  out <- list()
  out$qi1 <- qi1
  out$qi2 <- qi2

  out$kde <- kde
  out$fit.chi <- fit.chi
  out$obs.noise <- data.frame(data)
  out$exp.dist <- data.frame(x=x_grid, exp.pdf)

  if (x_grid[thresh] < qchisq(0.95, fit.chi$estimate[1], fit.chi$estimate[2])) {
    qi.color <- "#00a800"
  } else {
    qi.color <- "#a80000"
  }
  out$plot <- ggplot(out$obs.noise, aes(x=data)) + theme_bw() +
    geom_density(size=1) +
    labs(title="Noise Distributions of Air",
         subtitle="Observed (black), Expected (color)",
         x="Intensity", y="Density") +
    coord_cartesian(xlim=c(0,quantile(out$obs.noise$data,0.99))) +
    geom_line(inherit.aes=FALSE,
              data=out$exp.dist,
              aes(x=x, y=exp.pdf),
              color=qi.color,
              size=1) +
    geom_vline(xintercept = x_grid[thresh], linetype="dashed", size=1) +
    geom_vline(xintercept =qchisq(0.95, fit.chi$estimate[1], fit.chi$estimate[2]),
               linetype="dashed", color=qi.color, size=1)

  return(out)
}
