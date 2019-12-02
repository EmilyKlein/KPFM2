relative.regional.krill.mc <- function(numerator.mc, denominator.mc){
  #
  # George Watters
  # code last edited 17 June 2008
  #
  # this function is designed to make one MC object relative to another via
  # standard division (hence the argument names)
  #
  # I imagine this could be used for a variety of purposes, but, initially, the goal here
  # is to provide a mechanism by which performance measures for predators (and
  # also krill I guess) developed from runs in which fishing is simulated
  # can be referenced to predicted trajectories from runs in which fishing was
  # not simulated -- this is useful for getting appropriate reference points
  # in situations like simulated climate-change hypotheses
  #
  # make the out.list the same as numerator.mc and then we'll write over stuff as necessary
  out.list <- numerator.mc
  #
  # intent is to use the following flag to ensure the user does not merge relative and "unrelative"
  # mc objects with merge.mc() at some later time
  out.list$setup$relative <- TRUE
  # also keep track of what the numerator and denominator were
  out.list$numerator <- numerator.mc$call
  out.list$denominator <- denominator.mc$call
  #
  # first make the abundance arrays relative
  # if there are no predators in the SSMU then you have both the numerator and denominator = 0
  # so gonna set the ratio equal to NA
  out.list$N$krill <- numerator.mc$N$krill/denominator.mc$N$krill
  out.list$N$krill[is.nan(out.list$N$krill)] <- NA
  #
  #
  out.list
}
