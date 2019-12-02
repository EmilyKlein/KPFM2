relative.mc <- function(numerator.mc, denominator.mc){
  #
  # George Watters
  # code last edited 6 November 2007
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
  out.list$N$pengs <- numerator.mc$N$pengs/denominator.mc$N$pengs
  out.list$N$pengs[is.nan(out.list$N$pengs)] <- NA
  out.list$N$seals <- numerator.mc$N$seals/denominator.mc$N$seals
  out.list$N$seals[is.nan(out.list$N$seals)] <- NA
  out.list$N$whales <- numerator.mc$N$whales/denominator.mc$N$whales
  out.list$N$whales[is.nan(out.list$N$whales)] <- NA
  out.list$N$fish <- numerator.mc$N$fish/denominator.mc$N$fish
  out.list$N$fish[is.nan(out.list$N$fish)] <- NA
  #
  # now do the recruitment arrays
  # if there was no spawning/breeding in time t then both numerator and denominator are 0
  # this evaluates to NaN and we reset to NA so that it will work in plotting functions etc.
  out.list$R$R$krill <- numerator.mc$R$R$krill/denominator.mc$R$R$krill
  out.list$R$R$krill[is.nan(out.list$R$R$krill)]<-NA
  out.list$R$R$pengs <- numerator.mc$R$R$pengs/denominator.mc$R$R$pengs
  out.list$R$R$pengs[is.nan(out.list$R$R$pengs)]<-NA
  out.list$R$R$seals <- numerator.mc$R$R$seals/denominator.mc$R$R$seals
  out.list$R$R$seals[is.nan(out.list$R$R$seals)]<-NA
  out.list$R$R$whales <- numerator.mc$R$R$whales/denominator.mc$R$R$whales
  out.list$R$R$whales[is.nan(out.list$R$R$whales)]<-NA
  out.list$R$R$fish <- numerator.mc$R$R$fish/denominator.mc$R$R$fish
  out.list$R$R$fish[is.nan(out.list$R$R$fish)]<-NA
  #
  # now do the demand arrays
  out.list$Q$demand$pengs <- numerator.mc$Q$demand$pengs/denominator.mc$Q$demand$pengs
  out.list$Q$demand$seals <- numerator.mc$Q$demand$seals/denominator.mc$Q$demand$seals
  out.list$Q$demand$whales <- numerator.mc$Q$demand$whales/denominator.mc$Q$demand$whales
  out.list$Q$demand$fish <- numerator.mc$Q$demand$fish/denominator.mc$Q$demand$fish
  # now do the consumption arrays
  out.list$Q$consumption$pengs <- numerator.mc$Q$consumption$pengs/denominator.mc$Q$consumption$pengs
  out.list$Q$consumption$seals <- numerator.mc$Q$consumption$seals/denominator.mc$Q$consumption$seals
  out.list$Q$consumption$whales <- numerator.mc$Q$consumption$whales/denominator.mc$Q$consumption$whales
  out.list$Q$consumption$fish <- numerator.mc$Q$consumption$fish/denominator.mc$Q$consumption$fish
  #
  out.list
}
