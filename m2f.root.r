m2f.root<-function(x, M0, V, CsumQ, N)
{
	# auxiliary function to estimate root of catch equation when
	# trying to find combined mortality rate from predation (M2) and fishing (F)
	#
	# George Watters
	# code last edited 20 May 2005
	#
	Z <- M0 + x + V
	CsumQ - (x/Z * (1 - exp( - Z)) * N)
}
