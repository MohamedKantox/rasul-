waveletSmooth =function(x,wavelet="haar",level=1,DecLvl=2,title=None){
  #Calculate the wavelet coefficients
  waveletTransform = dwt(x,wavelet,n.levels=DecLvl)
  coeff=waveletTransform@W[[-level]]
  coeff_2 = waveletTransform@W[[(-level-1)]]
  #Calculate a threshold
  sigma = mad(coeff)
  sigma_2 = mad(coeff_2)
  #On peut jouer sur le threshold
  uthresh = sigma*sqrt(2*log(length(x)))
  uthresh_2 = sigma*sqrt(2*log(length(x)))
  #Applying threshold : Type is "soft"
  threshold = function(value,value_2){
    for(i in 1:length(coeff)){
      #Super affectation because coeff is outside the for loop
      if(coeff[[i]]!=0) coeff[[i]] <<- (coeff[[i]]/abs(coeff[[i]])) * max(abs(coeff[[i]])-value,0)
    }
    for(i in 1:length(coeff_2)){
      #Super affectation because coeff is outside the for loop
      if(coeff_2[[i]]!=0) coeff_2[[i]] <<- (coeff_2[[i]]/abs(coeff_2[[i]])) * max(abs(coeff_2[[i]])-value,0)
      
    }
  }
  threshold(uthresh,uthresh_2)
  waveletTransform@W[[-level]] = coeff
  waveletTransform@W[[(-level-1)]] = coeff_2
  smoothedColumn = idwt(waveletTransform)
  return(smoothedColumn)
}