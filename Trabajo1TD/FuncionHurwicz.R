criterio.Hurwicz.General2 = function(tablaX,alfa=0.3,favorable=TRUE) {
  # si alfa es un escalar entre 0 y 1 lo obtiene para ese único valor
  # si alfa es igual a un número mayor que 1, lo usa para obtener cálculos para dividir el rango 0-1
  X = tablaX;
  if (favorable) {
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    if (alfa<=1) {
      valfa = c(alfa);
    } else {
      valfa = seq(from=0,to=1,by=(1/alfa)); ## alfa: 100, 200,
    }
    vHurwicz = rep(0,length(valfa))
    Alt_vHurwicz = rep(0,length(valfa))
    ##
    alfab = valfa[1];
    vAltH = alfab * Altmax + (1-alfab) * Altmin;
    vHurwicz[1] = max(vAltH);
    Alt_vHurwicz[1] = which.max(vAltH);
    Alt_vHurwicz_g = which.max.general(vAltH);
    
    alfasCambio=c(alfab);
    alternativas=c(which.max(vAltH));
    
    for (i in 2:length(valfa)) {
      alfab = valfa[i];
      vAltH = alfab * Altmax + (1-alfab) * Altmin;
      vHurwicz[i] = max(vAltH);
      Alt_vHurwicz[i] = which.max(vAltH);
      Alt_vHurwicz_g = which.max.general(vAltH);
      
      if (Alt_vHurwicz[i]!=Alt_vHurwicz[i-1]) {
        alfasCambio=c(alfasCambio,alfab);
        alternativas=c(alternativas, which.max(vAltH));
      }
    }
    metodo = 'favorable';
  } else {
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    if (alfa<=1) {
      valfa = c(alfa);
    } else {
      valfa = seq(from=0,to=1,by=(1/alfa)); ## alfa: 100, 200,
    }
    vHurwicz = rep(0,length(valfa))
    Alt_vHurwicz = rep(0,length(valfa))
    
    alfab = valfa[1];
    vAltH = (1-alfab) * Altmax + alfab * Altmin;
    vHurwicz[1] = min(vAltH);
    Alt_vHurwicz[1] = which.min(vAltH);
    Alt_vHurwicz_g = which.min.general(vAltH);
    
    alfasCambio=c(alfab);
    alternativas=c(which.min(vAltH));
    
    for (i in 2:length(valfa)) {
      alfab = valfa[i];
      vAltH = (1-alfab) * Altmax + alfab * Altmin;
      vHurwicz[i] = min(vAltH);
      Alt_vHurwicz[i] = which.min(vAltH);
      Alt_vHurwicz_g = which.min.general(vAltH);
      if (Alt_vHurwicz[i]!=Alt_vHurwicz[i-1]) {
        alfasCambio=c(alfasCambio,alfab);
        alternativas=c(alternativas, which.min(vAltH));
      }
      
    }
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Hurwicz';
  resultados$alfa = alfa;
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorAlternativas = vAltH;
  resultados$ValorOptimo = vHurwicz;
  
  resultados$alternativasPorIntervalos = alternativas;
  resultados$alfas = alfasCambio;
  
  if (length(valfa)==1) {
    resultados$AlternativaOptima = Alt_vHurwicz_g;
  } else {
    resultados$AlternativaOptima = Alt_vHurwicz;
  }
  
  return(resultados);
  
  
  
}
