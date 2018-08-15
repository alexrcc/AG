x<-sample(1:10,10);
p<-x[c(1:5)];
m<-x[c(6:10)];
N=30
auxsuma=0;
auxproducto=0;
promsuma=0;
promproducto=0;
auxsuma2=0;
auxproducto2=0;
promsuma2=0;
promproducto2=0;

r<-AG(p,m,N);

colnames(r)<-c("gP1","gP2","gP3","gP4","gP5","gM1","gM2","gM3","gM4","gM5","Suma(gP)","Producto(gM)","Suma(gM)","Producto(gP)","OPTIMO")
print(r)
for(i in 1:N){
    auxsuma=auxsuma+r[i,11];
    auxsuma2=auxsuma2+r[i,13];
}
promsuma=auxsuma/N;
promsuma2=auxsuma2/N;

if(promsuma>=promsuma2){
  print(promsuma);
}else{
  print(promsuma2);
}

