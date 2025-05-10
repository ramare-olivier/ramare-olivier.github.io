{
f(N,k)=
  local(res,t);
  res=-(log(N))^(k+1)/(k+1);
  for(t=2,N,res+=(log(t))^k/t);
  print(N,"   ",res)
}
