function [P, Tp, mn, sigma]=Psurprise(r, N, gamma, prob)
%PSURPRISE--probability of a surprise given a trend and climatology period
%
% [P, mn, sigma]=Psurprise(r, N, gamma, prob)
%
% Probability that sample in period N+1 is a "surprise". By
% this definition, T(N+1) is a surprise if Prob{T(N+1)}<(1-prob) using the
% statistics (mean and pooled variance) of the period 1:N.
%
% Input:
%  r (units/period) is the trend in the mean
%  N (# periods) is the number of periods
%  gamma = standard deviation in any one period (assumed fixed)
%  prob = significance level (i.e. 0.95 means T(N) has a 5% of being a
%         heatwave)
%
%Output:
%  P = probability of getting a heatwave with the current trend and
%      statistics
% Tp = threshold temperature based on the reference period
% mn = mean of T over the N periods
% sigma = standard deviation over the reference period
%
% Andrew Pershing (apershing@gmri.org), 2018

if(length(r)~=length(gamma))
    error('r and gamma must be the same length');
end
mn=r*(N-1)/2;

%The variance assuming stationarity (sigma^2) involves both the interannual 
% variance (gamma^2) and the trend. The weighting of these two terms
% depends on the number of years (N).
%
% 
coefsf1=[-1/2
1];
A=coefsf1(1)/N+coefsf1(2);

coefsN=[1/12
1/12
-7/12];
B=[N^2,N,1]*coefsN;

sigma=A*gamma.^2+B*r.^2;
sigma=sqrt(sigma);


Tp=zeros(length(r),1);
P=Tp;
for j=1:length(r);
    Tp(j)=norminv(prob,mn(j),sigma(j));
    P(j)=1-normcdf(Tp(j),r(j)*N,gamma(j));
end
