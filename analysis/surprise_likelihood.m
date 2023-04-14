function [like,mn,freqdist]=surprise_likelihood(Ps,nobs,ntrials)
%SURPRISE_LIKELIHOOD--computes likelihood of getting surprises
%
% [like,mn,freqdist]=surprise_likelihood(Ps,nobs,ntrials)
%
% Ps = m-by-1 vector of probabilities for m regions
% nobs = scalar with number of observed surprises
% ntrials = number of trials (default is 1000)
%
% like = likelihood of getting nobs given Ps
% mn = mean number of surprises
% freqdist = m+1 array of number of surprises
%
% Andrew Pershing (apershing@gmri.org), 2018

m=length(Ps);
if(nobs>m)
    error('can"t have more surprises than regions');
end

if(nargin<3)
    ntrials =1000;
end

S=rand(m,ntrials);%random numbers

for j=1:m;
    S(j,:)=S(j,:)<Ps(j);
end

N=sum(S);%number of surprises

bins=0:m;
freqdist=hist(N,bins);
freqdist=freqdist/sum(freqdist);
mn=mean(N);
like=freqdist(nobs+1);

freqdist=freqdist(:);


