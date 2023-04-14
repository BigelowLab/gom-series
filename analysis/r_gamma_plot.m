function varargout=r_gamma_plot(R, Gamma,N, points);
%R_GAMMA_PLOT--plot data ellipses in r (trend) and variance (gamma) space
%
% h=r_gamma_plot(R, Gamma,N, {points});
%
% R = m-by-n array of trends.  Each column should be a time 
% Gamma = m-by-n array of variances (std. deviations)
% N = number of years in climatology
%
% points = optional flag to plot the data points in addition to the
% ellipses
%
% Andrew Pershing (apershing@gmri.org), 2018

clf;

[m,n]=size(R);
if(size(Gamma,1)~=m | size(Gamma,2)~=n)
    error('R and Gamma should be the same size');
end
if(nargin<4)
    points=0;
end
if(nargin<3)
    N=30;
end

rsp=span(R);
ssp=span(Gamma);

r=linspace(-0.05, 0.1)';
gamma=linspace(0.001,1,99);

for j=1:length(gamma)
    [Phw(:,j),t1,t2,t3]=Psurprise(r,N,repmat(gamma(j),length(r),1),0.977);
end

%h=pcolor(r,gamma,Phw');
%[c,h]=contour(r,gamma,Phw',0.02:0.02:0.2);%contours so we can work in Illustrator
map=flipud(gray(12));
for j=1:9;
    c=contourc(r,gamma,Phw',[0.02 0.02]*j);
    %c=[c,[10;10]];%add a point
    FILL=0;
    if(FILL)
        if(j<9)
            h(j)=fill([10,c(1,2:end)],[10,c(2,2:end)],'r');hold on;
        else
            len=size(c,2);
            h(j)=fill([10,c(1,2:min(len,214))],[10,c(2,2:min(len,214))],'r');%not stable but this one is flakey
        end
        set(h(j),'FaceColor',map(j,:));
    else
        %plot as lines
        if(j<9)
            h(j)=plot([10,c(1,2:end)],[10,c(2,2:end)],'r');hold on;
        else
            len=size(c,2);
            h(j)=plot([10,c(1,2:min(len,214))],[10,c(2,2:min(len,214))],'r');%not stable but this one is flakey
        end
        set(h(j),'Color',map(j,:));
    end
end
axis([-0.05 0.1 0 1]);
xlabel('Temperature Trend');
ylabel('Temperature Variability');
caxis([0 0.18])
colormap(map(1:end-3,:))

colorbar
hold on

col=parula(max(n,2));
for j=1:n
    tmp=R(:,j)+Gamma(:,j);
    I=find(~isnan(tmp) & ~isinf(tmp) & ~imag(tmp));
    [eX,eY]=error_ellipse(R(I,j),Gamma(I,j),0.75);
    g(j)=plot(eX,eY,'-','color',col(j,:));
    g3(j)=plotmrks(mean(eX),mean(eY),col(j,:),'p');
    set(g3(j),'MarkerSize',12);
    if(points)
        g2(j)=plotmrks(R(:,j),Gamma(:,j));
        set(g2(j),'MarkerFaceColor',col(j,:),'MarkerSize',4,'MarkerEdgeColor','none');
    end
end
h=[h(:);g(:)];
if(points)
    h=[h;g2(:);g3(:)];
end
if(nargout>0)
    varargout{1}=h;
end

