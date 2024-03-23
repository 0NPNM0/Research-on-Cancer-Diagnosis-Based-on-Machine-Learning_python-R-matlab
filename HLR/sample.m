clear
clc

%Requirement: Matlab version >= 2013b



%%%%%%%%%%% generate benchmark dataset %%%%%%%%%%%%%%%%%%%%%%%%

        train_size=100;
        beta=zeros(2000,1);
        beta(1)=2;
        beta(2)=2;
        beta(3)=2;
        beta(4)=2;
        beta(5)=2;

        sample_size=train_size;
        x1 = normrnd(0, 1, sample_size, size(beta,1));
        [n,p]=size(x1);
        cor=0.9;   %correlation
        for i=2:5
            x1(:,i) = cor*(x1(:,1))+ (1-cor)*(x1(:,i));
        end

        l=(x1*beta+0.3*normrnd(0, 1, n, 1));
        prob=exp(l)./(1 + exp(l));
        prob(find(prob>=0.5)) =1;
        prob(find(prob<0.5))=0;
        y1=prob;

%%%%%%%%%%% HLX with logistic regression %%%%%%%%%%%%%%%%%%%%%%%%
         %alpha=[0.01:0.01:1];
       [beta_path,info] = HLR_Regularization(x1,y1,'binomial','NumLambda',100,'Alpha',0.25);
