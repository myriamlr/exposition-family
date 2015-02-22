function [] = coreCA(X)

    colSums = sum(X);
    rowSums = sum(X,2);
    grandSum = sum(colSums);
    Or = X/grandSum;
    m = rowSums/grandSum;
    w = (colSums/grandSum)’;
    
    Ex = wx'*wy;
    Zx = Ox - Ex;

    M = 1./wx;
    W = 1./wy;    

    [s,d,t,Fi,Fj,U,V] = gensvd(Zx,M,W);
    
    Fi = repmat(M,length(keepem),1)' .* U .* repmat(s',size(U,1),1);
    Fj = repmat(W,length(keepem),1)' .* V .* repmat(s',size(V,1),1);
    
end