function [s,d,t,U,V] = gensvd(X,M,W)
    %%%repmat(1:10,10,1)' * ([1:10;1:10]') * repmat(1:2,2,1)

    newX = repmat(sqrt(M),size(X,2),1)' .* X .* repmat(sqrt(W),size(X,1),1);
    [U,s,V] = svd(newX,'econ');
    s = diag(s);
    
    keepem = find(s.^2 > eps);
    U = repmat(1./sqrt(M),length(keepem),1)' .* U(:,keepem);
    V = repmat(1./sqrt(W),length(keepem),1)' .* V(:,keepem);
    s = s(keepem);
    
    d = s.^2;
    t = d/sum(d);

end