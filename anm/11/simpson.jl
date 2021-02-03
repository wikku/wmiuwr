using Base.MathConstants

simpson(y,h) = h/3*(4*sum(y) - 2*sum(y[1:2:end]) - y[1] - y[end])
simpsonerr(h,maxd4f) = h^4/180*maxd4f

f(x) = (cos(x))^2*e^(-x)

run(b,n) = simpson(f.(0:1/n:b), 1/n) # n even
