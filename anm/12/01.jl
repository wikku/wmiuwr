# ∬_D sin² y sin² x (1+x²+y²)^-1/2 dx dy
# = ∫_{-1}^3 sin² x ∫_{-c(x)}^{c(x)} sin² y (1+x²+y²)^-1/2 dy dx

function trapezoidal(a, b, n, f)
    h = (b-a)/n
    if h == 0 return 0 end
    xs = (a:h:b)
    h/2 * (f(a) + 2*sum(f.(xs[2:end-1])) + f(b))
end

N1 = 1000
N2 = 4000

c(x) = if x < sqrt(3)/2 sqrt(1-x^2) else 0.5 end
φ(x) = trapezoidal(-c(x), c(x), N1, y -> (sin(y))^2*(1+x^2+y^2)^(-1/2))
I = trapezoidal(-1, 3, N2, x -> (sin(x))^2*φ(x))

println(I)

# vim: et
