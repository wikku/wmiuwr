using Polynomials
using Printf


function B(n,k)
    polyroots = filter(i -> i != k, 0:n)
    antid = integrate(fromroots(polyroots))
    int = antid(n) - antid(0)
    return 1/n * (-1)^(n-k) * 1/(factorial(k)*factorial(n-k)) * int
end

for n = 1:10
    @printf "%2d:  " n
    for k = 0:n
        @printf "%.4f, " B(n,k)
    end
    println()
end

σ(n) = sum(map(k -> abs(B(n,k)), 0:n))
println("σ(10) = $(σ(10))")
println("σ(15) = $(σ(15))")
println("σ(20) = $(σ(20))")

# vim: et
