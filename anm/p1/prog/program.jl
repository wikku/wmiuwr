using Printf
using Base.MathConstants

const indices = 0:180
const invfacts = map(Float64 ∘ inv ∘ factorial ∘ big, indices)
const ln2 = log(Float64(2.0))
const reduced_domain = range(-ln2/2, ln2/2, length=1024)

"Returns (m,u) such that e^x = 2^m*e^u, |u| < 0.5ln2"
function reducerange(x)
    z = x/ln2
    m = round(z)
    w = z - m
    u = w*ln2
    return (m, u)
end


"Returns e^x by summing Taylor series up to last term smaller than ε"
naiveexp(ε) = x -> begin
    result = 0
    (xpow, k) = (1, 0)
    while (term = xpow*invfacts[k+1]; abs(term) >= ε)
        result += term
        (xpow, k) = (xpow*x, k+1)
    end
    return result
end

"Like naiveexp but sums in reverse"
rnaiveexp(ε) = x -> begin
    result = []
    (xpow, k) = (1, 0)
    while (term = xpow*invfacts[k+1]; abs(term) >= ε)
        push!(result, term)
        (xpow, k) = (xpow*x, k+1)
    end
    #println(k-1)
    return foldr(+, result)
end

"Calculates e^x using Horner's method for Taylor series truncated to term n"
hornerexp(n) = x -> begin
    result = invfacts[n+1]
    for k in n-1:-1:0
        result = result * x + invfacts[k+1]
    end
    return result
end

calcexp(f) = x -> begin
    (m, u) = reducerange(x)
    2^m*f(u)
end


abserr(approx) = x -> abs(approx(x) - exp(x))
relerr(approx) = x -> let y = big(exp(x)); abs(approx(x)-y)/y end
acc(x) = -log10(max(x, 1e-16))

function print_table()
    for (x, fx) in arr_tsin_relerrs
        @printf("%2.4f & %2.4f \\\\ \n", x, -log10(fx))
    end
end
# vim: et sw=4 ts=4

