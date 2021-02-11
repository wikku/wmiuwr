using Printf
using Test
using Plots

"""
Calculates ½c₁T₀(x) + c₂T₁(x) + ... + cₙTₙ₋₁(x), where
Tᵢ are Chebyshev polynomials of the first kind.
"""
function clenshaw(x, c)
    n = length(c)
    (b1, b2, b3) = (0, 0, 0)
    for i = n:-1:1
        (b1, b2, b3) = (2*x*b1 - b2 + c[i], b1, b2)
    end
    (b1 - b3)/2
end

function _test_clenshaw()
    @testset "clenshaw" begin
        @test clenshaw(0, [2]) == 1
        @test clenshaw(0, [2, 1]) == 1
        @test clenshaw(0, []) == 0
        @test clenshaw(1, [2, 1]) == 2
    end
end


function halvelast(a)
    c = copy(a)
    c[end] /= 2
    return c
end

"""
Find an antiderivative of ∑' fᵢ Tᵢ
"""
function integrate(f)
    N = length(f)
    F = zero([f; [0]])
    for i = 2:N+1
        F[i] = (get(f,i-1,0) - get(f,i+1,0)) / (2i-2)
    end
    return F
end

function _test_integrate()
    @testset "integrate" begin
        @test integrate(Array{Float64}([])) == [0.0]
        @test integrate([2.0]) == [0.0, 1.0]
        @test integrate([0.0,2.0]) == [0.0, 0.0, 0.5]
        # ∫ 2x dx = x^2/2 + C= 0.5T_2 + C
    end
end

function adjustconstant!(F, η, ξ)
    @assert ξ == -1 || ξ == 1
    @assert F[1] == 0
    N = length(F)
    Tofξ = map(k -> ξ^k, 0:N-1)
    v = sum(map(*, F, Tofξ))
    F[1] = 2(η - v)
end

"""
Compute one Picard iteration for the ODE y' = f(x,y), y(ξ) = η

x ranges from -1 to 1, ξ ∈ { -1, 1 }
y is represented as a Chebyshev series ∑' cᵢ Tᵢ
Returns coefficients of a Chebyshev series
"""
function iteration(c, f, η, ξ)
    @assert ξ == -1 || ξ == 1
    #N = max(length(c) - 1, 1)
    N = length(c)
    x = map(s -> cos(π*s/N), 0:N)
    y = map(x -> clenshaw(x, c), x)
    rhs = map(f, x, y)
    C = halvelast(rhs * 2 / N)
    A′ = halvelast(map(x -> clenshaw(x, C), x)) # rhs ≈ ∑' A′ᵢ Tᵢ
    A = integrate(A′)
    adjustconstant!(A, η, ξ)
    return A
end

function clenshawnorton(f, η, ξ, iters)
    c = [2η, 0]
    seq = [c]
    for i in 1:iters
        c = iteration(c, f, η, ξ)
        if i == 1
            c = c[1:end-1]
        end
        push!(seq, c)
    end
    return seq
end

xs = (-1:0.01:1)
function computechebyshev(c, xs=xs)
    f = x -> clenshaw(x, c)
    f.(xs)
end

defaultlabels(ns=(0:1000)) = map(n -> "y$n", collect(transpose(ns)))

function myplot(cs, legend=:best, labels=defaultlabels())
    plot(
        xs,
        map(computechebyshev, cs),
        labels=labels,
        xlabel = "x",
        ylabel = "y",
        legend=legend
    )
end


function _test()
    _test_clenshaw()
    _test_integrate()
end


# vim: et sw=4 ts=4
