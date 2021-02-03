using Random

function parsefloat64bits(s)
    sgn = if (s[1] == '0') 1 else -1 end
    expt = parse(Int, s[2:12], base=2) - 1023
    mantissa = parse(Int, string("1", s[13:64]), base=2) / (2^52)
    return sgn * mantissa * (2.0^expt)
end

function test()
    r = randexp(Float64)
    if issubnormal(r) return end
    @assert r == parsefloat64bits(bitstring(r))
end

for _ in 1:100000 test() end
