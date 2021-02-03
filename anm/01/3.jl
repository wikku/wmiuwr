x0 = 4.71
wx0 = -14.636489
relerr_wx0(x) = abs((x-wx0)/wx0)
w1(x) = x^3 - 6x^2 + 3x - typeof(x)(0.149)
w2(x) = ((x - 6)x + 3)x - typeof(x)(0.149)

for func in [w1, w2]
    for prec in [Float16, Float32, Float64]
        val = func(prec(x0))
        println("$func($prec($x0))) = $val\t relative error $(relerr_wx0(val)))")
    end
end
