# we wzorze mamy sin, po nazwach s_k, c_k domyślamy się, że chodzi o sin i cos

# dla 0 <= x <= pi/2 mamy
# sin(x/2) = sqrt((1-cos x)/2), cos(x/2) = sqrt((1+cos x)/2)
# co tłumaczy rekurencje i początkowe wyrazy ciągów

bf = BigFloat
bfpi = bf(pi)

sines = []
cosines = []

# algorytm z treści
function P(n)
    (s, c, p) = (bf(1.0), bf(0.0), bf(2.0))
    results = []
    for k = 2:n
        push!(results, p*s)
        (s, c, p) = (sqrt(0.5(1-c)), sqrt(0.5(1+c)), 2*p)
        #push!(sines, (s, sin(2*bf(pi)/p), sinpi(bf(2)/p)))
        push!(cosines, (c, cos(2*bf(pi)/p), cospi(bf(2)/p)))
    end
    results
end

# algorytm z sinpi
function P2(n)
    results = []
    for k = 2:n
        p = bf(2)^(k-1)
        push!(results, p*sinpi(1/p))
    end
    results
end

errpidigits(x) = -floor(log10(abs(bfpi-x)))

function gettable()
    prec = precision(bf)
    collect(zip(2:prec, map(correctpidigits, P(prec))))
end

# precision(bf) to liczba bitów znaczących

# źródłem kłopotów jest kumulacja błędów w obliczonych wartościach sin i cos.
# można ich uniknąć używając funkcji sin/sinpi.

# cosinusy będą blisko 1, gdzie mamy mniejszą precyzję niż blisko 0.

# sin(x/2) = sqrt(0.5(1-cos(x))) = sqrt(0.5(1-sqrt(1-sin(x)^2)))
#          = sqrt(0.5(1-(sqrt(1-sin(x))*sqrt(1+sin(x)))))

# taylor dla sqrt(1/2(1-sqrt(1-x^2)))
f(x) = 715x^9/65536 + 33x^7/2048 + 7x^5/256 + x^3/16 + x/2

# iterowana aproksymacja sin(x/2) taylorem
function P3(n)
    (s, p) = (bf(1.0), bf(2.0))
    results = []
    for k = 2:n
        push!(results, p*s)
        (s, p) = (f(s), 2*p)
    end
    results
end

function P4(n)
    (s, p) = (bf(1.0), bf(2.0))
    results = []
    for k = 2:n
        push!(results, p*s)
        (s, p) = (f(s), 2*p)
    end
    results
end
