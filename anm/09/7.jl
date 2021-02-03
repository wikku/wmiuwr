f(x) = log(1 + x + x^2)

samples = range(-1, 1, step=0.001)

d(a0, a, a2, c, c2, e) = x -> f(x) - a0*(x+0.5)^5 -  a*(x+0.5)^4 - a2*(x+0.5)^3 - c*(x+0.5)^2 - c2*(x+0.5) - e

# -0.18, 1, 0, -0.27
#
function iter(v)
    signchanges = 0
    last = v[1]
    maxd = -999
    mind = 999
    imax = -999
    for i = 2:length(v)
        y = v[i]
        maxd = max(abs(y), maxd)
        imax = max(abs(y), imax)
        if sign(y) == -sign(last)
            signchanges += 1
            mind = min(imax, mind)
            imax = -999
        end
        last = y
    end
    (signchanges, mind, maxd)
end

function brute()
    a0 = rand(-0.001:0.00001:0.001)
    a = rand(-0.19:0.0001:-0.17)
    a2 = rand(-0.04:0.0001:0.04)
    c = rand(0.98:0.0001:1.02)
    c2 = rand(-0.02:0.0001:0.02)
    e = rand(-0.27:0.0001:-0.25)
    (s, mind, maxd) = iter(d(a0,a,a2,c,c2,e).(samples))
    if s > 2 && maxd < 0.03
        println((a0, a, a2, c, c2, e), (s, mind, maxd))
    end
end

function bruteall()
    while true
        brute()
    end
end
# vim: et
