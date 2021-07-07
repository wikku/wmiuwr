#!/usr/bin/env julia
using Mods
import Base: (*), (setindex!), (convert), (size), (getindex)

global MODULO

struct ModMat <: AbstractArray{Int64,2}
	val :: Array{Int64,2}
end


function *(A::ModMat, B::ModMat) :: ModMat
	#print("works!")
	res = A.val * B.val
	map!(x -> x % MODULO, res, res)
	return ModMat(res)
end

function setindex!(A::ModMat, v::Int64, i::Int64, j::Int64)
	A.val[i,j] = v
end

function getindex(A::ModMat, i::Int64, j::Int64)
	getindex(A.val, i,j)
end

#convert(::Type{Union{}}, x::ModMat) = x

size(x::ModMat) = size(x.val)

makeM(n) = ModMat([ Int64(mod(x,2^n) == floor(y/2^n)) for x=0:2^2n-1, y=0:2^2n-1 ])
global M



# dp[k][m] — liczba kolorowań długości k, które kończą się na maskę m
# dp[k+1] = Md[k]
# M[i+1][j+1] —
#    czy maski j,i są kompatybilne
#    tzn. wspólna kolumna oraz bez zakazanego wzorca

# maska
# .05
# .16
# .27
# .38
# .49

testMat = [
	0 5
	1 6
	2 7
	3 8
	4 9
]
function matrixtomask(mat)
	mat = transpose(mat)
	mat = vcat(mat[1,:], mat[2,:])
	mat = transpose(mat)
	mat = Array(mat)
	bits = map(string, mat)
	bits = reverse(join(bits))
	return parse(Int, bits, base=2)
end
function removeend!(c)
	m1 = matrixtomask(c[:,1:2])
	m2 = matrixtomask(c[:,2:3])
	#println("deleting ", m1, ",", m2)
	M[m2+1,m1+1] = 0
end

rows = [[0 0 0], [0 0 1], [0 1 0], [0 1 1], [1 0 0], [1 0 1], [1 1 0], [1 1 1]]

function updateM!(pat)
	for r1 in rows
		for r2 in rows
		removeend!(vcat(r1, r2, pat))
		removeend!(vcat(r1, pat, r2))
		removeend!(vcat(pat, r1, r2))
		end
	end
end

b(c) = if c == '.' 0 else 1 end

function strtorow(s)
	[b(s[1]) b(s[2]) b(s[3])]
end

function main()
	(n, p, m) = [parse(Int, x) for x in split(readline())]
	#global M = makeM(5, m)
	global MODULO = m
	global M = makeM(5)
	for _ in 1:p
		r1 = strtorow(readline())
		r2 = strtorow(readline())
		r3 = strtorow(readline())
		pat = vcat(r1, r2, r3)
		#print(pat)
		updateM!(pat)
	end
	#display(M.val)
	#@time Mpow = M^(n-2)
	Mpow = M^(n-2)
	#M = Mpow.val
	#display(M)
	println(sum(Mpow) % m)
end

main()
