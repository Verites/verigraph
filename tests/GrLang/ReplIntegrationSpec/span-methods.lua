GrLang.add_node_type('N')
GrLang.add_edge_type('E', 'N', 'N')

X = Graph [[ a b c : N ]] 
Y = Graph [[ d e : N ]]
Z = Graph [[ ad ae be: N]]
f, g = Morphism(Z,X)[[ad ae->a; be->b]], Morphism(Z,Y)[[ad->d; ae be->e]]

s = Span(f,g)

print('span')
print(s)
print()

print('dot')
print(s:to_dot())

L1 = Graph [[ a b : N ]]
L2 = Graph [[ c d : N ]]

A, B, C, AB = Graph [[ ac : N ]], Graph [[ bd : N ]], Graph [[ bc : N ]], Graph [[ ac bd : N ]]
a = Span(Morphism(A,L1)[[ac -> a]], Morphism(A,L2)[[ac -> c]])
b = Span(Morphism(B,L1)[[bd -> b]], Morphism(B,L2)[[bd -> d]])
c = Span(Morphism(C,L1)[[bc -> b]], Morphism(C,L2)[[bc -> c]])
ab = Span(Morphism(AB,L1)[[ac -> a; bd -> b]], Morphism(AB,L2)[[ac -> c; bd -> d]])

print()
print('subobject_intersection')
print(Span.subobject_intersection(a,b))
print(Span.subobject_intersection(a,ab))

print()
print('subobject_union')
print(Span.subobject_union(a,b))
print(Span.subobject_union(a,c))
