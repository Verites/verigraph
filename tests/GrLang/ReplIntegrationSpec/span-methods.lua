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
