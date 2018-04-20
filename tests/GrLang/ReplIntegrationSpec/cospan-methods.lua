GrLang.add_node_type('N')
GrLang.add_edge_type('E', 'N', 'N')

X = Graph [[ a b c : N ]] 
Y = Graph [[ e f g h i : N ]]
Z = Graph [[ae bfg chi: N]]
f, g = Morphism(X,Z)[[a->ae; b->bfg; c->chi]], Morphism(Y,Z)[[e->ae; f g ->bfg; h->chi; i->chi]]

print('commuters')
for h in Cospan(f,g):commuters() do
  print(h)
end