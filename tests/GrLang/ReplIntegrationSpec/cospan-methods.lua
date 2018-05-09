GrLang.add_node_type('N')
GrLang.add_edge_type('E', 'N', 'N')

X = Graph [[ a b c : N ]] 
Y = Graph [[ e f g h i : N ]]
Z = Graph [[ae bfg chi: N]]
f, g = Morphism(X,Z)[[a->ae; b->bfg; c->chi]], Morphism(Y,Z)[[e->ae; f g ->bfg; h->chi; i->chi]]
c = Cospan(f,g)

print('cospan')
print(c)

print()
print('to_dot')
print(c:to_dot())

function assert(val)
  if not val then
    error("Assertion failed!", 2)
  end
end

print()
print('commuters')
for h in Cospan(f,g):commuters() do
  print(h)
  assert(h:dom() == f:dom())
  assert(h:cod() == g:dom())
end