GrLang.add_node_type('N')
GrLang.add_edge_type('E', 'N', 'N')

function with_dom(...)
  if select('#', ...) > 0 then
    local fst = ...
    return fst:dom(), ...
  else
    return ...
  end
end

function with_cod(...)
  if select('#', ...) > 0 then
    local fst = ...
    return fst:cod(), ...
  else
    return ...
  end
end

X = Graph [[ a b c : N ]] 
Y = Graph [[ e f g h i : N ]]

f = Morphism(X, Y)[[a->e; b->f; c->g]]
print('dom', f:dom() == X)
print('cod', f:cod() == Y)
print('is_monic,epic,iso', f:is_monic(), f:is_epic(), f:is_iso())

Z = Graph [[ae bfg c h i: N]]
f, g = Morphism(X,Z)[[a->ae; b->bfg; c->c]], Morphism(Y,Z)[[e->ae; f g ->bfg; h->h; i->i]]
print('pullback', with_dom(f:pullback(g)))

W = Graph [[ae bf : N]]
f, g = Morphism(W,X)[[ae->a; bf->b]], Morphism(W,Y)[[ae->e; bf->f]]
print('pushout', with_cod(f:pushout(g)))

f, g = Morphism(X,Y)[[a->e; b->f; c->g]], Morphism(X,Y)[[a->e; b->f; c->h]]
print('equalize_with', with_dom(f:equalize_with(g)))
print('coequalize_with', with_cod(f:coequalize_with(g)))

f = Morphism(X, Y)[[a->e; b->f; c->h]]
g = Y:identity()
print('has_pushout_complement', f:has_pushout_complement(g))
print('pushout_complement', with_dom(f:pushout_complement(g)))

g = Morphism(Y, Y)[[e->e; f->f; g h->g; i->i]]
print('has_pushout_complement', f:has_pushout_complement(g))
print('pushout_complement', with_dom(f:pushout_complement(g)))

g = Morphism(Y, Y)[[e->e; f->f; g ->g; h i->h]]
b,gg,c = g:initial_pushout()
print('initial_pushout', b:dom(), b, c:dom(), c, gg)

W = Graph [[ac : N]]
X = Graph [[a b: N]]
Y = Graph [[c d : N]]
f, g = Morphism(W,X)[[ac->a]], Morphism(W,Y)[[ac->c]]
print()
print('commutative_overlappings_with(all)')
for h,k in f:commutative_overlappings_with(g) do
  print(with_cod(h, k))
end

print()
print('commutative_overlappings_with(monic)')
for h,k in f:commutative_overlappings_with(g, 'monic') do
  print(with_cod(h, k))
end