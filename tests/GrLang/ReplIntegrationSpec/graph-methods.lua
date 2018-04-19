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

E = Graph [[]]
X = Graph [[ a b c : N ; a-:E->b ]] 

print('is_empty', E:is_empty(), X:is_empty())
print('identity', X:identity())

X = Graph [[ a b : N; a-f:E-> b]]
Y = Graph [[ c d e : N; c-g:E-> d; d -h:E-> e]]
print('product', with_dom(X:product(Y)))
print('disjoint_union', with_cod(X:disjoint_union(Y)))

print()
print('subgraphs')
for f in X:subgraphs() do
  print(with_dom(f))
end

print()
print('quotients')
for f in Y:quotients() do
  print(with_cod(f))
end

print()
print('morphisms_to')
for f in X:morphisms_to(Y) do
  print(f)
end

X = Graph [[ a : N; a-:E-> a]]
Y = Graph [[ b c: N; b-:E->b; c-:E->c]]
print()
print('overlappings_with(all)')
for f,g in X:overlappings_with(Y) do
  print(with_cod(f,g))
end

print()
print('overlappings_with(monic)')
for f,g in X:overlappings_with(Y, 'monic') do
  print(with_cod(f,g))
end
