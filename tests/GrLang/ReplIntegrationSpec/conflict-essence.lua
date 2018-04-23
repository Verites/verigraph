function proto_essence(l1, m1, m2)
  p1, p2 = m2:pullback(m1)
  _, q2 = l1:pullback(p1)
  _, _, c = q2:initial_pushout()
  return p1 .. c, p2 .. c
end

function disabling_essence(rule1, m1, m2)
  return proto_essence(rule1:l(), m1, m2)
end

function conflict_essence(rule1, rule2, m1, m2)
  p1, p2 = m2:pullback(m1)
  _, q12 = rule1:l():pullback(p1)
  _, q21 = rule2:l():pullback(p2)
  _, _, c1 = q12:initial_pushout()
  _, _, c2 = q21:initial_pushout()
  c = Morphism.subobject_union(c1, c2)
  return p1 .. c, p2 .. c
end

function print_span(p1, p2)
  print(p1:dom(), p1, p2)
end

GrLang.add_node_type('N')
GrLang.add_edge_type('E', 'N', 'N')

rule1 = Rule [[
  match { n1 n2 : N }
  delete n1
]]

rule2 = rule1

G = Graph [[ n1 n2 : N ]]
m1 = Morphism(rule1:lhs(), G) [[ n1 -> n1; n2 -> n2 ]]
m2 = Morphism(rule2:lhs(), G) [[ n1 -> n1; n2 -> n2 ]]

print_span(disabling_essence(rule1, m1, m2))
print_span(disabling_essence(rule2, m2, m1))
print_span(conflict_essence(rule1, rule2, m1, m2))

m2 = Morphism(rule2:lhs(), G) [[ n1 -> n2; n2 -> n1 ]]

print_span(disabling_essence(rule1, m1, m2))
print_span(disabling_essence(rule2, m2, m1))
print_span(conflict_essence(rule1, rule2, m1, m2))

print()
print('All morphisms')
for f in rule1:lhs():morphisms_to(G) do
  print(f)
end

print()
print('Monomorphisms')
for f in rule1:lhs():morphisms_to(G, 'monic') do
  print(f)
end

print()
print('Subgraphs')
for f in rule1:lhs():subgraphs() do
  print(f:dom(), f)
end 

print()
print('Quotients')
for f in rule1:lhs():quotients() do
  print(f, f:cod())
end 

print()
print('Disjoint Union')
j1, j2 = rule1:lhs():disjoint_union(rule2:lhs())
print(j1:cod(), j1, j2)

print()
print('Overlappings')
for j1, j2 in rule1:lhs():overlappings_with(rule2:lhs()) do
  print(j1:cod(), j1, j2)
end

print()
print('Monic Overlappings')
for j1, j2 in rule1:lhs():overlappings_with(rule2:lhs(), 'monic') do
  print(j1:cod(), j1, j2)
end