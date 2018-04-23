function print_types()
  print('Types: ')
  GrLang.print_types()
  print()
end

print_types()
GrLang.reset_types()
print_types()

GrLang.add_node_type('N')
GrLang.add_edge_type('E', 'N', 'N')
print_types()

function foo()
  local a = Graph[[]]
  print(pcall(GrLang.reset_types))
end

foo()
GrLang.reset_types()
print_types()

GrLang.add_node_type('N')
GrLang.add_edge_type('E', 'N', 'N')
print_types()

b = Graph[[]]
print(pcall(GrLang.reset_types))
b = nil
GrLang.reset_types()
print_types()