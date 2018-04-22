rules, names = GrLang.readGGX('grammars/Elevator/elevator.ggx')

print('Types:')
GrLang.print_types()
print()

print('Rules:')
print()
for _, name in ipairs(names) do
  print(name, rules[name])
end