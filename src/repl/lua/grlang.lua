require 'help'
local docstring = help.docstring

function catch_haskell(result, error_msg)
  if result == '_HASKELLERR' then
    error(error_msg)
  else
    return result
  end
end


--[[ GrLang class ]]

GrLang = docstring{[==[
Superclass for all GrLang values.
All instances of this class are wrappers of Haskell values.
]==],
  functions = {
    'add_node_type',
    'add_edge_type',
    'node_types',
    'edge_types',
    'print_types'
  },
  methods = {
    'to_dot',
    'view'
  }
} .. {__index = {}}

local function newGrLang(class, idx)
  local instance = { index = idx }
  setmetatable(instance, class)
  return instance
end

GrLang.node_types = docstring[==[
List the names of all registered node types.
]==] .. function ()
  return catch_haskell(GrLang.native.getNodeTypes())
end

GrLang.edge_types = docstring[==[
List the names of all registered edge types.
]==] .. function ()
  return catch_haskell(GrLang.native.getEdgeTypes())
end

GrLang.print_types = docstring[==[
Print the names of all registered node and edge types.
]==] .. function ()
  for i,t in ipairs(GrLang.node_types()) do
    print('node type ' .. tostring(t))
  end
  for i,t in ipairs(GrLang.edge_types()) do
    print('edge type ' .. tostring(t))
  end
end

GrLang.add_node_type = docstring[==[
Given a name, register a new node type.
]==] .. function (name)
  return catch_haskell(GrLang.native.addNodeType(name))
end

GrLang.add_edge_type = docstring[==[
Given a name and the node types for source and target, register a new edge type.
]==] .. function (name, srcName, tgtName)
  return catch_haskell(GrLang.native.addEdgeType(name, srcName, tgtName))
end

function GrLang.__tostring(value)
  return catch_haskell(GrLang.native.toString(value.index))
end

function GrLang.__eq(value1, value2)
  return value1.index == value2.index or GrLang.native.equals(value1.index, value2.index)
end

function GrLang.__gc(value)
  return catch_haskell(GrLang.native.deallocate(value.index))
end

GrLang.__index.to_dot = docstring[==[
Write the value in the dot format for graph drawing.
Optionally receives a name for the given value.
]==] .. function (value, name)
  return catch_haskell(GrLang.native.toDot(value.index, name or ''))
end

GrLang.__index.view = docstring[==[
Draw the given graph using xdot.
]==] .. function (graph)
  local file_name = '/tmp/verigraph-dot' .. os.date()
  local file = io.open(file_name, 'w')
  file:write(graph:to_dot(''))
  file:close()
  os.execute('xdot \"' .. file_name .. '\"')
end


--[[ Creating subclasses ]]

local function subclass_of_GrLang(factory)
  local class = { __index = {}, __tostring = GrLang.__tostring, __eq = GrLang.__eq, __gc = GrLang.__gc }

  setmetatable(class.__index, { __index = GrLang.__index })

  setmetatable(class, {
    __call = factory or function (cls, str)
      local idx = catch_haskell(cls.native.parse(str))
      return newGrLang(cls, idx)
    end
  })

  return class
end

--[[ Graph class ]]

Graph = docstring{[==[
Class for GrLang graphs, subclass of GrLang.

Instances can be constructed as follows:
    Graph [[
      n1 n2 : NodeType
      n1 -:EdgeType1, f g:EdgeType2-> n2
    ]]
]==],
  methods = { 'identity' }
} .. subclass_of_GrLang()

Graph.__index.identity = docstring "Returns the identity morphism"
  .. function(graph)
    if not graph.__identity then
      local idx = catch_haskell(Graph.native.identity(graph.index))
      graph.__identity = newMorphism(Morphism, idx, graph, graph)
    end
    return graph.__identity
  end

--[[ Morphism class ]]

Morphism = docstring{[==[
Class for GrLang morphisms, subclass of GrLang.

Instances can be constructed as follows:
    Morphism(domain, codomain) [[
      n1 -> n2 -- indicate that element n1 is mapped to n2
      e1 e2 -> e3 -- you can map multiple elements at once
    ]]

Note that morphisms can be composed with the concatenation
operator, that is, `f .. g` returns the composite of `f` and `g`
when the domain of `f` is the same as the codomain of `g`.
]==],
  methods = {
    'domain', 'codomain', '..'
  }
} .. subclass_of_GrLang(
  function (cls, domain, codomain)
    return function (str)
      local idx = catch_haskell(
        cls.native.parse(domain.index, codomain.index, str))
      return newMorphism(cls, idx, domain, codomain)
    end
  end)

function newMorphism(cls, idx, domain, codomain)
  local result = newGrLang(cls, idx)
  result.__domain = domain
  result.__codomain = codomain
  return result
end

Morphism.__index.domain = docstring "Returns the domain graph."
  .. function(morphism) return morphism.__domain end

Morphism.__index.codomain = docstring "Returns the codomain graph."
  .. function(morphism) return morphism.__codomain end

Morphism.__concat = function(m1, m2)
  if m1.__domain ~= m2.__codomain then
    error("Given morphisms are not composable.")
  else
    local idx = catch_haskell(Morphism.native.compose(m1.index, m2.index))
    return newMorphism(Morphism, idx, m2.__domain, m1.__codomain)
  end
end

--[[ Rule class ]]

Rule = docstring{[==[
Class for GrLang rules, subclass of GrLang.

Instances can be constructed as follows:
    Rule [[
      match {
        n1 n2 : NodeType
        n1 -:EdgeType1, f g:EdgeType2-> n2
      }
      forbid n2 -:EdgeType1-> n1
      delete f
      clone n2 as n3
      -- also create and join
    ]]
]==],
  methods = {
    'find_matches'
  }
} .. subclass_of_GrLang()


Rule.__index.find_matches = docstring[==[
Find all applicable matches into the given graph.
]==] .. function(rule, graph)
  if type(graph) ~= 'table' or getmetatable(graph) ~= Graph then
    error('Rule.find_matches must be called with a graph.')
  end
  return catch_haskell(Rule.native.findMatches(rule.index, graph.index))
end

--[[ Requiring GrLang files ]]

local function loader(modname, path)
  catch_haskell(GrLang.native.compileFile(path))
  return
end

-- Memoize the search path using a weak table, so no memory leak
local search_path = {}
setmetatable(search_path, {__mode = 'k'})

local function get_search_path()
  if not search_path[package.path] then
    local path = ''
    
    local i
    for i in package.path:gmatch('[^;]*;') do
      if i:match('%?%.lua') then
        path = path .. i:gsub('%?%.lua', '?.tg') .. i:gsub('%?%.lua', '?.grl')
      end
    end

    i = package.path:match('[^;]*$')
    if i:match('%?%.lua') then
      path = path .. i:gsub('%?%.lua', '?.tg') .. ';' .. i:gsub('%?%.lua', '?.grl')
    end

    search_path[package.path] = path
  end
  return search_path[package.path]
end

package.searchers[#package.searchers + 1] =
  function(modname)
    path = package.searchpath(modname, get_search_path())
    if path then
      return loader, path
    end
  end
